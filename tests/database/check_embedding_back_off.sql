-- Checks the cron job's back-off (sql/8_search.sql, private.embed_questions()): when it calls the
-- embed-questions Edge Function, and when it holds off because its calls fill nothing. Run it whole in a
-- project's SQL Editor after sql/8_search.sql, on a project whose questions are embedded. It changes nothing:
-- it sets up each case, runs the cron job's function, and ends with an error that rolls all of it back,
-- the calls it queued included. The error's message is the result, case by case: whether the function called
-- (called) and how many calls in a row it then counts as having filled nothing (misses). Every case says
-- "ok": true when it went as it should.

do $$
declare
  q       bigint;
  other   bigint;
  queued  integer;
  results jsonb := '{}';
  checked jsonb;
begin
  select component_id into q from public.question_embedding order by component_id limit 1;
  select component_id into other from public.question_embedding order by component_id desc limit 1;
  if q is null or q = other then
    raise exception 'This needs at least two embedded questions.';
  end if;
  delete from private.question_embedding_stale;                     -- no texts to make again in between
  update public.question_embedding set embedding = null, embedded_at = null where component_id = q; -- one to fill
  update public.question_embedding set embedded_at = null;          -- no progress, unless a case makes some

  -- Each case: set the state the last call left, run the cron job's function, and compare with what it should do.
  -- 1. the first call ever: calls
  update private.question_embedding_calls set last_called_at = null, calls_without_progress = 0;
  select count(*) into queued from net.http_request_queue;
  perform private.embed_questions();
  checked := jsonb_build_object('called', (select count(*) from net.http_request_queue) > queued,
                                'misses', (select calls_without_progress from private.question_embedding_calls));
  results := results || jsonb_build_object('1 first call', checked || jsonb_build_object('ok', checked = '{"called": true, "misses": 0}'));

  -- 2. the 5th call in a row to fill nothing: still calls
  update private.question_embedding_calls set last_called_at = now() - interval '15 seconds', calls_without_progress = 4;
  select count(*) into queued from net.http_request_queue;
  perform private.embed_questions();
  checked := jsonb_build_object('called', (select count(*) from net.http_request_queue) > queued,
                                'misses', (select calls_without_progress from private.question_embedding_calls));
  results := results || jsonb_build_object('2 fifth miss', checked || jsonb_build_object('ok', checked = '{"called": true, "misses": 5}'));

  -- 3. the 6th, 15 seconds after the last call: holds off, and doesn't count the same call twice
  update private.question_embedding_calls set last_called_at = now() - interval '15 seconds', calls_without_progress = 5;
  select count(*) into queued from net.http_request_queue;
  perform private.embed_questions();
  checked := jsonb_build_object('called', (select count(*) from net.http_request_queue) > queued,
                                'misses', (select calls_without_progress from private.question_embedding_calls));
  results := results || jsonb_build_object('3 sixth miss holds off', checked || jsonb_build_object('ok', checked = '{"called": false, "misses": 5}'));

  -- 4. the same, 6 minutes after the last call: tries again
  update private.question_embedding_calls set last_called_at = now() - interval '6 minutes', calls_without_progress = 5;
  select count(*) into queued from net.http_request_queue;
  perform private.embed_questions();
  checked := jsonb_build_object('called', (select count(*) from net.http_request_queue) > queued,
                                'misses', (select calls_without_progress from private.question_embedding_calls));
  results := results || jsonb_build_object('4 after 5 minutes', checked || jsonb_build_object('ok', checked = '{"called": true, "misses": 6}'));

  -- 5. 10 misses, but the last call filled one: calls, and starts counting again
  update private.question_embedding_calls set last_called_at = now() - interval '15 seconds', calls_without_progress = 10;
  update public.question_embedding set embedded_at = now() where component_id = other;
  select count(*) into queued from net.http_request_queue;
  perform private.embed_questions();
  checked := jsonb_build_object('called', (select count(*) from net.http_request_queue) > queued,
                                'misses', (select calls_without_progress from private.question_embedding_calls));
  results := results || jsonb_build_object('5 progress resets', checked || jsonb_build_object('ok', checked = '{"called": true, "misses": 0}'));

  -- 6. nothing left to fill: no call, and the misses are forgotten
  update public.question_embedding
  set embedding = (select embedding from public.question_embedding where component_id = other)
  where component_id = q;
  update private.question_embedding_calls set last_called_at = now() - interval '15 seconds', calls_without_progress = 7;
  select count(*) into queued from net.http_request_queue;
  perform private.embed_questions();
  checked := jsonb_build_object('called', (select count(*) from net.http_request_queue) > queued,
                                'misses', (select calls_without_progress from private.question_embedding_calls));
  results := results || jsonb_build_object('6 nothing to fill', checked || jsonb_build_object('ok', checked = '{"called": false, "misses": 0}'));

  raise exception 'Rolled back. %', jsonb_pretty(results);
end;
$$;

-- =====================================================================================================
-- MATR metadata database: one person at a time works on a draft (public.draft.editing_by, editing_at)
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor: on the
-- test project first (the tests need it), then on the real one.
--
-- Two people could open the same draft on the Edit Extraction page, and whoever saved last overwrote the
-- other. Now opening a draft there claims it: the draft says who is working on it and when their page
-- last said they still are. The page renews the claim every minute while they are active, and lets it go
-- when they leave the page, close it, or have done nothing for 15 minutes. A claim that hasn't been
-- renewed for 3 minutes (a closed laptop, a lost connection) has lapsed, so nobody is kept out by a page
-- that is gone. Anyone can take a draft over from whoever has it.
--
-- ORDER: run this FIRST, then deploy the site's new code. The new code reads draft.editing_by and calls
-- claim_draft() and release_draft(), so it breaks every page that lists drafts until this has run. The
-- old code never mentions them, so it works before and after this runs.
--
-- After it has run:
--   * docs/sql/sql_scripts.sql already builds the same (section 1's create table public.draft, sections
--     2, 3, 4c, 5d and 6, and the build checks).
--   * docs/database_tables.md lists the columns; regenerate it with tests/database/describe_schema.R.
--
-- Nothing is lost. It is one transaction: if any step fails, nothing changes.
-- =====================================================================================================

begin;

-- ------ 1. Who is working on a draft, and when their page last said so ----------------------------------
alter table public.draft
  add column editing_by bigint,
  add column editing_at timestamptz,
  add constraint draft_editing_says_when check ((editing_by is null) = (editing_at is null)),
  add constraint draft_editing_by_fkey foreign key (editing_by) references public.staff (staff_id);

create index draft_editing_by_idx on public.draft (editing_by);

comment on column public.draft.editing_by is
  'Who has it open on the Edit Extraction page, if anyone (claim_draft()). Empty when nobody does.';
comment on column public.draft.editing_at is
  'When their page last renewed the claim. One not renewed for a few minutes has lapsed, and anyone may claim the draft.';

-- ------ 2. Claiming and letting go --------------------------------------------------------------------
create function public.claim_draft(
  p_draft_id      bigint,
  p_staff_id      bigint,
  p_take_over     boolean default false,
  p_stale_seconds integer default 180
)
returns jsonb
language plpgsql
set search_path = public, pg_temp
as $$
declare
  d public.draft%rowtype;
begin
  -- the lock makes two claims of the same draft take turns, so only one of them gets it
  select * into d from public.draft where draft_id = p_draft_id for update;
  if not found then
    raise exception 'That is no longer in the database.';
  end if;
  perform 1 from public.staff where staff_id = p_staff_id and active;
  if not found then
    raise exception 'Only active staff can change a draft.';
  end if;
  if d.status in ('Applied', 'Withdrawn') then
    raise exception 'That is no longer being prepared, so it can''t be worked on.';
  end if;

  if p_take_over or d.editing_by is null or d.editing_by = p_staff_id
     or d.editing_at < now() - make_interval(secs => p_stale_seconds) then
    update public.draft set editing_by = p_staff_id, editing_at = now()
    where draft_id = p_draft_id
    returning editing_by, editing_at into d.editing_by, d.editing_at;
    return jsonb_build_object(
      'claimed', true, 'editing_by', d.editing_by, 'editing_at', d.editing_at, 'revision', d.revision,
      'editing_name', (select name from public.staff where staff_id = d.editing_by)
    );
  end if;
  return jsonb_build_object(
    'claimed', false, 'editing_by', d.editing_by, 'editing_at', d.editing_at, 'revision', d.revision,
    'editing_name', (select name from public.staff where staff_id = d.editing_by)
  );
end;
$$;

create function public.release_draft(p_draft_id bigint, p_staff_id bigint)
returns boolean
language plpgsql
set search_path = public, pg_temp
as $$
begin
  -- an applied draft can't be changed (draft_guard), and nobody can claim one, so there is nothing to let go
  update public.draft set editing_by = null, editing_at = null
  where draft_id = p_draft_id and editing_by = p_staff_id and status <> 'Applied';
  return found;
end;
$$;

revoke all on function public.claim_draft(bigint, bigint, boolean, integer) from public, anon, authenticated;
revoke all on function public.release_draft(bigint, bigint) from public, anon, authenticated;
grant execute on function public.claim_draft(bigint, bigint, boolean, integer) to service_role;
grant execute on function public.release_draft(bigint, bigint)                 to service_role;

comment on function public.claim_draft(bigint, bigint, boolean, integer) is
  'Claims a draft for one staff member to work on, or renews their claim. Refused (claimed false) while someone else''s claim was renewed within p_stale_seconds, unless p_take_over. Never raises the revision or moves saved_at. Returns {claimed, editing_by, editing_name, editing_at, revision}.';
comment on function public.release_draft(bigint, bigint) is
  'Lets go of a staff member''s claim on a draft, if they still have it. Returns whether they did.';

-- ------ 3. Check -------------------------------------------------------------------------------------
do $$
declare
  ana   bigint;
  ben   bigint;
  d     bigint;
  claim jsonb;
begin
  insert into public.staff (name, email) values ('Claim Ana', 'ana@claim.test') returning staff_id into ana;
  insert into public.staff (name, email) values ('Claim Ben', 'ben@claim.test') returning staff_id into ben;
  d := public.create_draft('Edit', ana, '{"summary": "Claim test"}'::jsonb,
                           '{"edit_kind": "Add", "form": {}, "items": []}'::jsonb)::bigint;

  claim := public.claim_draft(d, ana);
  assert (claim ->> 'claimed')::boolean and (claim ->> 'editing_by')::bigint = ana, 'the first claim gets it';
  assert claim ->> 'editing_name' = 'Claim Ana', 'and says who has it';
  assert not (public.claim_draft(d, ben) ->> 'claimed')::boolean, 'a second person is refused';
  assert (public.claim_draft(d, ana) ->> 'claimed')::boolean, 'the first person renews it';
  update public.draft set editing_at = now() - interval '10 minutes' where draft_id = d;
  assert (public.claim_draft(d, ben) ->> 'claimed')::boolean, 'a lapsed claim can be taken';
  assert (public.claim_draft(d, ana, true) ->> 'claimed')::boolean, 'and anyone can take over';
  assert not public.release_draft(d, ben), 'only who has it lets it go';
  assert public.release_draft(d, ana), 'who has it lets it go';
  assert (select editing_by is null and editing_at is null and revision = 1 from public.draft where draft_id = d),
    'nobody has it, and claiming never raised the revision';

  delete from public.draft where draft_id = d;
  delete from public.staff where staff_id in (ana, ben);
end;
$$;

commit;

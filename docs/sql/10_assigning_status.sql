-- =====================================================================================================
-- MATR metadata database: applying the Variable Names and Coding is a reading step of its own
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor: on the
-- test project first (the tests need it), then on the real one. Run 9_integrity_rules.sql first.
--
-- A draft with a file of Variable Names and Coding (extracted_draft.supplement) is read, checked, and then
-- has its MATR IDs and codes applied, in a request of its own. While that happens the reading job says so:
-- ai_extracted_draft.reading_status is 'assigning', between 'received' and 'ready'.
--
-- ORDER: run this before the site's new code reads a draft with such a file. Until it has run, the job's
-- 'assigning' is refused and that draft's reading fails.
--
-- Nothing is lost and no row changes. It is one transaction: if any step fails, nothing changes.
-- docs/sql/sql_scripts.sql already builds the same.
-- =====================================================================================================

begin;

alter table public.ai_extracted_draft
  drop constraint ai_extracted_draft_reading_status_check,
  add constraint ai_extracted_draft_reading_status_check
    check (reading_status in ('queued', 'sent', 'received', 'assigning', 'ready', 'failed'));

-- ------ Check ----------------------------------------------------------------------------------------
do $$
begin
  assert pg_get_constraintdef((select oid from pg_constraint where conname = 'ai_extracted_draft_reading_status_check'))
    like '%assigning%', 'a reading can be applying its Variable Names and Coding';
end;
$$;

commit;

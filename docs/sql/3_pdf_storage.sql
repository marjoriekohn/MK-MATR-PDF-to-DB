-- =====================================================================================================
-- MATR metadata database: keep the extractions' PDFs in Storage (bucket extraction-pdfs)
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor: on the
-- test project first (the tests need it), then on the real one.
--
-- The site kept each uploaded PDF only on the disk of the server it runs on. A hosted site loses that
-- disk when it restarts or is deployed again, and its servers don't share one, so a draft opened later
-- often had no PDF to show (Open PDF was missing) or to read again. Now each upload is also kept in a
-- private Storage bucket, named by its SHA-256, and a server that hasn't got a draft's PDF fetches it
-- from there (R/model_storage.R).
--
-- The bucket is private: only the site's server reaches it, with the secret key, which Row Level
-- Security doesn't apply to. No policy is added on storage.objects, so the publishable key and
-- signed-in staff can't list, read or write it. Files up to 50 MB, as the site accepts
-- (shiny.maxRequestSize in global.R), and only PDFs.
--
-- ORDER: run this FIRST, then deploy the site's new code. The new code keeps every upload in the
-- bucket, and refuses the upload (saying this script hasn't run) while there is no bucket. The old code
-- never uses it.
--
-- PDFs uploaded before this ran are only on whichever server received them, if it still has them. A
-- draft whose PDF is gone has none to show; Replace PDF (only offered while a draft is being read)
-- starts the reading again.
--
-- docs/sql/sql_scripts.sql doesn't touch Storage (see its header), so it isn't changed. It is one
-- transaction: if any step fails, nothing changes.
-- =====================================================================================================

begin;

insert into storage.buckets (id, name, public, file_size_limit, allowed_mime_types)
values ('extraction-pdfs', 'extraction-pdfs', false, 50 * 1024 * 1024, array['application/pdf'])
on conflict (id) do nothing;

-- public.pdf_file still lists each file once, with its pages and size; the file itself is now here.
comment on table public.pdf_file is
  'A PDF uploaded for an extraction, once per file (by its SHA-256): its pages and size. The file itself is in Storage, bucket extraction-pdfs, as <hash>.pdf (docs/migrations/20261021_pdf_storage.sql).';

-- ------ Check ----------------------------------------------------------------------------------------
do $$
begin
  assert exists (select 1 from storage.buckets where id = 'extraction-pdfs' and not public), 'the bucket is there, and private';
  assert not exists (
    select 1 from pg_policies
    where schemaname = 'storage' and tablename = 'objects' and qual ilike '%extraction-pdfs%'
  ), 'no policy lets anyone but the site''s server in';
end;
$$;

commit;

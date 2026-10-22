-- =====================================================================================================
-- MATR metadata database: the respondent's subject is called "Respondent", not "Self"
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor: on the
-- test project first, then on the real one.
--
-- Who a variable is about is a row of public.subject. The row for the respondent is renamed from "Self"
-- to "Respondent", which is what the site, the public dashboard and Make an edit then show. Variables
-- point at the row by its id, so none of them change.
--
-- ORDER: either way round. The site recognises both names for the respondent, so it works before and
-- after this runs. If the subject list already has a "Respondent" row, nothing is renamed: merge the two
-- by hand on Make an edit (kind Subject) instead.
--
-- After it has run, docs/sql/sql_scripts.sql already builds the same (section 7's starting data).
-- It is one transaction: if it fails, nothing changes.
-- =====================================================================================================

begin;

update public.subject
   set name = 'Respondent'
 where lower(btrim(name)) = 'self'
   and not exists (select 1 from public.subject where lower(btrim(name)) = 'respondent');

commit;

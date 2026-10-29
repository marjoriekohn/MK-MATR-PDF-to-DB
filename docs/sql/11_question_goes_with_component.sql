-- =====================================================================================================
-- MATR metadata database: a question row goes with its component
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor: on the
-- test project first (the tests need it), then on the real one. Run 9_integrity_rules.sql first.
--
-- Since 9_integrity_rules.sql (M2), a component is a Question exactly when it has a question row, checked
-- as each transaction ends. So the two can only be removed in one transaction. The site's own deletes
-- already are (apply_draft_items removes the question row, then the component), but a component could not
-- be removed by itself: its question row pointed at it with no cascade. Now removing a component removes its
-- question row too, as removing a variable removes its closed_ended or open_ended row. What points at the
-- question (its places in questionnaires, scale items, categories) still has to go first: those foreign
-- keys don't cascade, so nothing printed is lost by accident.
--
-- Nothing is lost and no row changes. It is one transaction: if any step fails, nothing changes.
-- docs/sql/sql_scripts.sql already builds the same.
-- =====================================================================================================

begin;

alter table public.question
  drop constraint question_component_id_fkey,
  add constraint question_component_id_fkey
    foreign key (component_id) references public.component (component_id) on delete cascade;

-- ------ Check ----------------------------------------------------------------------------------------
do $$
begin
  assert (select confdeltype from pg_constraint where conname = 'question_component_id_fkey') = 'c',
    'removing a component removes its question row';
end;
$$;

commit;

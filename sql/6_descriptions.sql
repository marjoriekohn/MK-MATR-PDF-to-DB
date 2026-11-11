-- =====================================================================================================
-- MATR metadata database, script 6 of 8: DESCRIPTIONS
-- =====================================================================================================
-- Run after 5_permissions.sql; can be run again at any time. What the Supabase table editor shows beside
-- each table and column.

begin;

comment on table public.registry                is 'Outermost container; every study, questionnaire and variable rolls up to one.';
comment on table public.principal_investigator  is 'A researcher who leads a study or is its contact.';
comment on table public.study                   is 'One research project run under a registry.';
comment on column public.study.status           is 'Active: the study is ongoing. Ended: all data collections are finished.';
comment on table public.respondent_type         is 'The population a questionnaire was written for, bounded by age.';
comment on table public.questionnaire           is 'One form (instrument): what stays true across every wave it was given in.';
comment on column public.questionnaire.participant_count is 'Across all its waves, for when the waves'' own counts aren''t known.';
comment on table public.wave                    is 'One round of data collection for a questionnaire, with the files its questions were extracted from.';
comment on column public.wave.pdf_sha256        is 'The questionnaire PDF the wave was extracted from (pdf_file); the file is in Storage as <sha256>.pdf.';
comment on column public.wave.supplement_text   is 'The file of MATR IDs and codes, when it was a CSV or text file; a PDF is supplement_sha256 instead.';
comment on table public.pdf_file                is 'A PDF uploaded to the site, once per file (by its SHA-256): its pages and size. The file itself is in Storage, bucket extraction-pdfs, as <hash>.pdf. The dashboard sees only a wave''s.';
comment on table public.component               is 'One piece of printed material, held once for the whole registry: one for each kind and wording, however many times it is printed. The one Group is shared by every group.';
comment on column public.component.kind         is 'Section, Group, Grid (hold other blocks); Question (has variables); Instruction, Introduction, Definition, Note, Interviewer Script (remarks).';
comment on table public.placement               is 'One place where a questionnaire prints a component: what holds it, its order, and for a question the earlier question it depends on. Variables and wave numbers attach here.';
comment on column public.placement.depends_on_placement_id is 'For a Question: the earlier Question it only makes sense after ("When did you start?" after "Do you smoke?"). Empty for a question that stands on its own.';
comment on table public.wave_question           is 'That a wave asked a question: its place among the wave''s questions (question_number) and the number printed beside it (printed_number).';
comment on table public.variable                is 'One place an answer is recorded: one column in the response data, named by its MATR ID.';
comment on column public.variable.variable_type is 'open_ended: the respondent writes it (answer_data_type says what). closed_ended: chosen from its response options.';
comment on column public.variable.label         is 'What tells a question''s variables apart: "Twin A", "child a", a check box''s words.';
comment on table public.answer_option           is 'One answer choice, stored once for the whole registry, in its normal form (lower case, straight quotes, no full stop).';
comment on table public.response_option         is 'One answer option as it was printed for one closed-ended variable, with the code it was recorded as.';
comment on table public.tag                     is 'A word the dashboard filters by, put on questions, variables or response options. kind: subject (who it is about), time frame (when), unit (what a number is in).';
comment on table public.placement_tag           is 'The tags on a question (its placement).';
comment on table public.variable_tag            is 'The tags on a variable: who its answer is about, when, and what unit its number is in.';
comment on table public.response_option_tag     is 'The tags on one response option of one variable (a unit or time frame printed by the choice).';
comment on table public.measurement_scale       is 'A published, validated set of questions scored together.';
comment on table public.question_scale          is 'A question''s membership and item number in a measurement scale.';
comment on table public.staff                   is 'MATR employee who extracts and edits metadata. Never deleted; marked inactive.';
comment on table public.extraction              is 'One extraction of a questionnaire: typed by hand (Manual) or read from a PDF by an AI (AI-Powered). A Draft while staff work on it (the questionnaire as JSON); once Pushed, the record of what the AI read and what staff changed (the AI Monitor page).';
comment on column public.extraction.info        is 'Where it goes: {"registry_id", "study_id", "questionnaire_id", "wave_id"}, each the ID as text, or "" while not picked.';
comment on column public.extraction.questionnaire is 'The questionnaire as staff last saved it on Edit Extraction (the page''s JSON); empty until they first save.';
comment on column public.extraction.ai_original is 'The questionnaire JSON the AI returned, written once by the reading job and never changed.';
comment on column public.extraction.review_edits is 'How the saved copy differs from what the AI returned, worked out by the site at each save.';
comment on column public.extraction.revision    is 'Goes up at each save by staff, so a save made from a stale copy is refused.';
comment on table public.history                 is 'Everything staff changed in the catalog, one row per change: an extraction pushed, a wave or questionnaire emptied, a row added, updated or deleted on Make an edit. Only ever added to.';
comment on column public.history.details        is 'What was done: an extraction''s counts, an edit''s fields with old and new values, an emptying''s counts.';

comment on function public.push_extraction(bigint, bigint, boolean) is 'Push an extraction into the catalog (its wave must ask nothing yet). With p_dry_run, only reports what it would do.';
comment on function public.apply_change(bigint, jsonb, text)        is 'Apply an edit made on Make an edit: a list of items (one field of one row each) in one transaction, written to the history.';
comment on function public.empty_wave(bigint, bigint)               is 'Remove what a wave asks (the wave stays), and what that leaves owned by nothing.';
comment on function public.empty_questionnaire(bigint, bigint)      is 'Remove everything a questionnaire prints and asks (it and its waves stay).';
comment on function public.empty_study(bigint, bigint)              is 'Empty every questionnaire of a study (the study, questionnaires and waves stay).';
comment on function public.empty_registry(bigint, bigint)           is 'Empty every questionnaire of a registry (the registry, studies, questionnaires and waves stay).';
comment on function public.upvote_questionnaire(bigint)             is 'One more upvote for a questionnaire, from the dashboard.';

commit;

-- =====================================================================================================
-- MATR metadata database, script 7 of 8: SEED
-- =====================================================================================================
-- Run after 6_descriptions.sql; can be run again at any time (a row already there is updated to this).
-- The starting rows: what the live database held on 2026-11-08, before this schema, apart from anything
-- extracted or drafted. IDs are kept, so links from outside the database stay good.
--   registries (2), investigators (2), studies (2), respondent types (2), questionnaires (4), waves (6),
--   subject tags (11, the subjects list), answer options (2), measurement scales (13), staff (7).
-- On a new project, add staff here (one row per person, with the email they sign in with) or in the table
-- editor afterwards.

begin;

insert into public.registry (registry_id, name, abbreviation, participant_count, description) values
  (311343933, 'Spit for Science', 'S4S', 15000, 'Spit for Science collects longitudinal phenotypic survey data as well as genotypic data from our registrants. We make these data available to qualified researchers in a de-identified format and take many measures to protect the privacy of our participants. The resulting phenotypic datasets contain an array of measures collected from over 15,000 participants at up to five timepoints during their college careers.'),
  (422188105, 'Mid-Atlantic Twin Registry', 'MATR', 60000, 'The MATR is a population-based twin registry housed in Virginia Commonwealth University (VCU)''s Office of the Vice President for Research & Innovation. With more than 50,000 individuals of varying ages and demographic backgrounds, the MATR is the largest twin registry in the United States. Most MATR registrants live in Virginia, North Carolina, and South Carolina, but twins from all over the US are part of the registry. MATR scientists collaborate with research teams from a variety of disciplines and institutions. We encourage researchers to contact us to find out how we can help you achieve your research goals utilizing our registry''s services.')
on conflict (registry_id) do update set name = excluded.name, abbreviation = excluded.abbreviation,
  participant_count = excluded.participant_count, description = excluded.description;

insert into public.principal_investigator (principal_investigator_id, name, email) values
  (477093093, 'Lenn Murrelle', 'lenn.murrelle@venebio.com'),
  (890369635, 'Lindon Eaves', 'ecpromwormle@vcu.edu')
on conflict (principal_investigator_id) do update set name = excluded.name, email = excluded.email;

insert into public.study (study_id, registry_id, principal_investigator_id, contact_principal_investigator_id, name, status, description, methods, acknowledgements) values
  (586158981, 422188105, 890369635, 890369635, 'Geno-Neurological Assessment', 'Ended', null, null, null),
  (686980661, 422188105, 477093093, 477093093, 'The Mid-Atlantic School Age Twin Study', 'Ended',
   'Parent-report questionnaire study of school-age twins (ages 6–18) enrolled through the Mid-Atlantic Twin Registry (MATR). One parent per twin household reported on each twin’s temperament and behavior, including rhythmicity (sleep/eating), mood, adaptability, prosociality, early difficulty, social competence, and ADHD-/ODD-/conduct-related behavioral dimensions (e.g., inattention, hyperactivity, impulsivity, aggression, oppositional defiance).',
   'Participants were recruited through the population-based Mid-Atlantic Twin Registry (MATR) from families with twins living in Virginia and North Carolina. Mailed questionnaires were sent to MATR families; one parent per household completed surveys about each twin. The analytic sample included 2,844 parental responses (≈88.5% pairwise completion). Zygosity was determined using parent-report similarity/confusion items collected during MATR ascertainment (using established algorithms).',
   'This project was facilitated by the Mid-Atlantic Twin Registry (MATR) administered by the Cohort and Registry Administration (CARA) Core, a Virginia Commonwealth University shared resource. We thank the MATR twins and their families for participating.')
on conflict (study_id) do update set registry_id = excluded.registry_id, principal_investigator_id = excluded.principal_investigator_id,
  contact_principal_investigator_id = excluded.contact_principal_investigator_id, name = excluded.name, status = excluded.status,
  description = excluded.description, methods = excluded.methods, acknowledgements = excluded.acknowledgements;

insert into public.respondent_type (respondent_type_id, name, minimum_age, maximum_age, description) values
  (950205524, 'Twins', 6, 18, null),
  (967262857, 'Parents', null, null, null)
on conflict (respondent_type_id) do update set name = excluded.name, minimum_age = excluded.minimum_age,
  maximum_age = excluded.maximum_age, description = excluded.description;

insert into public.questionnaire (questionnaire_id, study_id, respondent_type_id, name, abbreviation, description) values
  (166381609, 686980661, 950205524, 'Kid Questionnaire', 'KQ', null),
  (180266975, 686980661, 950205524, 'Geno-Neurological Assessment', 'GNA', null),
  (354507358, 586158981, 967262857, 'Geno-Neurological Assessment', 'GNA', null),
  (568119208, 686980661, 967262857, 'Mother Questionnaire', 'MQ', null)
on conflict (questionnaire_id) do update set study_id = excluded.study_id, respondent_type_id = excluded.respondent_type_id,
  name = excluded.name, abbreviation = excluded.abbreviation, description = excluded.description;

insert into public.wave (wave_id, questionnaire_id, name, respondent_count, administered_date) values
  (240501690, 354507358, 'Pilot', null, null),
  (365494457, 568119208, 'Pilot', null, null),
  (478860515, 180266975, 'Pilot', null, null),
  (782384927, 166381609, 'Pilot', null, null),
  (827026564, 166381609, 'Wave 1', null, null),
  (978714364, 568119208, 'Wave 1', null, null)
on conflict (wave_id) do update set questionnaire_id = excluded.questionnaire_id, name = excluded.name,
  respondent_count = excluded.respondent_count, administered_date = excluded.administered_date;

-- the tags: subjects (the relation to the respondent, in lower case), time frames and units, written one way
-- (R/service_draft.R: normal_subject(), normal_time_frame(), normal_unit())
insert into public.tag (tag_id, kind, name) values
  (923939683, 'subject', 'brother'),
  (893244383, 'subject', 'children'),
  (175894758, 'subject', 'daughter'),
  (968416887, 'subject', 'family'),
  (903010384, 'subject', 'father'),
  (552931313, 'subject', 'mother'),
  (684985819, 'subject', 'respondent'),
  (742386590, 'subject', 'siblings'),
  (835895131, 'subject', 'sister'),
  (107894001, 'subject', 'son'),
  (868145262, 'subject', 'twin'),
  (642902192, 'time frame', 'lifetime'),
  (899022208, 'time frame', 'past 30 days'),
  (409015614, 'time frame', 'past six months'),
  (433114348, 'time frame', 'past two weeks'),
  (149500717, 'time frame', 'past week'),
  (267860203, 'time frame', 'past year'),
  (225091318, 'unit', 'pounds')
on conflict (tag_id) do update set kind = excluded.kind, name = excluded.name;

insert into public.answer_option (answer_option_id, standardized_label) values
  (306030140, 'yes'),
  (432991684, 'no')
on conflict (answer_option_id) do update set standardized_label = excluded.standardized_label;

insert into public.measurement_scale (scale_id, name, abbreviation, version, description, citation) values
  (171579679, 'Fagerstrom Test for Nicotine Dependence', 'FTND', null, 'The Fagerström Test for Cigarette Dependence (Heatherton et al., 1991) is a questionnaire that measures of dependence on nicotine. The questionnaire utilizes multiple-choice and dichotomus-choice response formats. The 6-item Fagerström Test for Cigarette Dependence is a revision of the 8-item Fagerstrom Tolerance Questionnaire (FTQ; Fagerstrom, 1978). The Fagerström Test for Cigarette Dependence consists of 6 of the original items from the FTQ. Two items from the FTQ were eliminated. Scoring was revised for the items that measure time to the first cigarette of the day and cigarettes smoked per day. An iterated principal axes factor analysis revealed that all of the Fagerström Test for Cigarette Dependence items loaded on a single factor. Based on a sample of smokers, it was determined that the the Fagerström Test for Cigarette Dependence has acceptable levels of internal consistency.', 'https://doi.org/10.1037/t03773-000'),
  (238168103, 'Chalder Fatigue Scale', 'CFQ-11', null, null, null),
  (248023741, 'Global Physical Activity Questionnaire', 'GPAQ', null, null, null),
  (306024048, 'International Physical Activity Questionnaire', 'IPAQ', null, null, null),
  (311945903, 'Alcohol Use Disorders Identification Test Consumption', 'AUDIT-C', null, null, null),
  (359710177, 'Eating Disorder Examination-Adolescent', 'EDE-A', null, 'The Eating Disorder Examination (EDE; Z. Cooper & C. Fairburn, 1987) is a semi-structured interview that is designed to assess the full range of the specific psychopathology of eating disorders, including patients'' concerns about their shape and weight. It was developed to overcome the limitations inherent in the use of self-report instruments to measure this psychopathology. Item selection included a literature review and unstructured interviews with patients with anorexia nervosa and bulimia nervosa. Each item was defined, specific questions were created, and a standard coding scheme was devised with anchor points appropriate to each individual item. A pilot interview was conducted with patients with anorexia nervosa, patients with bulimia nervosa, age-matched controls known to be concerned about their shape or weight, and aged-matched controls with no such concerns. This led to the final edition of the EDE, a 62-item semi-structured clinical interview. All questions refer exclusively to the previous 4 weeks. The inter-rater reliability of each EDE item was calculated using 3 raters and 12 subjects, and was found to be uniformly high.', 'https://doi.org/10.1037/t03975-000'),
  (496208818, 'MOS 36-item Short-Form Health Survey', 'SF-36', null, 'The MOS 36-item Short-form Health Survey (SF-36; Ware & Sherbourne, 1992) was developed to survey health status in the Medical Outcomes Study. The SF-36 was designed for use in clinical practice and research, health policy evaluations, and general population surveys. The SF-36 includes one multi-item scale that assesses eight health concepts: 1) limitations in physical activities because of health problems; 2) limitations in social activities because of physical or emotional problems; 3) limitations in usual role activities because of physical health problems; 4) bodily pain; 5) general mental health (psychological distress and well-being); 6) limitations in usual role activities because of emotional problems; 7) vitality (energy and fatigue); and 8) general health perceptions. The survey was constructed for self-administration by persons 14 years of age and older, and for administration by a trained interviewer in person or by telephone. The SF-36 items and scales were constructed for scoring using the Likert method of summated ratings. The measure can be administered in 5-10 minutes.', 'https://doi.org/10.1037/t06708-000'),
  (648198046, 'Patient Health Questionnaire-9', 'PHQ-9', null, 'The Patient Health Questionnaire (PHQ) is a self-administered version of the PRIME-MD diagnostic instrument for common mental disorders. It is used to make criteria-based diagnoses of depressive and other mental disorders commonly encountered in primary care. The Patient Health Questionnaire-9 (PHQ-9; Kroenke, Spitzer & Williams, 1999) is the 9-item depression module of the PHQ, which is half the length of many other depression measures. It consists of the actual 9 criteria upon which the diagnosis of DSM-IV depressive disorders is based. This feature distinguishes the PHQ-9 from other "2-step" depression measures for which, when scores are high, additional questions must be asked to establish DSM-IV depressive diagnoses. Major depression is diagnosed if 5 or more of the 9 depressive symptom criteria have been present at least "more than half the days" in the past 2 weeks, and 1 of the symptoms is depressed mood or anhedonia. Other depression is diagnosed if 2, 3, or 4 depressive symptoms have been present at least "more than half the days" in the past 2 weeks, and 1 of the symptoms is depressed mood or anhedonia. One of the 9 symptom criteria ("thoughts that you would be better off dead or of hurting yourself in some way") counts if present at all, regardless of duration. As with the original PRIME-MD, before making a final diagnosis, the clinician is expected to rule out physical causes of depression, normal bereavement, and history of a manic episode. The PHQ-9 also measures severity with an additional question at the end of the diagnostic portion of the scale. This measure was administered to 6,000 patients in 8 primary care clinics and 7 obstetrics-gynecology clinics. It was found that the PHQ-9 had excellent internal reliability and test-retest reliability. Evidence of criterion and construct validity was also established.', 'https://doi.org/10.1037/t06165-000'),
  (727019114, 'Cigarette Dependence Scale', 'CDS-12', null, 'The Cigarette Dependence Scale-12--Swedish Version (Rydell et al., 2016) is a Swedish of the original version of the Cigarette Dependence Scale-12 (CDS-12; Etter et al., 2003). The CDS-12--Swedish Version is an instrument suitable to assess tobacco dependence in non-clinical Swedish populations. The instrument’s items were translated into Swedish by one of the authors (blinded for peer review) and back-translated by a mother-tongue translator to check for fidelity to the original wording. A pilot study was performed on a convenience sample of individuals who were about the same age as the study population in the Swedish Sibling Health Cohort, with different trajectories of tobacco use (never, former, and current users). Following this pilot study, the questionnaire was revised and sent for professional editing. The Swedish CDS-12 uses the same score range as that of the original CDS-12, i.e. 12 to 60. The CDS-12--Swedish Version has an acceptable internal consistency. Factor analyses suggested a one-factor model. CDS-12 score and proportions classified as dependent were significantly higher among daily smokers compared to non-daily smokers, positively correlated with the number of cigarettes, and negatively with quit attempts as well as their duration.', 'https://doi.org/10.1037/t64156-000'),
  (949525605, 'Psychiatric Diagnostic Screening Questionnaire', 'PDSQ', null, null, null),
  (963505346, 'Alcohol Use Disorders Identification Test', 'AUDIT', null, 'The Alcohol Use Disorders Identification Test (AUDIT) is a simple ten-question test developed by the World Health Organization to determine if a person''s alcohol consumption may be harmful. The test was designed to be used internationally, and was validated in a study using patients from six countries. Questions 1-3 deal with alcohol consumption, 4-6 relate to alcohol dependence and 7-10 consider alcohol related problems. A score of 8 or more in men (7 in women) indicates a strong likelihood of hazardous or harmful alcohol consumption. A score of 20 or more is suggestive of alcohol dependence.', 'https://doi.org/10.1037/t01528-000'),
  (985184800, 'Youth Risk Behavior Survey', 'YRBS', null, 'The National Youth Risk Behavior Survey (YRBS) monitors health risk behaviors that contribute substantially to the leading causes of death, disability, and social problems among children and young adults in the US using a 3-stage cluster design to produce a representative sample of public and private high school students in grades 9 through 12.', 'https://doi.org/10.1016/j.ncl.2016.06.010'),
  (991285211, 'Eysenck Personality Questionnaire–Revised', 'EPQ-R', null, 'The Eysenck Personality Questionnaire-Revised (EPQ-R) measures three major dimensions of personality that account for most of the variance in personality. The EPQ-R is an excellent choice when you are administering a test battery and need a brief instrument representing the personality domain. This measure has proven useful for numerous applications in human resources, career counseling, clinical settings and research. Scales measured by the EPQ-R are Extraversion-Introversion, Neuroticism and Toughmindedness with the addition of a Lie Scale. The long form of the EPQ-R was developed in 1993, the short form in 2001.', 'https://doi.org/10.1037/t05461-000')
on conflict (scale_id) do update set name = excluded.name, abbreviation = excluded.abbreviation, version = excluded.version,
  description = excluded.description, citation = excluded.citation;

insert into public.staff (staff_id, name, email, active, role) values
  (273849056, 'Tim York', 'tpyork@vcu.edu', true, 'ADMIN'),
  (421809404, 'Emily Lilley', 'echlilley@vcu.edu', true, 'STAFF'),
  (459394115, 'Maggie Kohn', 'kohnmm@vcu.edu', true, 'STAFF'),
  (570408858, 'Aleeza Rahman', 'rahmanaa5@vcu.edu', true, 'STAFF'),
  (630125955, 'Maria Fatima Ali', 'alimf2@vcu.edu', true, 'STAFF'),
  (697838706, 'Danielle Coates', 'coatesdk@vcu.edu', true, 'STAFF'),
  (787552479, 'Elizabeth Prom-Wormley', 'ecpromwormle@vcu.edu', true, 'ADMIN')
on conflict (staff_id) do update set name = excluded.name, email = excluded.email, active = excluded.active, role = excluded.role;

commit;

-- the REST API learns the rows' tables (if 5_permissions.sql's notice hadn't reached it yet)
notify pgrst, 'reload schema';

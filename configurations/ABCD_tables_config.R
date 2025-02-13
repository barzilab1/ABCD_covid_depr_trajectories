
# Timepoints to remove from the dataset
tp_include = c(
  "baseline_year"
  , "1_year"
  , "2_year"
  , "3_year"
  , "4_year"
)


# tables included in each section
culture_environment_tables = c(
  "ce_y_fes"                  # Family Environment Scale [Youth]
  , "ce_y_pm"                 # Parental Monitoring [Youth]
  , "ce_y_psb"                # Prosocial Behavior [Youth],
  , "ce_y_wps"                # Wills Problem Solving Scale [Youth]
)


linked_external_tables = c(
  "led_l_adi"                # Area Deprivation Index (ADI) [Linked Dataset]
)

mental_health_tables = c(
  "mh_y_bpm"                # bpm
  , "mh_y_cbb"              # Cyberbullying
  , "mh_y_le"               # Life events
  , "mh_y_peq"              # Peer Experiences Questionnaire [Youth]
  , "mh_p_fhx"              # Family History [Parent]
  , "mh_y_poa"              # NIH Toolbox - Positive Affect Items [Youth]
)

ksad_tables = list(
  "suicidality" = NULL
 , "symptoms_youth" = NULL
 , "diagnoses_youth" = c(
     "Depression"
   , "Anxiety"
   , "Bipolar"
   , "Sleep"
 )
 , "diagnoses_parent" = c(
   "ADHD"
   , "Conduct"
   , "OCD"
   , "PTSD"
   , "ODD"
   , "Psychosis"
 )
)

physical_health_tables = c(
  "ph_y_mctq"                 # Munich Chronotype Questionnaire
  , "ph_y_pds"                # Pubertal Development Scale & Menstrual Cycle Survey History [Youth]

)


calculated_scores = c(
  "exposome"
  , "genetics"
)

covid_19_tables = c(
  "cvd_y_qtn"                # COVID-19 - Questionnaire [Youth]
)


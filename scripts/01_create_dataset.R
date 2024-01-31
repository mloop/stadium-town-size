library(tidyverse)
library(googlesheets4)
library(sjlabelled)

df <- read_sheet("https://docs.google.com/spreadsheets/d/1DBve4JLWO-tCRHOF8aTBYVG_T5ndqQikje37zUBLCMU/edit#gid=0")

df %>% Hmisc::describe()

df_clean <- df %>%
  select(-name_closest_ed, 
         -`...16`,
         -source_municipality_population,
         -municipality_population_year) %>%
  var_labels(
    school = "College/University",
    municipality_name = "Municipality",
    municipality_county = "County",
    municipality_state = "State",
    conference = "2023 Football Conference",
    football_stadium_capacity = "Football stadium capacity",
    municipality_population = "Population size",
    distance_nearest_emergency_department_miles = "Distance to nearest ED (miles)",
    academic_hospital_in_municipality = "Academic hospital present in municipality",
    is_closest_ed_free_standing = "Closest ED is free standing",
    acc_chest_pain_accreditation_or_certification = "Closest ED has ACC Chest Pain Accreditation or Certification",
    closest_ed_number_beds = "Number of beds in closest ED"
  )

df_clean

write_rds(df_clean, "../data/clean/01_clean_dataset.rds")

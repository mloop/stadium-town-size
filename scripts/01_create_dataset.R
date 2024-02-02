library(tidyverse)
library(googlesheets4)
library(sjlabelled)

df <- read_sheet("https://docs.google.com/spreadsheets/d/1DBve4JLWO-tCRHOF8aTBYVG_T5ndqQikje37zUBLCMU/edit#gid=0")

hosp <- read_excel("../data/raw/2022 Reporting Cycle Teaching Hospital List October 2021 Posting.xlsx",
                   skip = 5) %>%
  select(`City...7`, `State...8`) %>%
  rename(municipality_name = `City...7`,
         municipality_state = `State...8`) %>%
  left_join(., tibble(municipality_state = state.abb,
                      full_name = state.name)) %>%
  select(-municipality_state) %>%
  rename(municipality_state = full_name) %>%
  mutate(teaching_hospital_in_municipality = 1) %>%
  group_by(municipality_name, municipality_state, teaching_hospital_in_municipality) %>%
  slice(1) %>%
  ungroup()

df_hosp <- df %>%
  mutate(municipality_state = trimws(municipality_state)) %>%
  left_join(., 
            hosp, 
            by = c("municipality_name", "municipality_state"),
            relationship = "many-to-many") %>%
  mutate(teaching_hospital_in_municipality = if_else(is.na(teaching_hospital_in_municipality), 0, teaching_hospital_in_municipality))

df %>% Hmisc::describe()

df_clean <- df_hosp %>%
  select(-name_closest_ed, 
         -`...16`,
         -source_municipality_population,
         -municipality_population_year,
         -academic_hospital_in_municipality) %>%
  var_labels(
    school = "Name of College/University",
    municipality_name = "Municipality",
    municipality_county = "County",
    municipality_state = "State",
    conference = "2023 Football Conference",
    football_stadium_capacity = "Football stadium capacity",
    municipality_population = "Population size",
    distance_nearest_emergency_department_miles = "Distance to nearest ED (miles)",
    # academic_hospital_in_municipality = "Academic hospital present in municipality", this variable turned out to be kind of vague
    is_closest_ed_free_standing = "Closest ED is free standing",
    acc_chest_pain_accreditation_or_certification = "Closest ED has ACC Chest Pain Accreditation or Certification",
    closest_ed_number_beds = "Number of beds in closest ED",
    teaching_hospital_in_municipality = "Teaching hospital in municipality"  # this variable is defined by CMS data from 2022
  )

df_clean

write_rds(df_clean, "../data/clean/01_clean_dataset.rds")

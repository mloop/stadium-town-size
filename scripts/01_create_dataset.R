library(tidyverse)
library(googlesheets4)
library(sjlabelled)
library(haven)
library(readxl)

df <- read_sheet("https://docs.google.com/spreadsheets/d/1DBve4JLWO-tCRHOF8aTBYVG_T5ndqQikje37zUBLCMU/edit") %>%
  mutate(municipality_county = ifelse(str_detect(municipality_county, fixed("parish", ignore_case = TRUE)),
                          municipality_county,
                          paste0(municipality_county, " County")
                          )
         ) %>%
  rename(county = municipality_county)

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

# Add county health rankings data
county <- read_sas("../data/raw/analytic_data2023_0 (4).sas7bdat") %>%
  select(
    state,
    county,
    county_ranked,
    statecode,
    countycode,
    fipscode,
    v138_rawvalue, # drug overdose deaths
    v135_rawvalue,  # injury deaths
    v004_rawvalue,  # primary care physicians
    v134_rawvalue,  # alcohol impaired driving deaths
    v049_rawvalue  # excessive drinking
  ) %>%
  as_tibble() %>%
  rename(
    drug_overdose_deaths = v138_rawvalue,
    injury_deaths = v135_rawvalue,
    primary_care_physicians = v004_rawvalue,
    alcohol_impaired_driving_deaths = v134_rawvalue,
    excessive_drinking = v049_rawvalue
  ) %>%
  filter(str_detect(county, fixed("county", ignore_case = TRUE)) | str_detect(county, fixed("parish", ignore_case = TRUE))) %>%
  left_join(., tibble(state = state.abb,
                      full_name = state.name),
            by = "state") %>%
  select(-state) %>%
  rename(municipality_state = full_name) %>%
  var_labels(
    excessive_drinking = "Percentage of adults reporting binge or heavy drinking (age-adjusted)",
    alcohol_impaired_driving_deaths = "Percentage of driving deaths with alcohol involvement",
    primary_care_physicians = "Number of primary care physicians per 100,000 population",
    injury_deaths = "Number of deaths due to injury per 100,000 population",
    drug_overdose_deaths = "Number of drug poisoning deaths per 100,000 population"
  )

df_clean <- df_hosp %>%
  select(-name_closest_ed, 
         -`...16`,
         -source_municipality_population,
         -municipality_population_year,
         -academic_hospital_in_municipality) %>%
  left_join(., county,
            by = c("municipality_state", "county")) %>%
  var_labels(
    school = "Name of College/University",
    municipality_name = "Municipality",
    county = "County",
    municipality_state = "State",
    conference = "2023 Football Conference",
    football_stadium_capacity = "Football stadium capacity",
    municipality_population = "Population size",
    distance_nearest_emergency_department_miles = "Distance to nearest ED (miles)",
    # academic_hospital_in_municipality = "Academic hospital present in municipality", this variable turned out to be kind of vague
    is_closest_ed_free_standing = "Closest ED is free standing",
    acc_accreditation_or_certification = "Closest ED has ACC Chest Pain Accreditation or Certification",
    closest_ed_number_beds = "Number of beds in closest ED",
    teaching_hospital_in_municipality = "Teaching hospital in municipality"  # this variable is defined by CMS data from 2022
  )

df_clean

write_rds(df_clean, "../data/clean/01_clean_dataset.rds")

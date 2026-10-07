library(readxl)
library(dplyr)
library(purrr)
library(readr)

base_path <- "C:/Users/dorot/The University of Sydney (Staff)/Dorothy Honour's Thesis - Dorothy - Honour's Thesis - Documents/Data/ABS/Childcare workers/"

clean_cw_file <- function(year) {
  
  file_path <- paste0(base_path, year, "_CW.xlsx")
  
  read_excel(file_path, sheet = "Data sheet 0") %>%
    slice(-c(1:8, 24:37)) %>%
    rename(
      state             = ...2,
      gccsa_type        = ...3,
      count_CW_workers  = ...4,
      count_CW_teachers = ...5,
      total_CW          = ...6
    ) %>%
    mutate(year = year) %>%
    fill(state, .direction = "down") %>%
    mutate(gccsa_code = case_when(
      state == "New South Wales"              & gccsa_type == "Greater Capital City" ~ "1GSYD",
      state == "New South Wales"              & gccsa_type == "Rest of State"        ~ "1RNSW",
      state == "Victoria"                     & gccsa_type == "Greater Capital City" ~ "2GMEL",
      state == "Victoria"                     & gccsa_type == "Rest of State"        ~ "2RVIC",
      state == "Queensland"                   & gccsa_type == "Greater Capital City" ~ "3GBRI",
      state == "Queensland"                   & gccsa_type == "Rest of State"        ~ "3RQLD",
      state == "South Australia"              & gccsa_type == "Greater Capital City" ~ "4GADE",
      state == "South Australia"              & gccsa_type == "Rest of State"        ~ "4RSAU",
      state == "Western Australia"            & gccsa_type == "Greater Capital City" ~ "5GPER",
      state == "Western Australia"            & gccsa_type == "Rest of State"        ~ "5RWAU",
      state == "Tasmania"                     & gccsa_type == "Greater Capital City" ~ "6GHOB",
      state == "Tasmania"                     & gccsa_type == "Rest of State"        ~ "6RTAS",
      state == "Northern Territory"           & gccsa_type == "Greater Capital City" ~ "7GDAR",
      state == "Northern Territory"           & gccsa_type == "Rest of State"        ~ "7RNTE",
      state == "Australian Capital Territory" & gccsa_type == "Greater Capital City" ~ "8ACTE"
    )) %>%
    select(year, gccsa_code, count_CW_workers, count_CW_teachers, total_CW)
}

childcare_workers_2021_2025 <- map_dfr(2021:2025, clean_cw_file)



clean_cw_file <- function(year) {
  
  file_path <- paste0(base_path, year, "_CW.xlsx")
  
  read_excel(file_path, sheet = "Data sheet 0") %>%
    slice(-c(1:8, 32:48)) %>%
    rename(
      state             = ...2,
      gccsa_type        = ...3,
      count_CW_workers  = ...4,
      count_CW_teachers = ...5,
      total_CW          = ...6
    ) %>%
    mutate(year = year) %>%
    fill(state, .direction = "down") %>%
mutate(gccsa_code = case_when(
  state == "New South Wales"              & gccsa_type == "Capital City" ~ "1GSYD",
  state == "New South Wales"              & gccsa_type == "Capital city" ~ "1GSYD",
  state == "New South Wales"              & gccsa_type == "Balance of state"        ~ "1RNSW",
  state == "New South Wales"              & gccsa_type == "Balance of State"        ~ "1RNSW",
  state == "Victoria"                     & gccsa_type == "Capital city" ~ "2GMEL",
  state == "Victoria"                     & gccsa_type == "Capital City" ~ "2GMEL",
  state == "Victoria"                     & gccsa_type == "Balance of state"        ~ "2RVIC",
  state == "Victoria"                     & gccsa_type == "Balance of State"        ~ "2RVIC",
  state == "Queensland"                   & gccsa_type == "Capital city" ~ "3GBRI",
  state == "Queensland"                   & gccsa_type == "Capital City" ~ "3GBRI",
  state == "Queensland"                   & gccsa_type == "Balance of state"        ~ "3RQLD",
  state == "Queensland"                   & gccsa_type == "Balance of State"        ~ "3RQLD",
  state == "South Australia"              & gccsa_type == "Capital city" ~ "4GADE",
  state == "South Australia"              & gccsa_type == "Capital City" ~ "4GADE",
  state == "South Australia"              & gccsa_type == "Balance of State"        ~ "4RSAU",
  state == "South Australia"              & gccsa_type == "Balance of state"        ~ "4RSAU",
  state == "Western Australia"            & gccsa_type == "Capital city" ~ "5GPER",
  state == "Western Australia"            & gccsa_type == "Capital City" ~ "5GPER",
  state == "Western Australia"            & gccsa_type == "Balance of State"        ~ "5RWAU",
  state == "Western Australia"            & gccsa_type == "Balance of state"        ~ "5RWAU",
  state == "Tasmania"                     & gccsa_type == "Capital City" ~ "6GHOB",
  state == "Tasmania"                     & gccsa_type == "Capital city" ~ "6GHOB",
  state == "Tasmania"                     & gccsa_type == "Balance of State"        ~ "6RTAS",
  state == "Tasmania"                     & gccsa_type == "Balance of state"        ~ "6RTAS",
  state == "Northern Territory"           & gccsa_type == "Capital City" ~ "7GDAR",
  state == "Northern Territory"           & gccsa_type == "Capital city" ~ "7GDAR",
  state == "Northern Territory"           & gccsa_type == "Balance of State"        ~ "7RNTE",
  state == "Northern Territory"           & gccsa_type == "Balance of state"        ~ "7RNTE",
  state == "Australian Capital Territory" & gccsa_type == "Capital City" ~ "8ACTE",
  state == "Australian Capital Territory" & gccsa_type == "Capital city" ~ "8ACTE"
)) %>%
  select(year, gccsa_code, count_CW_workers, count_CW_teachers, total_CW)%>%
  filter(!is.na(gccsa_code))
}

  childcare_workers_2018_2020 <- map_dfr(2018:2020, clean_cw_file)

  childcare_workers_all <- rbind(childcare_workers_2018_2020, childcare_workers_2021_2025)

  write.csv(childcare_workers_all, "childcare_workers_2018_2025.csv")


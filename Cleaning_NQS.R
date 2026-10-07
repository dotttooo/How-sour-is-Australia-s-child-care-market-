#NQF CLEAN
library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)
library(purrr)
library(strayr)
library(sf)


library(devtools)       
devtools::install_github("wfmackey/absmapsdata")
library(absmapsdata)
data(package = "absmapsdata")



sheets <- excel_sheets("NQS_Q42025.xlsx")

data_sheets <- sheets[grepl("Q\\d+data", sheets)]  # keeps only tabs matching "Q####data"

# read and combine
combined <- map_dfr(data_sheets, function(x) {
  read_excel("NQS_Q42025.xlsx", 
             sheet = x,
             na = c("", "NA", "-")) %>%   # treat "-" as NA on read to fix warnings
    select(where(~ !all(is.na(.)))) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(tab_name = x)
})

ls(combined)

## Cleaning 
clean_combined <- combined %>%
  mutate(
    quarter = as.numeric(substr(tab_name, 2, 2)),                
    year    = as.numeric(substr(tab_name, 3, 6)),
    year_quarter = ((year-2000)*4+quarter),
  )%>%
  
  mutate(
    `Service ID` = coalesce(`Service ID`, `Service Approval Number`),
    `Approval Date`           = coalesce(`Approval Date`, `ApprovalDate`),
    `ARIA+`                   = coalesce(`ARIA+`, `ARIA`),
    `Overall Rating`          = coalesce(`Overall Rating`, `OverallRating`),
    `Q1`          = coalesce(`Quality Area 1`, `Q1`),
    `Q2`          = coalesce(`Quality Area 2`, `Q2`),
    `Q3`          = coalesce(`Quality Area 3`, `Q3`),
    `Q4`          = coalesce(`Quality Area 4`, `Q4`),
    `Q5`          = coalesce(`Quality Area 5`, `Q5`),
    `Q6`          = coalesce(`Quality Area 6`, `Q6`),
    `Q7`          = coalesce(`Quality Area 7`, `Q7`),
    `Service Sub Type`        = coalesce(`Service Sub Type`,
                                         `Service sub-type (ordered counting method)`),
    `OSHC Before School`      = coalesce(`OSHC Before School`, `OSHC BeforeSchool`),
    
    # Consolidate all Preschool Stand Alone variants into one clean column
    `Preschool Kindergarten Stand Alone` = coalesce(
      `Preschool/Kindergarten Stand Alone`,
      `PreschoolKindergarten Stand Alone`,
      `Preschool/\r\nKindergarten Stand Alone`
    ),
    
    # Consolidate all Preschool Part of a School variants into one clean column
    `Preschool Kindergarten Part of a School` = coalesce(
      `PreschoolKindergarten Part of a School`,
      `Preschool/\r\nKindergarten Part of a School`
    )
  ) %>%
  
  select(-c(
    `Service Approval Number`,
    `ApprovalDate`,
    `ARIA`,
    `OverallRating`,
    `Quality Area 1`, `Quality Area 2`, `Quality Area 3`, `Quality Area 4`, `Quality Area 5`, `Quality Area 6`, `Quality Area 7`,
    `Service sub-type (ordered counting method)`,
    `OSHC BeforeSchool`,
    `Preschool/Kindergarten Stand Alone`,
    `PreschoolKindergarten Stand Alone`,
    `Preschool/\r\nKindergarten Stand Alone`,
    `PreschoolKindergarten Part of a School`,
    `Preschool/\r\nKindergarten Part of a School`
  )) %>%
  
  # I look to only inspect centre based care and not family day care. By default the service sub type long day care should be LDC. Even though some Long Day Care will say yes for FDC types as well.
  filter(`Service Type` == "Centre-Based Care",
         `Service Sub Type` == "LDC")%>%
  select(-c(`Service Type`, `Service Sub Type`,
            `Long Day Care`, `OSHC Before School`, `OSHC After School`, `OSHC Vacation Care`, `Nature Care Other`))%>%
  
  distinct()

# Recode ratings
recode_rating <- function(x) {
  case_when(
    x == "Significant Improvement Required" ~ 1,
    x == "Working Towards NQS"              ~ 2,
    x == "Meeting NQS"                      ~ 3,
    x == "Excellent"                        ~ 4,
    x == "Exceeding NQS"                    ~ 5,
    TRUE                                    ~ NA_real_
  )
}

# apply to all rating columns at once
clean_combined <- clean_combined %>%
  mutate(across(c(`Overall Rating`, 
                  `Q1`, `Q2`, `Q3`,
                  `Q4`, `Q5`, `Q6`, 
                  `Q7`), recode_rating))


# ── SPATIAL JOIN SA1 / SA2 / SA3 ─────────────────────────────────────────────
sa1 <- read_absmap("sa12021")

# Backup coordinates from the price dataset, keyed by sid - used to try fix missign SA2 but dud not work
backup_coords <- read.csv("clean_FULLDAY.csv") %>%
  distinct(sid, latitude, longitude) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  rename(backup_lat = latitude, backup_lon = longitude)

clean_combined <- clean_combined %>%
  mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) %>%
  left_join(backup_coords, by = c("Service ID" = "sid")) %>%
  mutate(Latitude = if_else(is.na(Latitude), backup_lat, Latitude),
         Longitude = if_else(is.na(Longitude), backup_lon, Longitude)) %>%
  select(-backup_lat, -backup_lon) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_join(sa1 %>% select(
    sa1_code_2021,
    sa2_code_2021, sa2_name_2021,
    sa3_code_2021, sa3_name_2021,
    sa4_code_2021, sa4_name_2021,
    gcc_code_2021, gcc_name_2021,
    state_code_2021, state_name_2021
  )) %>%
  st_drop_geometry()


#Rename variables
clean_combined <- clean_combined %>%
  rename(pid = "Provider ID",
         sid = "Service ID",
         management = "Provider Management Type", 
         ARIA = "ARIA+", 
         provider = "Provider Name",
         service = "Service Name",
         capacity = "Maximum total places",
         sa1_code = "sa1_code_2021",
         sa2_code = "sa2_code_2021",
         sa3_code = "sa3_code_2021",
         sa3_name = "sa3_name_2021",
         sa4_code = "sa4_code_2021",
         sa4_name = "sa4_name_2021",
         gccsa_code = "gcc_code_2021",
         state_code = "state_code_2021",
         state_name = "state_name_2021")





### Sometimes management is 'other' -------------------------------------------

## Replacing management = other, by referencing the management of records from future years where the data is more complete
clean_combined <- clean_combined %>%
  group_by(provider) %>%
  mutate(
    # Find the most common non-"Other" and non-"Not Stated" management type
    known_management = {
      vals <- na.omit(unique(management[!management %in% c("Other", "Not Stated")]))
      if (length(vals) == 0) "Other/Unknown" else first(vals)
    },
    
    # Replace both "Other" and "Not Stated" if a known type exists
    management = if_else(management %in% c("Other", "Not Stated") & !is.na(known_management),
                         known_management,
                         management)
  ) %>%
  ungroup() %>%
  # Drop any rows that are still "Other" or "Not Stated"
  filter(!management %in% c("Other", "Not Stated")) %>%
  select(-known_management)%>%
  #create wider management groups
  mutate(management_wide = case_when(
    management == "Private for profit" ~ "For profit",
    management %in% c("Private not for profit community managed",
                      "Private not for profit other organisations") ~ "Not for profit",
    management %in% c("State/Territory and Local Government managed",
                      "State/Territory government schools") ~ "State/Territory",
    management %in% c("Catholic schools", "Independent schools") ~ "Private Schools",
    TRUE ~ "Other/Unknown"  # fallback just in case
  ))

list_unknown_management <- clean_combined %>% filter(management_wide == "Other/Unknown")%>%
  distinct(provider)

# Utilising Claude to research, define and reclassify unknowns
clean_combined <- clean_combined %>%
  mutate(management_wide = case_when(
    # provider name overrides - For Profit
    provider %in% c(
      "ADELAIDE BUSINESS GROUP PTY LTD",
      "Ringa Ringa Rosey Pty Ltd",
      "Joanne Greenwood",
      "Ngoc Trinh",
      "Brookvale Childcare Pty Ltd",
      "Brightstars Early Learning Centres Macquarie Fields Pty Ltd",
      "Kids Academy Erina Heights Pty Ltd",
      "Little Brightstars Early Learning Pty Ltd",
      "Susan Kanisek",
      "Karen Place Pty Ltd ATF McNamara Family Trust (Subject To Deed of Company Arrangement)",
      "Ms Sandra Ashton",
      "Mrs Linda Anne Edwards",
      "Giggle FDC Pty Ltd as Trustee for Lewis-Smith Family Trust",
      "Kangabunnabys Childcare (Yamba) Pty Ltd",
      "Dalruby Pty Ltd",
      "Paradise Pre-School & Long Day Child Care Centre Pty Ltd",
      "MHRHPty Ltd",
      "Mrs Natalie Frances Braun",
      "Sarai Holdings Pty Ltd",
      "Fortad Pty Ltd",
      "Village Kids Childrens Centre - Bentley Park Pty Ltd",
      "Waterlily Early Learning Centre Pty Ltd",
      "Kerry Lynn Crowe",
      "Ms Katherine Joyce Smith",
      "Mr William Edwards",
      "Mrs Katrina Fay Dyson",
      "Giggle FDC Pty Ltd ATF The Trustee for Lewis-Smith Family Trust",
      "Aspley Early Learning Centre No 2 Pty Ltd ATF Earl Family Trus",
      "Aspley Early Learning Centre No 2 Pty Ltd ATF Earl Family Trust",
      "Aspley Early Learning Centre No 2 Pty Ltd ATF Earl Family Trust No 2",
      "Marlimarli Daycare Pty Ltd",
      "Three Little Pigs Pty Limited",
      "Tustin Developments No2 Pty Ltd",
      "Sydney Cove Children's Centre B Pty Ltd",
      "Sydney Cove Children's Centre C Pty Ltd",
      "Sydney Cove Children's Centre Pty Ltd"
      
    ) ~ "For profit",
    
    # provider name overrides - Not for Profit
    provider %in% c(
      "CANOPY COMMUNITY LTD",
      "O'Sullivan Beach Children's Centre (Child Care) Incorporated",
      "Dandaloo Gayngil Aboriginal Corporation",
      "Brook View Family Centre Incorporated",
      "High Wycombe Out of School Care Centre Incorporated",
      "Rose Nowers Early Learning Centre Inc",
      "Mortdale Community Services Incorporated",
      "Busy Bee Pre Kindergarten Inc",
      "Carnamah Child Care Centre Inc.",
      "Balwyn Community Centre Inc",
      "Hampden Bridge Child Care Centre",
      "Lady Forster Kindergarten Inc",
      "Steiner In The Eurobodalla Incorporated",
      "Eudunda Community Centre Inc",
      "Busselton Family Playgroup Inc",
      "Djarindjin Aboriginal Corporation",
      "Mirrabooka Multicultural Child Care Centre Inc",
      "Special Needs Support Group Inc",
      "Derek Robson Children's Services Centre Inc",
      "Albert Park College Childcare Centre Association Inc",
      "Williamstown Child Care Centre Co-op Ltd",
      "Haddon & District Community House Inc",
      "Manning River Centre For Rudolf Steiner Education Limited"
    ) ~ "Not for profit",
    
    # provider name overrides - State/Territory
    provider %in% c(
      "Australian Sports Commission",
      "Gunbalanya Community School",
      "Ngukurr Community Education Centre School Council",
      "Nakara School Council Inc",
      "Clyde Fenton School Council Inc",
      "TAFESA",
      "Casey City Council",
      "Bendigo Regional Institute of TAFE",
      "Pittwater Council"
    ) ~ "State/Territory",
    
    # provider name overrides - Private Schools
    provider %in% c(
      "PARKLANDS CHRISTIAN COLLEGE LIMITED",
      "Penrhos College",
      "Wesley College",
      "St Nicholas School Ltd"
    ) ~ "Private Schools",
    
    TRUE ~ management_wide
  ))

### Calculating age of service and provider -------------------------------------------

clean_combined <- clean_combined %>%
  group_by(sid) %>%
  mutate(
    sid_first_quarter = min(year_quarter))%>%
  ungroup()%>%
   # sid_last_quarter = max(year_quarter),
    mutate(sid_age = year_quarter - sid_first_quarter
  ) %>%
  ungroup() %>%
  group_by(pid) %>%
  mutate(
    pid_first_quarter = min(year_quarter))%>%
  ungroup()%>%
  mutate(
    pid_age = year_quarter - pid_first_quarter
  ) %>%
  ungroup()%>%
  select(-c(pid_first_quarter, sid_first_quarter))

wilcox.test(clean_combined$sid_age, clean_combined$pid_age)


# ### Create a dummy for an entrants first rating the first time it appears -------------------------------------------
# 
# clean_combined <- clean_combined %>%
#   group_by(sid, year_quarter)%>%
#   slice() #so it only takes the most recent
# 
# THIS IS INCOMPLETE
# View(clean_combined)



write.csv(clean_combined, "NQS_clean.csv")


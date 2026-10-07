library(dplyr)
library(readr)
library(readxl)
library(sf)
library(dplyr)
library(strayr)
library(absmapsdata)

####### Join Data sets

ACARA <- read.csv("School Profile 2008-2025.csv")
ACARA_loc <- read.csv("School Location 2025.csv")

ACARA <- ACARA %>%
  select(c("ACARA.SML.ID", "Calendar.Year", "ICSEA", "Year.Range"))
ACARA_loc <- ACARA_loc %>%
  select(!c("Calendar.Year","Location.AGE.ID","School.AGE.ID","Rolled.School.ID"))

ACARA_complete <- ACARA %>%
  left_join(ACARA_loc, by = "ACARA.SML.ID")
  
write.csv(ACARA_complete, "ACARA_complete.csv")
  
########## Cleaning data
ACARA_complete <- read.csv("ACARA_complete.csv")

ACARA_complete <- ACARA_complete %>%
  select(c("ACARA.SML.ID", "Calendar.Year", "ICSEA", "Year.Range", "School.Type", "School.Sector", "Latitude","Longitude",
           "Statistical.Area.2", "Statistical.Area.3","Statistical.Area.4","State"))%>%
  rename(sa2_code = "Statistical.Area.2",
         sa3_code = "Statistical.Area.3",
         sa4_code = "Statistical.Area.4",
         year = "Calendar.Year")

write.csv(ACARA_complete, "ACARA_complete.csv")

View(ACARA_complete)


#### NEED TO FIGURE OUT HOW TO BEST MATCH ACARA TO REGRESSION DATATABLE -> I think it is best if I create averages at the SA2 level then km radiuses because my comp var assumes at SA2 level then create one for SA3, LGA etc. or whatever matches my assumptions
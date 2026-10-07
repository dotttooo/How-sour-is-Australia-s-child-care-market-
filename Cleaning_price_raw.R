library(dplyr)
library(tidyr)
library(readr)
library(writexl)

clean <- read_csv("phase2_historical_fees_250226.csv")%>%
  distinct()%>%
  mutate(
    ageGroup = recode(
      ageGroup, 
      "0012MN" = "0_12_mths",
      "1324MN" = "13_24_mths",
      "2535MN" = "25_35_mths",
      "36MNPR" = "36+_mths",
      "OVPRAG" = "over_preschool_age"))%>%
  mutate(serviceId = substr(serviceId, 1, 11))%>%
  select(!publicId)%>%
  group_by(serviceId, providerId,serviceName, latitude, longitude, date, sessionType, ageGroup)%>%
  mutate(rate_index = row_number() -1) %>%
  ungroup()

#Pivot children prices
clean_wide_FULLDAY <- clean %>%
  filter(sessionType == "FULLDY") %>%
  pivot_wider(
    names_from  = c(ageGroup, rate_index),
    values_from = rate,
    names_glue  = "{ageGroup}_{rate_index}")

# # ## Cleaning out dupes
# dupe_rows <- clean_wide_FULLDAY %>%
#   filter(if_any(matches("_[1-9][0-]*$"), ~ !is.na(.)))
# 
# # write_csv(dupe_rows, "FULLDAY_dupes.csv")

# Clean without dupes (delete all dupes)
clean_wide_FULLDAY <- clean_wide_FULLDAY %>%
  filter(!if_any(matches("_[1-9][0-9]*$"), ~ !is.na(.x)))%>%
  select(
    serviceId,
    providerId,
    serviceName,
    latitude,
    longitude,
    date,
    `0_12_mths_0`,
    `13_24_mths_0`,
    `25_35_mths_0`,
    `36+_mths_0`,
    over_preschool_age_0
  )%>%
  rename_with(~ sub("_0$", "", .x))

#Calculate an average cost
clean_wide_FULLDAY <- clean_wide_FULLDAY %>%
  #Hindsight checking, some costs were extremely high - checking individual sites this was a data entry error by the centres themselves. Without making assumptions and adjusting them separately I have chosen to omit them but this will not significantly affect the average.
  mutate(across(c(`0_12_mths`, `13_24_mths`, `25_35_mths`, `36+_mths`), 
                ~ if_else(. > 500, 0, .))) %>% #500 was set after checking websites for highest prices and where I realised errors sat
  mutate(cost = rowMeans(across(c(`0_12_mths`, `13_24_mths`, `25_35_mths`, `36+_mths`)), na.rm = TRUE))

## Manipulating data for merging with ACECQA
#1 Turn dates into quarters

childcare_price_dt <- clean_wide_FULLDAY %>%
  mutate(date = as.Date(date), 
         year = year(date),
         quarter = quarter(date), 
         year_quarter = (year - 2000)*4+quarter)%>%
  group_by(serviceId, providerId, latitude, longitude, year_quarter) %>%
  #sometimes price changes within the same quarter - therefore I am taking the most recent price change if this happens
  slice_max(date, n=1, with_ties = FALSE) %>%
  ungroup()%>%
  #remove centres with no price reported 
  filter(cost > 0.01)%>%
  #select only relevant columns
  mutate(sid = serviceId)%>%
  select(c(sid, year_quarter, cost, latitude, longitude))%>% #not selecting pid because the scrape takes the most recent pid but historical prices so matching would be incorrect if I took it
  
  
  
  ####NOT THE CLEANEST _ SEE IF I CAN MERGE DUPES IF THEY ARE EXACTLY THE SAME, AND DELETE THOSE WHO ARE NOT OR ARE INCONSISTENT IN THE DATA (if)
  
  write_csv(childcare_price_dt, "clean_FULLDAY.csv")


### For cost analysis

cost_analysis <- clean_wide_FULLDAY %>%
  mutate(date = as.Date(date), 
         year = year(date),
         quarter = quarter(date), 
         year_quarter = (year - 2000)*4+quarter)%>%
  group_by(serviceId, providerId, latitude, longitude, year_quarter) %>%
  #sometimes price changes within the same quarter - therefore I am taking the most recent price change if this happens
  slice_max(date, n=1, with_ties = FALSE) %>%
  ungroup()%>%
  #remove centres with no price reported 
  filter(cost > 0.01)%>%
  #select only relevant columns
  mutate(sid = serviceId)%>%
  select(c(sid, year_quarter, cost, latitude, longitude, sa32021))



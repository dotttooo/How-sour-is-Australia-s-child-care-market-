#DeptEd CLEAN
library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(readxl)
library(purrr)


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

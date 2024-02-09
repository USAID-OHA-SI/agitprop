# PROJECT:  agitprop
# AUTHOR:   A.Chtourou, G. Sarfaty, K.Srikanth | USAID
# PURPOSE:  2023 OHA Top TPs 
# REF ID:   3adb8fa1 
# LICENSE:  MIT
# DATE:     2024-01-26
# NOTE:     Adapted from WAD_TPs_2023

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(systemfonts)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(janitor)
library(lubridate)

# SI specific paths/functions  
load_secrets()

# MSD
filepath <- si_path() %>% 
  return_latest("MER_Structured_Datasets_OU_IM_FY21-24")

# Grab metadata
get_metadata(filepath) 

# MSD PREP --------------------------------------

#read MSD
df_msd <- read_psd(filepath)

#read NGA genie file
df_nga <- si_path() %>% 
  return_latest("All indicators_FY23Q4_Nigeria") %>% 
  read_csv()

#select only names in MSD
names_to_keep <- names(df_msd)
setdiff(names(df_msd), names(df_nga)) #only qtrs
setdiff(names(df_nga), names(df_msd))

#select all names from NGA file that match MSD (minus qtr1-qtr4)
df_nga_new <- df_nga %>% 
  select(any_of(names_to_keep))

#rbind together removing TZ and NGA for FY23 ONLY
df <- df_msd %>% 
  filter(!(operatingunit=="Nigeria" & fiscal_year=="2023"),
         !(operatingunit=="Tanzania" & fiscal_year=="2023")) %>% 
  select(-c(qtr1:qtr4)) %>% 
  rbind(df_nga_new)

#initial MSD 
df_initial <- si_path() %>%
  return_latest("MER_Structured_Datasets_OU_IM_FY21-24_20231114_v1_1") %>%
  read_psd()

# #check which variables in df_initial and not in df
variables_not_in_df_initial <- setdiff(names(df), names(df_initial))
variables_not_in_df <- setdiff(names(df_initial), names(df))

# Print the results
cat("Variables in df but not in df_initial:", variables_not_in_df_initial, "\n")
cat("Variables in df_initial but not in df:", variables_not_in_df, "\n")

#exclude extra columns and filter initial dataset to Tanzania
df_msd_TZA <- df_initial %>% 
  select(-c(qtr1:qtr4)) %>% 
  filter(operatingunit == "Tanzania")

#bind initial TZA data 
df_final <- df %>% 
  rbind(df_msd_TZA)

# QUICK CHECKS -----------------------------------------------------------------
checks<-df_final %>% 
  mutate(OU_group=case_when(
    operatingunit=="Nigeria" ~ "Nigeria",
    operatingunit=="Tanzania" ~ "Tanzania",
    TRUE ~ "All Other OUs"
  )) %>% 
  filter(fiscal_year=="2023",
         indicator %in% c("HTS_TST","HTS_TST_POS","TX_CURR"),
         standardizeddisaggregate=="Total Numerator",
         indicatortype !='CS') %>% 
  group_by(OU_group,funding_agency,indicator) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(indicator,cumulative) %>% 
  prinf()
  
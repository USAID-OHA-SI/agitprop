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


#run join code prior to this 
df_arch <- si_path() %>% 
  return_latest("OU_IM_FY15") %>% 
  read_psd() %>% 
  select(-c(qtr1:qtr4)) 

#bind archived + current MSD and filter 
df_bind <- df_final %>%
  bind_rows(df_arch) %>% 
  dplyr::filter(funding_agency %in% "USAID") %>% 
  mutate(fiscal_year=as.character(fiscal_year))

# INDICATOR REFERENCE ----------------------------------------------------------
ind<-distinct(df_bind,indicator,standardizeddisaggregate)


# ESTIMATION INPUTS ------------------------------------------------------------


TX_U15<-df_bind %>% 
  mutate(OU_group=case_when(
    operatingunit=="Nigeria" ~ "Nigeria",
    operatingunit=="Tanzania" ~ "Tanzania",
    TRUE ~ "All Other OUs"
  )) %>% 
  filter(indicator=="TX_CURR",
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus", "MostCompleteAgeDisagg"),
         funding_agency=="USAID",
         !fiscal_year=="2024") %>% 
  group_by(fiscal_year,OU_group,funding_agency,indicator,trendscoarse) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(trendscoarse,cumulative) %>% 
  mutate(total = coalesce(`<15`, 0) + coalesce(`15+`, 0) + coalesce(`Unknown Age`, 0),
         prct_u15=(`<15`/total)*100) %>% 
  arrange(OU_group) %>% 
  prinf()



TX_PVLS_U15<-df_bind %>% 
  mutate(OU_group=case_when(
    operatingunit=="Nigeria" ~ "Nigeria",
    operatingunit=="Tanzania" ~ "Tanzania",
    TRUE ~ "All Other OUs"
  )) %>% 
  filter(indicator=="TX_PVLS",
         standardizeddisaggregate %in% c("Age Aggregated/Sex/Indication/HIVStatus", "Age/Sex/Indication/HIVStatus", "Age Aggregated/Sex"),
         funding_agency=="USAID",
         trendscoarse=="<15",
         !fiscal_year=="2024") %>% 
  group_by(fiscal_year,OU_group,funding_agency,indicator,numeratordenom,trendscoarse) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(numeratordenom,cumulative) %>% 
  mutate(vls = (N/D)*100) %>% 
  arrange(OU_group) %>% 
  prinf()


TX_PVLS_U15_all<-df_bind %>% 
  filter(indicator=="TX_PVLS",
         standardizeddisaggregate %in% c("Age Aggregated/Sex/Indication/HIVStatus", "Age/Sex/Indication/HIVStatus", "Age Aggregated/Sex"),
         funding_agency=="USAID",
         trendscoarse=="<15",
         fiscal_year=="2023",
         !operatingunit %in% c("Nigeria", "Tanzania")) %>% 
  group_by(fiscal_year,funding_agency,indicator,numeratordenom,trendscoarse) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(numeratordenom,cumulative) %>% 
  mutate(vls = (N/D)*100) %>% 
  prinf()


TX_KP<-df_bind %>% 
  mutate(OU_group=case_when(
    operatingunit=="Nigeria" ~ "Nigeria",
    operatingunit=="Tanzania" ~ "Tanzania",
    TRUE ~ "All Other OUs"
  )) %>% 
  filter(indicator=="TX_CURR",
         standardizeddisaggregate=="KeyPop/HIVStatus",
         funding_agency=="USAID",
         fiscal_year=="2023") %>% 
  group_by(fiscal_year,funding_agency,OU_group,indicator,standardizeddisaggregate) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  add_row(
    fiscal_year = "2023", 
    OU_group = "Overall", 
    funding_agency = "Total", 
    indicator = "Total",
    standardizeddisaggregate="KeyPop/HIVStatus",
    cumulative = sum(.$cumulative, na.rm = TRUE)) %>% 
  prinf()


TX_KP_USAID<-df_bind %>% 
  mutate(OU_group=case_when(
    operatingunit=="Nigeria" ~ "Nigeria",
    operatingunit=="Tanzania" ~ "Tanzania",
    TRUE ~ "All Other OUs"
  )) %>% 
  filter(indicator=="TX_CURR",
         standardizeddisaggregate %in% c("KeyPop/HIVStatus","Total Numerator"),
         funding_agency=="USAID",
         fiscal_year %in% c("2020","2021","2022","2023")) %>% 
  group_by(fiscal_year,funding_agency,OU_group,indicator,standardizeddisaggregate) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  spread(standardizeddisaggregate,cumulative) %>% 
  mutate(prct_KP=(`KeyPop/HIVStatus`/`Total Numerator`)*100) %>% 
  arrange(OU_group) %>%
  prinf()

  


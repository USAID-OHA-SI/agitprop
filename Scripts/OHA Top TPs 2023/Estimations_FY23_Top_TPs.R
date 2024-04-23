# PROJECT:  agitprop
# AUTHOR:   A.Chtourou, G. Sarfaty, K.Srikanth | USAID
# PURPOSE:  2023 OHA Top TPs 
# REF ID:   3adb8fa1 
# LICENSE:  MIT
# DATE:     2024-01-26
# UPDATED:  2024-04-16
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
library(googlesheets4)

# SI specific paths/functions  
load_secrets()

# MSD
filepath <- si_path() %>% 
  return_latest("MER_Structured_Datasets_OU_IM_FY22-24")

# Grab metadata
get_metadata(filepath) 

# MSD PREP ---------------------------------------------------------------------

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

#rbind together removing NGA for FY23 ONLY
df <- df_msd %>% 
  filter(!(operatingunit=="Nigeria" & fiscal_year=="2023")) %>% 
  select(-c(qtr1:qtr4)) %>% 
  rbind(df_nga_new)

# #Q4 TZA Daily Genie 
# df_TZA <- si_path() %>%
#   return_latest("Genie-OUByIMs-Tanzania-Daily-2024-02-07") %>%
#   read_psd()
# 
# #exclude extra columns 
# df_msd_TZA <- df_TZA %>% 
#   select(-c(qtr1:qtr4)) 
# 
# #bind TZA data to clean MSD w/ NGA file
# df_final <- df %>% 
#   rbind(df_msd_TZA)

df_final<-df


#run join code prior to this 
df_arch <- si_path() %>% 
  return_latest("OU_IM_FY15") %>% 
  read_psd() %>% 
  select(-c(qtr1:qtr4)) 

#bind archived + current MSD and filter 
df_bind <- df_final %>%
  bind_rows(df_arch) %>% 
  # dplyr::filter(funding_agency %in% c("USAID")) %>% #not needed for inital check
  mutate(fiscal_year=as.character(fiscal_year))

# LOCAL PARTNER PREP -----------------------------------------------------------

#Read in the google sheet hyperfile with local partner
sheet_id <- "1MQviknJkJDttGdNEJeNaYPKmHCw6BqPuJ0C5cslV5IE"

df_partner <- read_sheet(sheet_id, sheet = "MechID-PartnerType", range = "A:B") %>% 
  clean_names() %>% 
  rename(mech_code = mechanism_id) %>% 
  mutate(mech_code = as.character(mech_code),
         partner_type = case_when(partner_type == "Regional" ~ "Local",
                                  partner_type == "TBD Local" ~ "Local", TRUE ~ partner_type))

# INDICATOR REFERENCE ----------------------------------------------------------
ind<-distinct(df_bind,indicator,standardizeddisaggregate)


# QUICK CHECKS -----------------------------------------------------------------
checks<-df_bind %>% 
  mutate(OU_group=case_when(
    operatingunit=="Nigeria" ~ "Nigeria",
    TRUE ~ "All Other OUs")) %>% 
  filter(fiscal_year=="2023",
         indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR"),
         standardizeddisaggregate=="Total Numerator",
         indicatortype !="CS") %>% 
  group_by(OU_group,funding_agency,indicator) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(indicator,cumulative) %>% 
  prinf()

# CURR OVERALL -----------------------------------------------------------------

#CURR WITHOUT NGA
curr<-df_bind %>%
  filter(fiscal_year=="2023",
         indicator %in% c("TX_CURR"),
         standardizeddisaggregate=="Total Numerator",
         indicatortype !="CS",
         operatingunit !="Nigeria") %>% 
  group_by(funding_agency,indicator) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(funding_agency,cumulative) %>% 
  prinf()

#FY24Q1 NGA CURR 
NGA_q1<-df_bind %>% 
  filter(fiscal_year=="2024",
         indicator %in% c("TX_CURR"),
         standardizeddisaggregate=="Total Numerator",
         indicatortype !="CS",
         operatingunit =="Nigeria") %>% 
  group_by(operatingunit,fiscal_year,funding_agency,indicator) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(funding_agency,cumulative) %>% 
  mutate(total=`DOD`+`HHS/CDC`+`USAID`,
         usaid_share=`USAID`/total) %>% 
  prinf()


# ESTIMATION INPUTS ------------------------------------------------------------

#TX <15
TX_U15<-df_bind %>% 
  mutate(OU_group=case_when(
    operatingunit=="Nigeria" ~ "Nigeria",
    TRUE ~ "All Other OUs"
  )) %>% 
  filter(indicator=="TX_CURR",
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus", 
                                         "MostCompleteAgeDisagg"),
         funding_agency=="USAID",
         fiscal_year %in% c("2021","2022","2023","2024")) %>% 
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
         standardizeddisaggregate %in% c("Age Aggregated/Sex/Indication/HIVStatus", "Age/Sex/Indication/HIVStatus", 
                                         "Age Aggregated/Sex","Age/Sex/HIVStatus"),
         funding_agency=="USAID",
         trendscoarse=="<15",
         fiscal_year=="2023",
         !operatingunit %in% c("Nigeria")) %>%
  group_by(fiscal_year,funding_agency,indicator,numeratordenom,trendscoarse) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(numeratordenom,cumulative) %>% 
  mutate(vls = (N/D)*100) %>% 
  prinf()


#TX UNDER 10-19
TX_10to19<-df_bind %>% 
  mutate(OU_group=case_when(
    operatingunit=="Nigeria" ~ "Nigeria",
    TRUE ~ "All Other OUs"
  )) %>% 
  filter(indicator=="TX_CURR",
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus", 
                                         "MostCompleteAgeDisagg"),
         funding_agency=="USAID",
         fiscal_year %in% c("2020","2021","2022","2023","2024")) %>% 
  mutate(age_grp=case_when(
    ageasentered %in% c("10-14","15-19") ~ "ten_19",
    TRUE ~ "all other ages"
  )) %>% 
  group_by(fiscal_year,OU_group,age_grp,indicator) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(age_grp,cumulative) %>% 
  mutate(total = coalesce(`ten_19`, 0) + coalesce(`all other ages`, 0),
         prct_ten_19=(`ten_19`/total)*100) %>% 
  arrange(OU_group) %>% 
  prinf()


TX_10to19_vls<-df_bind %>% 
  mutate(OU_group=case_when(
    operatingunit=="Nigeria" ~ "Nigeria",
    TRUE ~ "All Other OUs"
  )) %>% 
  filter(indicator=="TX_PVLS",
         standardizeddisaggregate %in% c("Age Aggregated/Sex/Indication/HIVStatus", 
                                         "Age/Sex/Indication/HIVStatus", 
                                         "Age Aggregated/Sex","Age/Sex/HIVStatus"),
         funding_agency=="USAID",
         fiscal_year %in% c("2020","2021","2022","2023","2024"),
         ageasentered %in% c("10-14","15-19")) %>% 
  group_by(fiscal_year,OU_group,indicator,numeratordenom) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(numeratordenom,cumulative) %>% 
  mutate(vls = (N/D)*100) %>% 
  arrange(OU_group) %>% 
  prinf()


TX_10to19_vls_all<-df_bind %>% 
  filter(indicator=="TX_PVLS",
         standardizeddisaggregate %in% c("Age Aggregated/Sex/Indication/HIVStatus", 
                                         "Age/Sex/Indication/HIVStatus", "Age Aggregated/Sex",
                                         "Age/Sex/HIVStatus"),
         funding_agency=="USAID",
         fiscal_year %in% c("2020","2021","2022","2023","2024"),
         ageasentered %in% c("10-14","15-19")) %>% 
  group_by(fiscal_year,indicator,numeratordenom) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(numeratordenom,cumulative) %>% 
  mutate(vls = (N/D)*100) %>% 
  prinf()


#TX UNDER 20-29
TX_20to29<-df_bind %>% 
  mutate(OU_group=case_when(
    operatingunit=="Nigeria" ~ "Nigeria",
    TRUE ~ "All Other OUs"
  )) %>% 
  filter(indicator=="TX_CURR",
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus", "MostCompleteAgeDisagg"),
         funding_agency=="USAID",
         fiscal_year %in% c("2020","2021","2022","2023","2024")) %>% 
  mutate(age_grp=case_when(
    ageasentered %in% c("20-24","25-29") ~ "twenty_29",
    TRUE ~ "all other ages"
  )) %>% 
  group_by(fiscal_year,OU_group,age_grp,indicator) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(age_grp,cumulative) %>% 
  mutate(total = coalesce(`twenty_29`, 0) + coalesce(`all other ages`, 0),
         prct_twenty_29=(`twenty_29`/total)*100) %>% 
  arrange(OU_group) %>% 
  prinf()


TX_20to29_vls<-df_bind %>% 
  mutate(OU_group=case_when(
    operatingunit=="Nigeria" ~ "Nigeria",
    TRUE ~ "All Other OUs"
  )) %>% 
  filter(indicator=="TX_PVLS",
         standardizeddisaggregate %in% c("Age Aggregated/Sex/Indication/HIVStatus", 
                                         "Age/Sex/Indication/HIVStatus", "Age Aggregated/Sex",
                                         "Age/Sex/HIVStatus"),
         funding_agency=="USAID",
         fiscal_year %in% c("2020","2021","2022","2023","2024"),
         ageasentered %in% c("20-24","25-29")) %>% 
  group_by(fiscal_year,OU_group,indicator,numeratordenom) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(numeratordenom,cumulative) %>% 
  mutate(vls = (N/D)*100) %>% 
  arrange(OU_group) %>% 
  prinf()


TX_20to29_vls_all<-df_bind %>% 
  filter(indicator=="TX_PVLS",
         standardizeddisaggregate %in% c("Age Aggregated/Sex/Indication/HIVStatus", 
                                         "Age/Sex/Indication/HIVStatus", "Age Aggregated/Sex",
                                         "Age/Sex/HIVStatus"),
         funding_agency=="USAID",
         fiscal_year %in% c("2020","2021","2022","2023","2024"),
         ageasentered %in% c("20-24","25-29")) %>% 
  group_by(fiscal_year,indicator,numeratordenom) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(numeratordenom,cumulative) %>% 
  mutate(vls = (N/D)*100) %>% 
  prinf()

# ESTIMATION INPUTS KP----------------------------------------------------------
TX_KP<-df_bind %>% 
  mutate(OU_group=case_when(
    operatingunit=="Nigeria" ~ "Nigeria",
    TRUE ~ "All Other OUs"
  )) %>% 
  filter(indicator=="TX_CURR",
         standardizeddisaggregate=="KeyPop/HIVStatus",
         funding_agency=="USAID",
         fiscal_year =="2023") %>% 
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
    TRUE ~ "All Other OUs"
  )) %>% 
  filter(indicator=="TX_CURR",
         standardizeddisaggregate %in% c("KeyPop/HIVStatus","Total Numerator"),
         funding_agency=="USAID",
         fiscal_year %in% c("2020","2021","2022","2023","2024")) %>% 
  group_by(fiscal_year,funding_agency,OU_group,indicator,standardizeddisaggregate) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  spread(standardizeddisaggregate,cumulative) %>% 
  mutate(prct_KP=(`KeyPop/HIVStatus`/`Total Numerator`)*100) %>% 
  arrange(OU_group) %>%
  prinf()


# LP TX ESTIMATE ---------------------------------------------------------------

df_tx_lp <- df_bind %>%
  left_join(df_partner, by = c("mech_code")) %>%
  filter(funding_agency == "USAID",
         indicator == "TX_CURR",
         standardizeddisaggregate == "Total Numerator",
         fiscal_year %in% c("2022","2023","2024")) %>%
  mutate(ou_qc = case_when(operatingunit == "Nigeria" ~ "Nigeria",
                           TRUE ~ "All other OUs")) %>%
  group_by(funding_agency, fiscal_year, indicator, partner_type,ou_qc) %>%
  summarise(across(cumulative, sum, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(partner_type,cumulative) %>% 
  mutate(Total = rowSums(select(.,-c(funding_agency, fiscal_year, indicator,
                                     ou_qc)),na.rm = TRUE),
         share = Local / Total) %>% 
  prinf()

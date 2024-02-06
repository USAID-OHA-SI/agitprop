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
library(readxl)
library(janitor)
library(lubridate)
library(googlesheets4)


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

#rbind together
df <- df_msd %>% 
  filter(!operatingunit %in% c("Nigeria", "Tanzania")) %>% 
  select(-c(qtr1:qtr4)) %>% 
  rbind(df_nga_new)

#initial MSD 
df_initial <- si_path() %>%
  return_latest("MER_Structured_Datasets_OU_IM_FY21-24_20231114_v1_1") %>%
  read_psd()

#check which variables in df_initial and not in df
#variables_not_in_df_initial <- setdiff(names(df), names(df_initial))
#variables_not_in_df <- setdiff(names(df_initial), names(df))

# Print the results
#cat("Variables in df but not in df_initial:", variables_not_in_df_initial, "\n")
#cat("Variables in df_initial but not in df:", variables_not_in_df, "\n")

#exclude extra columns and filter initial dataset to Tanzania
df_msd_TZA <- df_initial %>% 
  select(-c(qtr1:qtr4)) %>% 
  filter(operatingunit == "Tanzania")

#bind initial TZA data to clean MSD w/ NGA file
df_final <- df %>% 
  rbind(df_msd_TZA)


#Functions-----------------------------------------------------------------------
#clean number function
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)} billion"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)} million"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)} thousand"),
                   TRUE ~ glue("{x}"))
}

#local partners function
#catch-22/Scripts/20211102_Local Partner Meeting/20211022_LP_TX_trends.R
df_arch <- si_path() %>% 
  return_latest("OU_IM_FY15-20") %>% 
  read_msd()

#Read in the google sheet hyperfile with local partner
sheet_id <- "1MQviknJkJDttGdNEJeNaYPKmHCw6BqPuJ0C5cslV5IE"

df_partner <- read_sheet(sheet_id, sheet = "MechID-PartnerType", range = "A:B") %>% 
  clean_names() %>% 
  rename(mech_code = mechanism_id) %>% 
  mutate(mech_code = as.character(mech_code),
         partner_type = case_when(partner_type == "Regional" ~ "Local",
                                  partner_type == "TBD Local" ~ "Local", TRUE ~ partner_type)) 
#source info
#curr_pd <- identifypd(df_final)
#curr_fy <- identifypd(df_final, "year")
#msd_source <- source_info()

#result_type = value or share
grab_lp_results <- function(indic, result_type) {
  
  df_munge <- suppressMessages(df_final %>% 
                                 # bind_rows(df_arch) %>%
                                 left_join(df_partner, by = c("mech_code")) %>%
                                 filter(funding_agency == "USAID",
                                        indicator == indic,
                                        standardizeddisaggregate == "Total Numerator") %>% 
                                 group_by(funding_agency, fiscal_year, indicator, partner_type) %>% 
                                 summarise(across(cumulative, sum, na.rm = TRUE)) %>%
                                 ungroup() %>% 
                                 filter(partner_type != "TBD") %>%
                                 #filter(fiscal_year != 2022) %>%
                                 pivot_wider(names_from = partner_type, values_from = cumulative) %>%
                                 group_by(fiscal_year) %>%
                                 mutate(Total = International + Local,
                                        share = Local / Total)  %>%
                                 pivot_longer(cols = International:Total, names_to = "partner_type"))
  
  title_info_lp <- df_munge %>% 
    filter(partner_type == "Local", fiscal_year == metadata$curr_fy) %>% 
    select(fiscal_year, indicator, value, share) %>% 
    mutate(
      value = value, #change to 1 if you want 1 decimal accuracy
      share = percent(round(share, 2))) %>% 
    pull(result_type)
  
  return(title_info_lp)
}

#key pops function
pull_kp <- function(indic) {
  kp_val <- df_final %>%
    filter(funding_agency == "USAID",
           str_detect(standardizeddisaggregate, "KeyPop(?!\\/Status)"),
           indicator %in% c("PrEP_NEW", "TX_CURR", "KP_PREV"),
           fiscal_year == metadata$curr_fy) %>%
    count(indicator, wt = cumulative) %>%
    mutate(n = clean_number(n, 1)) %>% 
    pivot_wider(names_from = indicator, values_from = "n") %>% 
    pull(indic)
  
  return(kp_val)
}

#MUNGE

#Epi Control--------------------------------------------------------------------
df_vls_global <- df_final %>% 
  clean_indicator() %>% 
  clean_agency() %>% 
  clean_countries(colname = "country") %>% 
  filter(
    fiscal_year != metadata$curr_fy + 1,
    funding_agency == "USAID",
    indicator %in% c("TX_PVLS_D", "TX_PVLS"),
    standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")
  ) %>% 
  group_by(fiscal_year, funding_agency, country, indicator) %>% 
  summarise(across(cumulative, sum, na.rm = T), .groups = "drop") %>% 
  # reshape_msd() %>% 
  # select(-period_type) %>% 
  pivot_wider(names_from = indicator, values_from = cumulative) %>% 
  group_by(funding_agency, country) %>% 
  # mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period)) %>% 
  # relocate(TX_CURR_LAG2, .before = TX_CURR) %>% 
  filter(!is.na(TX_PVLS_D), TX_PVLS_D > 0) %>% 
  group_by(fiscal_year, funding_agency) %>% 
  summarise(across(starts_with("TX"), sum, na.rm = T), .groups = "drop") %>% 
  mutate(VLS = TX_PVLS/TX_PVLS_D)

df_tx_ou <- df_final %>% 
  clean_indicator() %>% 
  clean_agency() %>% 
  clean_countries(colname = "country") %>% 
  filter(
    fiscal_year != metadata$curr_fy + 1,
    funding_agency == "USAID",
    indicator %in% c("TX_PVLS_D", "TX_PVLS"),
    standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")
  ) %>% 
  group_by(fiscal_year, funding_agency, country, indicator) %>% 
  summarise(across(cumulative, sum, na.rm = T), .groups = "drop") %>% 
  # reshape_msd() %>% 
  # dplyr::select(-period_type) %>% 
  pivot_wider(names_from = indicator, values_from = cumulative) %>% 
  group_by(funding_agency, country) %>% 
  mutate(VLS = TX_PVLS/TX_PVLS_D) %>% 
  ungroup() %>%
  filter(!is.na(TX_PVLS_D), TX_PVLS_D > 0)

glbl_vls <<- df_vls_global %>% #TP.2
  filter(fiscal_year == metadata$curr_fy) %>%
  mutate(VLS_pct = percent(VLS)) %>%
  pull(VLS_pct)

meet_90_vls <<- df_tx_ou %>% #TP.2
  filter(fiscal_year == metadata$curr_fy, VLS >= .9) %>%
  distinct(country) %>% nrow()

meet_95_vls <<- df_tx_ou %>% #TP.2
  filter(fiscal_year == metadata$curr_fy, VLS >= .95) %>%
  distinct(country) %>% nrow()

#Prevention---------------------------------------------------------------------

#Prep
prep_val <- df_final %>% #TP.7
  filter(funding_agency == "USAID",
         standardizeddisaggregate %in% c("Total Numerator"),
         indicator %in% c("PrEP_NEW"),
         fiscal_year == metadata$curr_fy) %>%
  count(indicator, wt = cumulative)

prep_val_country <- df_final %>% #TP.7
  filter(funding_agency == "USAID",
         standardizeddisaggregate %in% c("Total Numerator"),
         indicator %in% c("PrEP_NEW"),
         fiscal_year == metadata$curr_fy) %>%
  distinct(country) %>% nrow()

prep_achv <- df_final %>% #TP.7
  filter(funding_agency == "USAID",
         standardizeddisaggregate %in% c("Total Numerator"),
         indicator %in% c("PrEP_NEW"),
         fiscal_year == metadata$curr_fy) %>%
  summarise(percent_achv = (sum(cumulative, na.rm = TRUE) / sum(targets, na.rm = TRUE))*100
            )

lp_prep_val <- grab_lp_results("PrEP_NEW", "value") #TP.8
lp_prep_share <- grab_lp_results("PrEP_NEW", "share") #TP.8

#kp prep
kp_prep_val <- pull_kp("PrEP_NEW") #TP.10

#AGYW prep
prep_agyw_val <- df_final %>% #TP.10
  filter(funding_agency == "USAID",
         standardizeddisaggregate %in% c("Age/Sex"),
         indicator %in% c("PrEP_NEW"),
         fiscal_year == metadata$curr_fy,
         sex == "Female",
         age_2019 %in% c("10-14","15-19", "20-24","25-29")
  ) %>%
  count(indicator, wt = cumulative) 

#vmmc
vmmc_val <- df_final %>% #TP.11
  filter(indicator == "VMMC_CIRC",
         standardizeddisaggregate == "Total Numerator",
         funding_agency == "USAID",
         fiscal_year == metadata$curr_fy) %>% 
  group_by(fiscal_year, funding_agency) %>% 
  summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
  ungroup() %>% 
  pull(cumulative)

vmmc_val_cntry <- df_final %>% #TP.11
  filter(indicator == "VMMC_CIRC",
         standardizeddisaggregate == "Total Numerator",
         funding_agency == "USAID",
         fiscal_year == metadata$curr_fy) %>% 
  group_by(fiscal_year, funding_agency) %>% 
  ungroup() %>% 
  distinct(country) %>% nrow()

vmmc_achv <- df_final %>% #TP.11
  filter(funding_agency == "USAID",
         standardizeddisaggregate == "Total Numerator",
         indicator %in% c("VMMC_CIRC"),
         fiscal_year == metadata$curr_fy) %>%
  summarise(percent_achv = (sum(cumulative, na.rm = TRUE) / sum(targets, na.rm = TRUE))*100
  )

agyw_prev_val <- df_final %>% #TP.11
  filter(indicator == "AGYW_PREV",
         standardizeddisaggregate %in% c("Total Numerator"),
         fiscal_year == metadata$curr_fy) %>% 
  summarise(sum(cumulative, na.rm = TRUE)
  )

agyw_prev_econ_val <- df_final %>% #TP.13
  filter(indicator=="AGYW_PREV", 
           standardizeddisaggregate %in% c("ComprehensiveEconomicStrengthening"),
           numeratordenom=="D",
           fiscal_year == metadata$curr_fy) %>% 
  summarise(sum(cumulative, na.rm = TRUE)
  )

#ovc
ovc_serv_val <- df_final %>% #TP.14
  filter(indicator %in% c("OVC_SERV"),
         standardizeddisaggregate %in% c("Total Numerator"),
         #  trendscoarse == "<18",
         fiscal_year == metadata$curr_fy,
         funding_agency == "USAID") %>% 
  group_by(fiscal_year, funding_agency) %>% 
  summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
  ungroup() %>% 
  mutate(cumulative = clean_number(cumulative, 2)) %>% 
  pull(cumulative)

ovc_serv_child_adolesc_val <- df_final %>% #TP.14
  filter(indicator %in% c("OVC_SERV"),
         standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex/Preventive", "Age/Sex/DREAMS"),
         trendscoarse == "<18",
         fiscal_year == metadata$curr_fy,
         funding_agency == "USAID") %>% 
  group_by(fiscal_year, funding_agency) %>% 
  summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
  ungroup() %>% 
  pull(cumulative)

ovc_serv_cntry <- df_final %>% #TP.14
  filter(indicator %in% c("OVC_SERV"),
         standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex/Preventive", "Age/Sex/DREAMS"),
         #trendscoarse == "<18",
         fiscal_year == metadata$curr_fy,
         funding_agency == "USAID") %>% 
  group_by(country, fiscal_year, funding_agency) %>% 
  summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
  ungroup() %>%
  distinct(country) %>% nrow()

#hts clean num
munge_hts_clean <- function(indic) {
  df_hts <- df_final %>% 
    filter(indicator == indic,
           standardizeddisaggregate == "Total Numerator") 
  
  df_hts <- suppressMessages(
    df_hts %>%
      bind_rows(df_hts %>% mutate(funding_agency = "PEPFAR")) %>%
      group_by(fiscal_year, funding_agency) %>%
      summarise(across(c(cumulative), sum, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(funding_agency %in% c("PEPFAR", "USAID")) %>%
      mutate(source = "MSD") %>%
      rename(value = cumulative) %>%
      pivot_wider(names_from = funding_agency, values_from = value) %>%
      group_by(fiscal_year) %>%
      mutate(share = USAID / PEPFAR) %>%
      pivot_longer(cols = PEPFAR:USAID, names_to = "funding_agency"))
  
  hts_val <- df_hts %>% 
    filter(fiscal_year == metadata$curr_fy,
           funding_agency == "USAID") %>% 
    select(fiscal_year, value, share) %>% 
    mutate(share = percent(round(share, 2)),
           value = round(value, 2),
           value_clean = value %>% clean_number()) %>%
    pull(value_clean)
  
  
  return(hts_val)
}

#hts complete num
munge_hts <- function(indic) {
  df_hts <- df_final %>% 
    filter(indicator == indic,
           standardizeddisaggregate == "Total Numerator") 
  
  df_hts <- suppressMessages(
    df_hts %>%
      bind_rows(df_hts %>% mutate(funding_agency = "PEPFAR")) %>%
      group_by(fiscal_year, funding_agency) %>%
      summarise(across(c(cumulative), sum, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(funding_agency %in% c("PEPFAR", "USAID")) %>%
      mutate(source = "MSD") %>%
      rename(value = cumulative) %>%
      pivot_wider(names_from = funding_agency, values_from = value) %>%
      group_by(fiscal_year) %>%
      mutate(share = USAID / PEPFAR) %>%
      pivot_longer(cols = PEPFAR:USAID, names_to = "funding_agency"))
  
  hts_val <- df_hts %>% 
    filter(fiscal_year == metadata$curr_fy,
           funding_agency == "USAID") %>% 
    select(fiscal_year, value, share) %>% 
    mutate(share = percent(round(share, 2)),
           value = round(value, 2)) %>% 
    pull(value)
  
  
  return(hts_val)
}
hts_val_clean <- munge_hts_clean("HTS_TST") #Testing and Diagnosis TP.18
hts_pos_val_clean <- munge_hts_clean("HTS_TST_POS") #Testing and Diagnosis TP.18
hts_self_val_clean <- munge_hts_clean("HTS_SELF") #Testing and Diagnosis TP.19

hts_val <- munge_hts("HTS_TST") #Testing and Diagnosis TP.18
hts_pos_val <- munge_hts("HTS_TST_POS") #Testing and Diagnosis TP.18
hts_self_val <- munge_hts("HTS_SELF") #Testing and Diagnosis TP.19

#youth testing 
df_create_youth <- df_final %>%
  mutate(ageasentered=case_when(
    ageasentered %in% c("10-14", "15-19", "20-24", "25-29")
    ~ "10-29",
    TRUE~ ageasentered))

df_youth <- df_create_youth %>%
  filter(ageasentered == '10-29')

munge_hts_youth <- function(indic) {
  df_hts <- df_youth %>% 
    filter(indicator == indic,
           standardizeddisaggregate == "Modality/Age/Sex/Result") 
  
  df_hts <- suppressMessages(
    df_hts %>%
      bind_rows(df_hts %>% mutate(funding_agency = "PEPFAR")) %>%
      group_by(fiscal_year, funding_agency) %>%
      summarise(across(c(cumulative), sum, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(funding_agency %in% c("PEPFAR", "USAID")) %>%
      mutate(source = "MSD") %>%
      rename(value = cumulative) %>%
      pivot_wider(names_from = funding_agency, values_from = value) %>%
      group_by(fiscal_year) %>%
      mutate(share = USAID / PEPFAR) %>%
      pivot_longer(cols = PEPFAR:USAID, names_to = "funding_agency"))
  
  hts_val <- df_hts %>% 
    filter(fiscal_year == metadata$curr_fy,
           funding_agency == "USAID") %>% 
    select(fiscal_year, value, share) %>% 
    mutate(share = percent(round(share, 2))) %>% 
    pull(value)
  
  
  return(hts_val)
}

hts_val_youth <- munge_hts_youth("HTS_TST") #Testing and diagnosis TP.20
hts_pos_val_youth <- munge_hts_youth("HTS_TST_POS") #Testing and diagnosis TP.20


hts_youth_achv <- df_youth %>% 
  filter(funding_agency == "USAID",
         standardizeddisaggregate %in% c("Modality/Age/Sex/Result"),
         indicator %in% c("HTS_TST"),
         fiscal_year == metadata$curr_fy) %>%
  summarise(percent_achv = (sum(cumulative, na.rm = TRUE) / sum(targets, na.rm = TRUE))*100
  )

hts_youth_pos_achv <- df_youth %>% 
  filter(funding_agency == "USAID",
         standardizeddisaggregate %in% c("Modality/Age/Sex/Result"),
         indicator %in% c("HTS_TST_POS"),
         fiscal_year == metadata$curr_fy) %>%
  summarise(percent_achv = (sum(cumulative, na.rm = TRUE) / sum(targets, na.rm = TRUE))*100
  )

hts_lp_value <- grab_lp_results("HTS_TST", "value")

#do we need this for Top TPS? 
#create a PEPFAR duplicate and aggregate up to country/global level
df_tx <- df %>%
  bind_rows(df %>% mutate(funding_agency = "PEPFAR")) %>% 
  filter(indicator %in% c("TX_CURR", "TX_NEW"),
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(fiscal_year, funding_agency, indicator) %>% 
  summarise(value = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(funding_agency %in% c("PEPFAR", "USAID"),
         value != 0) %>% 
  arrange(indicator, funding_agency, fiscal_year) %>% 
  mutate(source = "MSD")

df_tx <- df_tx %>% 
  filter(fiscal_year == metadata$curr_fy,
         indicator == "TX_NEW") %>%
  select(fiscal_year, indicator, funding_agency, value) %>% 
  pivot_wider(names_from = funding_agency) %>% 
  mutate(usaid_share = USAID/PEPFAR)  

tx_val <<- df_tx %>% 
  mutate(USAID_Lab = USAID %>% clean_number()) %>%
  pull(USAID_Lab) 


tx_lp_val <- grab_lp_results("TX_CURR", "value")
tx_lp_share <- grab_lp_results("TX_CURR", "share")
hts_lp_val <- grab_lp_results("HTS_TST", "value") #Testing and Diagnosis TP.21

#ovc - compare to ovc below? 

df_ovc_test <- df_final %>% 
  filter(funding_agency == "USAID",
         indicator %in% c("OVC_SERV"),
         standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex/DREAMS", "Age/Sex/Preventive"),
         #trendscoarse == "<18",
         fiscal_year <= metadata$curr_fy) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE))

df_ovc <- df_final %>% 
  clean_countries(colname = "country") %>% 
  filter(funding_agency == "USAID",
         indicator %in% c("OVC_SERV"),
         standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex/DREAMS", "Age/Sex/Preventive"),
         trendscoarse == "<18",
         fiscal_year <= metadata$curr_fy) %>% 
  bind_rows(df_final %>% filter(funding_agency == "USAID",
                               indicator %in% c("OVC_SERV"),
                               standardizeddisaggregate == "Age/Sex",
                               trendscoarse == "<18")) %>%
  group_by(mech_code) %>% 
  left_join(df_partner, by = c("mech_code")) %>%
  group_by(fiscal_year, funding_agency, indicator, partner_type) %>%
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(partner_type != "TBD") %>% 
  group_by(fiscal_year, indicator) %>% 
  mutate(total = sum(cumulative),
         share = cumulative / total) %>% 
  ungroup()

ovc_serv_country <<- df_ovc %>% #add country into grouping above to get number
  filter(fiscal_year == metadata$curr_fy) %>%
  distinct(country) %>% nrow()


## **Achievements related to those who are most disproportionately impacted by HIV**

kp_prev_val <- pull_kp("KP_PREV") #TP.28
kp_tx_val <- pull_kp("TX_CURR")

kp_tx_ou <- df_final %>%
  filter(funding_agency == "USAID",
         str_detect(standardizeddisaggregate, "KeyPop(?!\\/Status)"),
         indicator %in% c("TX_CURR"),
         fiscal_year == metadata$curr_fy) %>% 
  group_by(fiscal_year, funding_agency, country, indicator) %>% 
  summarise(across(cumulative, sum, na.rm = T), .groups = "drop") %>% 
  # reshape_msd() %>% 
  #filter(period == metadata$curr_pd) %>% 
  distinct(country) %>% 
  nrow() 








# agitprop/Scripts/archive/13_gend_gbv_BAN_map.R

# for now, assume this is GEND_GBV total numerator - check with Allison S. tomorrow

gbv_val <- df_final %>% #TP.32
  filter(indicator == "GEND_GBV",
         standardizeddisaggregate == "Total Numerator",
         funding_agency == "USAID",
         fiscal_year == metadata$curr_fy) %>% 
  group_by(fiscal_year, funding_agency) %>% 
  summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
  ungroup() %>% 
  pull(cumulative)

#local partner - GBV
gbv_lp_pct <- grab_lp_results("GEND_GBV", "share") #TP.32

#Dreams-------------------------------------------------------------------
# DREAMS DATA PREP --------------------------------------------------------------

# Google sheet ID for DREAMS DSNU list
dsnu_g_id <- "1YaGWvpkiXVPiAwA3KyHwFtbZHrVeCz6DijWPCKpUN-Q"

#MSD path
dsnu_msd_path <- si_path() %>% 
  return_latest("PSNU_IM_DREAMS_FY21-24")

#import DREAMS DSNU crosswalk
#recommend adding psnu_uids to this sheet
dsnu_list <- read_sheet(dsnu_g_id, sheet = "CURRENT (FY23/COP22)")

#rename names
names(dsnu_list)<-c("operatingunit", "psnu", "dsnu", "agencies_FY23")

#import msd and filter to current year
df_msd_dreams <- read_psd(dsnu_msd_path) %>% 
  filter(fiscal_year == metadata$curr_fy)

msd_dsnu_xwalk <- df_msd_dreams %>% 
  count(operatingunit, operatingunituid, psnu, psnuuid, cop22_psnu, cop22_psnuuid, dsnu, dsnuuid) %>% 
  mutate(raised_lvl = ifelse(psnuuid != cop22_psnuuid, TRUE, FALSE)) %>% 
  mutate(dsnu_new = case_when(operatingunit != "Uganda" & raised_lvl == TRUE ~ cop22_psnu,
                              TRUE ~ dsnu),
         dsnuuid_new = case_when(operatingunit != "Uganda" & raised_lvl == TRUE ~ cop22_psnuuid,
                                 TRUE ~ dsnuuid))

#join the new xwalk back to msd and then join the internal dsnu list to the msd
df_dreams_all <- df_msd_dreams %>% 
  left_join(msd_dsnu_xwalk) %>% 
  left_join(dsnu_list, by=c("operatingunit", "cop22_psnu" = "psnu", "dsnu_new"= "dsnu"))

#For total DREAMS programming, use all 4 disaggs + ages 10-29
viz_package <- df_dreams_all %>%
  filter(indicator=="AGYW_PREV",
         standardizeddisaggregate %in% c("Age/Sex/Time/Complete+",
                                         "Age/Sex/Time/Complete",
                                         "Age/Sex/Time/Started",
                                         "Age/Sex/Time/Incomplete"),
         numeratordenom=="D",
         age_2019 %in% c("10-14","15-19","20-24","25-29"),
         sex=="Female")

#service type disagg
viz_service<- df_dreams_all %>% filter(indicator=="AGYW_PREV", 
                                       standardizeddisaggregate %in% c("ComprehensiveEconomicStrengthening",
                                                                       "EducationSupport"),
                                       numeratordenom=="D")

#create shares by USAID and non-USAID contributing DSNUs
dreams_overall_val <- viz_package %>% #TP.12
  mutate(usaid_dsnu = ifelse(str_detect(agencies_FY23, "USAID"), "USAID", "non-USAID")) %>% 
  group_by(indicator, fiscal_year, usaid_dsnu) %>% 
  summarize(across(cumulative, \(x) sum(x, na.rm = TRUE)),.groups="drop") %>% 
  pivot_wider(names_from = "usaid_dsnu", values_from = 'cumulative') %>% 
  mutate(total = USAID + `non-USAID`,
         usaid_share = USAID/total) %>% 
  pull(USAID) %>% 
  clean_number(1)

## Numbers for education & econ boxes
## Filter for education and econ disaggs + USAID
df_serv_type <- viz_service %>% #TP.13
  filter(str_detect(agencies_FY23, "USAID")) %>%
  group_by(standardizeddisaggregate) %>%
  summarize(across(cumulative, \(x) sum(x, na.rm = TRUE)),.groups="drop") %>%
  pivot_wider(names_from = "standardizeddisaggregate", values_from = "cumulative") 

dream_econ_val <- df_serv_type %>% 
  pull(ComprehensiveEconomicStrengthening) %>% 
  clean_number(1)

dream_edu_val <- df_serv_type %>% 
  pull(EducationSupport) %>% 
  clean_number(1)
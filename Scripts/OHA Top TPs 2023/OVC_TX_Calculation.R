# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  check OVC TX Proxy values for OHA Top TPS
# REF ID:   304ed476 
# LICENSE:  MIT
# DATE:     2024-04-23
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(mindthegap)
library(googledrive)
library(googlesheets4)
library(gt)
library(gtExtras)


# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    get_metadata(filepath) 
  
  ref_id <- "304ed476"

# IMPORT ------------------------------------------------------------------


#Current MSD
filepath <- si_path() %>% 
  return_latest("MER_Structured_Datasets_PSNU_IM_FY22-24_20240315_v2_1.zip")

metadata_msd <- list(curr_pd = "FY23Q4",
                     curr_fy = 2023,
                     curr_fy_lab = "FY23",
                     curr_qtr = 4,
                     source = "FY23Q4c MSD",
                     caption = "Source: FY23Q4c MSD")

# MSD PREP -----------------------------------------------------------

#read MSD
df_msd_psnu <- read_psd(filepath)

#read NGA genie file
df_nga <- si_path() %>% 
  return_latest("All indicators_FY23Q4_Nigeria") %>% 
  read_csv()

#select only names in MSD
names_to_keep <- names(df_msd_psnu)
setdiff(names(df_msd_psnu), names(df_nga)) #only qtrs
setdiff(names(df_nga), names(df_msd_psnu))

#select all names from NGA file that match MSD (minus qtr1-qtr4)
df_nga_new <- df_nga %>% 
  select(any_of(names_to_keep))

#rbind together removing TZ and NGA for FY23 ONLY
df_psnu <- df_msd_psnu %>% 
  filter(!(operatingunit=="Nigeria" & fiscal_year=="2023")
         # ,
         # !(operatingunit=="Tanzania" & fiscal_year=="2023")
  ) %>% 
  select(-c(qtr1:qtr4)) %>% 
  rbind(df_nga_new) 


# MUNGE -----------------------------------------------------------------

#identify PSNUs that report OVC_SERV targets in FY23
df_ovc_serv <- df_psnu %>%
  clean_indicator() %>% 
  filter(indicator %in% c("OVC_SERV"),
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
         fiscal_year == metadata_msd$curr_fy) %>%
  group_by(fiscal_year, indicator, operatingunit, psnu) %>% 
  summarise(across(starts_with("targets"), sum, na.rm = T), .groups = "drop") %>% 
  pivot_wider(names_from = "indicator", values_from = "targets") %>% 
  filter(OVC_SERV > 0)

nga_psnus <- df_ovc_serv %>% 
  filter(operatingunit == "Nigeria") %>% 
  count(psnu) %>% pull(psnu)

all_other_psnus <- df_ovc_serv %>% 
  filter(operatingunit != "Nigeria") %>% 
  count(psnu) %>% pull(psnu)


# #get psnus for OVC_HIVSTAT_D for USAID
#   # FROM IVANA: we are using psnus by OVC_SERV targets reported across agencies
# df_ovc <- df_psnu %>%
#    clean_indicator() %>% 
#   filter(indicator %in% c("OVC_HIVSTAT_D"),
#          standardizeddisaggregate %in% c("Total Denominator"),
#          fiscal_year == metadata_msd$curr_fy,
#          funding_agency == "USAID") %>%
#    group_by(fiscal_year, indicator, operatingunit, psnu) %>% 
#    summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
#   pivot_wider(names_from = "indicator", values_from = "cumulative") %>% 
#   filter(OVC_HIVSTAT_D > 0)

#get psnus for PEPFAR that report TX_CURR <15 (nigeria will not be included here)
df_tx <- df_psnu %>%
  clean_indicator() %>% 
  filter(indicator %in% c("TX_CURR"),
         operatingunit != "Nigeria",
         psnu %in% all_other_psnus,
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus"),
         fiscal_year == metadata_msd$curr_fy,
         trendscoarse == "<15") %>%
  group_by(fiscal_year, indicator, trendscoarse, operatingunit, psnu) %>% 
  summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
  pivot_wider(names_from = "indicator", values_from = "cumulative") %>% 
  filter(TX_CURR > 0)
   
#  #now, check the PSNU overlap and only keep PSNUs where both report results
# ovc_tx_psnu <- df_tx %>% 
#   inner_join(df_ovc) %>% 
#   count(psnu) %>% 
#   pull(psnu)


#grab TX_CURR <15 for only these PSNUs
tx_u15_val <- df_tx %>% 
  filter(operatingunit != "Nigeria") %>% 
  mutate(ou_qc = case_when(operatingunit == "Nigeria" ~ "Nigeria",
                           operatingunit == "Tanzania" ~ "Tanzania",
                           TRUE ~ "All other OUs")) %>%
 group_by(ou_qc) %>% 
  mutate(total_tx_u15 = sum(TX_CURR)) %>% 
  ungroup() %>% 
  count(ou_qc, total_tx_u15)  %>% 
  select(-n) %>% 
  prinf()
  
#now, let's grab OVC_HIVSTAT_POS for the numerator (NGA will be in this one)
df_ovchivstat <- df_psnu %>%
  clean_indicator() %>% 
  filter(indicator == "OVC_HIVSTAT_POS",
         fiscal_year == metadata_msd$curr_fy,
         standardizeddisaggregate == "Age/Sex/ReportedStatus",
         target_age_2024 %in% c("<01","01-04", "05-09", "10-14")) %>% 
  mutate(ou_qc = case_when(operatingunit == "Nigeria" ~ "Nigeria",
                           operatingunit == "Tanzania" ~ "Tanzania",
                           TRUE ~ "All other OUs")) %>%
  group_by(fiscal_year, indicator, ou_qc) %>% 
  summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop")

# TABLES ---------------------------------------------------------------------

df_ovchivstat %>% 
  gt() %>% 
  fmt_number(columns = c(4), 
             decimals = 0) %>% 
  # fmt_percent(columns = 7, 
  #             decimals = 1) %>% 
  grand_summary_rows(
    columns = c(4),
    fns = list(
      Overall = ~ sum(., na.rm = T)
    ),
    formatter = fmt_number,
    decimals = 0
  ) %>% 
  tab_header(
    title = glue("TP #22: OVC HIVSTAT_POS <15" %>% toupper())) %>% 
  gt_theme_nytimes() 

tx_u15_val %>% 
  gt() %>% 
  fmt_number(columns = c(2), 
             decimals = 0) %>% 
  # fmt_percent(columns = 7, 
  #             decimals = 1) %>% 
  grand_summary_rows(
    columns = c(2),
    fns = list(
      Overall = ~ sum(., na.rm = T)
    ),
    formatter = fmt_number,
    decimals = 0
  ) %>% 
  tab_header(
    title = glue("TP #22: TX_CURR <15 in OVC PSNUs" %>% toupper()),
    subtitle = "Nigeria excluded") %>% 
  gt_theme_nytimes() 
  

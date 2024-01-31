# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  FY23Q4 MSD Join with Nigeria Genie Data
# REF ID:   54b9cbc1 
# LICENSE:  MIT
# DATE:     2023-11-28
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  
# FUNCTIONS --------------------------------------------------------------------

#clean number function
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)} billion"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)} million"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)} thousand"),
                   TRUE ~ glue("{x}"))
}

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # MSD
    filepath <- si_path() %>% 
      return_latest("MER_Structured_Datasets_OU_IM_FY21-24_20231114_v1_1")

  # Grab metadata
    get_metadata(filepath) 
  
  ref_id <- "54b9cbc1"

# IMPORT ------------------------------------------------------------------
  
  #read MSD
  df_msd <- read_psd(filepath)
    
  #read NGA genie file
  df_nga <- si_path() %>% 
    return_latest("All indicators_FY23Q4_Nigeria") %>% 
    read_csv()
  
  names_to_keep <- names(df_msd)
  setdiff(names(df_msd), names(df_nga)) #only qtrs
  setdiff(names(df_nga), names(df_msd))
  
  #select all names from NGA file that match MSD (minus qtr1-qtr4)
  df_nga_new <- df_nga %>% 
    select(any_of(names_to_keep))
  
#rbind together
  df <- df_msd %>% 
    filter(operatingunit != "Nigeria") %>% 
    select(-c(qtr1:qtr4)) %>% 
    rbind(df_nga_new)

# MUNGE -------------------------------------------------------------------
  
  df_vls_global <- df %>% 
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
  
  df_tx_ou <- df %>% 
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
  
  glbl_vls <<- df_vls_global %>%
    filter(fiscal_year == metadata$curr_fy) %>%
    mutate(VLS_pct = percent(VLS)) %>%
    pull(VLS_pct)
  
  meet_90_vls <<- df_tx_ou %>%
    filter(fiscal_year == metadata$curr_fy, VLS >= .9) %>%
    distinct(country) %>% nrow()
  
  meet_95_vls <<- df_tx_ou %>%
    filter(fiscal_year == metadata$curr_fy, VLS >= .95) %>%
    distinct(country) %>% nrow()
  
  
  munge_hts <- function(indic) {
    df_hts <- df %>% 
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
        mutate(share = USAID / PEPFAR)  %>%
        pivot_longer(cols = PEPFAR:USAID, names_to = "funding_agency"))
    
    hts_val <- df_hts %>% 
      filter(fiscal_year == metadata$curr_fy,
             funding_agency == "USAID") %>% 
      select(fiscal_year, value, share) %>% 
      mutate(share = percent(round(share, 2)),
             value = round(value, 2),
             value_clean = value %>%  clean_number()) %>%
      pull(value_clean)
    
    
    return(hts_val)
  }
  
  hts_val <- munge_hts("HTS_TST")
  hts_pos_val <- munge_hts("HTS_TST_POS")
  
  kp_tx_ou <- df %>%
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

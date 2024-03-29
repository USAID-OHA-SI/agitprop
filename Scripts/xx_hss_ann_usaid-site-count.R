# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  identify number of USAID sites reporting
# LICENSE:  MIT
# DATE:     2021-12-10
# UPDATED:  2022-01-03
# NOTE:     based on groundhogday/FY21Q4_pepfar-site-count.R

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr) #remotes::install_github("USAID-OHA-SI/glitr", build_vignettes = TRUE)
  library(glamr) #remotes::install_github("USAID-OHA-SI/glamr", build_vignettes = TRUE)
  library(janitor)

  source("Scripts/archive/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------

  #save DATIM credentials in OS password manager (one time)
  # set_datim()

  #store DATIM credentials in 
  load_secrets("datim")

  curr_fy <- source_info(return = "fiscal_year")
  datim_cy <- curr_fy - 1 
  
# DATIM API FUNCTION ------------------------------------------------------

  
  pull_sites <- function(ou_name, ou_uid, org_type, org_lvl,
                         fy_pd = 2022, 
                         username, password, 
                         baseurl = "https://final.datim.org/"){
    
    print(paste("Running DATIM API for", ou_name, org_type,  Sys.time(),
                sep = " ... "))
    
    cy_pd <- paste0(fy_pd-1, "Oct", collapse = ";")
    
    type_uid <- ifelse(org_type == "facility", "POHZmzofoVx", "PvuaP6YALSA") #excludes military & Other organisation unit type
    
    core_url <-
      paste0(baseurl,"api/29/analytics?",
             "dimension=pe:", cy_pd, "&", #period
             "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
             "dimension=bw8KHXzxd9i:NLV6dy7BE2O&", #Funding Agency - USAID
             "dimension=SH885jaRe0o&", #Funding Mechanism
             "dimension=LxhLO68FcXm:ELZsYsR89rn;CZplmfCbnv2;vw3VoiA4D0s;NYAJ6QkEKbC;Uo2vBxak9im;RxyNwEV3oQf;Fvs28dwjL6e;pkZRNlMgL89;gma5vVZgK49;FfxbuFZVAM5;wdoUps1qb3V;qOgXk080fJH;CUblPgOMGaT;twyHxdQVjMC;hGUykTtC0Xm;f5IPTM7mieH;lYTgCwEjUX6;cwZbCmUvjp7;R59aGLjmKBO;ECGbKy8o3FC;BTIqHnjeG7l;rI3JlpiuwEK;bybAqM1Lnba;AaCcy7dVfWw;Z6TU9Os82Yw;MvszPTQrUhy;cSTYDtvP0Nt;udCop657yzi;o8GCardEcYz;tOiM2uxcnkj;bZOF8bon1dD", #Technical Area
             "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets / Results - results
             "dimension=RUkVjD3BsS1&", #Top Level
             "dimension=mINJi7rR1a6:", type_uid,"&", #Type of organisational unit
             # "dimension=TWXpUVE2MqL&", #Support Type
             "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")
    
    df <- grabr::datim_process_query(core_url, username, password)
    
    if(!is.null(df)){
      df <- df %>%
        dplyr::filter(`Technical Area` %in% c("SC_CURR", "SC_ARVDISP", "HRH_PRE") | Value != 0) %>%
        dplyr::mutate(operatingunit = ifelse(stringr::str_detect(orglvl_3, "Region"), paste0(orglvl_3, "/", orglvl_4), orglvl_3)) %>%
        dplyr::distinct(operatingunit, orgunituid, sitetype = `Type of organisational unit`)
    }
    
    return(df)
  }
  

  
# IDENTIFY INPUTS FOR API -------------------------------------------------
  
  #country and level list
  ctry_list <- grabr::get_outable(datim_user(), datim_pwd()) %>% 
    select(country, country_uid, 
           community = community_lvl, facility = facility_lvl) %>% 
    pivot_longer(c(community, facility),
                 names_to = "type",
                 values_to = "level") 
  

# RUN API -----------------------------------------------------------------


  df_sites <- ctry_list %>% 
    pmap_dfr(~pull_sites(..1, ..2, ..3, ..4, 
                         username = datim_user(), password = datim_pwd()))
  
  df_tx <- ctry_list %>% 
    filter(type == "facility") %>% 
    mutate(type = "TX") %>% 
    select(country, country_uid, level, type) %>% 
    pmap_dfr(~ query_datim(..1, ..2, ..3, ..4, datim_user(), datim_pwd()))  
  
  df_labs <- ctry_list %>% 
    filter(type == "facility") %>% 
    mutate(type = "LAB") %>% 
    select(country, country_uid, level, type) %>% 
    pmap_dfr(~ query_datim(..1, ..2, ..3, ..4, datim_user(), datim_pwd()))  

  df_hts <- ctry_list %>% 
    filter(type == "facility") %>% 
    mutate(type = "HTS") %>% 
    select(country, country_uid, level, type) %>% 
    pmap_dfr(~ query_datim(..1, ..2, ..3, ..4, datim_user(), datim_pwd()))
  
# SITE COUNT --------------------------------------------------------------

  df_sites %>% 
    distinct(orgunituid) %>% 
    count()
 
  df_labs %>% 
    distinct(orgunituid) %>% 
    count()
  
  df_hts %>% 
    distinct(orgunituid) %>% 
    count()
  
  df_tx %>% 
    distinct(orgunituid) %>% 
    count()

# EXPORT ------------------------------------------------------------------

  write_csv(df_sites, "Dataout/FY21Q4i_PEPFAR-sites-and-types.csv", na = "")
    
# PREP WORK ---------------------------------------------------------------

  # #pull list of DATIM dimensions for API
  # datim_dimensions() %>%
  #   arrange(dimension) %>%
  #   prinf()
  # 
  # #pull items from dimensions to add to API
  # datim_dim_items("Support Type")
  # 
  # #all indicator uids
  # df_ind_datim <- datim_dim_items("Technical Area")
  # 
  # #indicators from MER 2.5
  # df_ind <- tribble(
  #                  ~category,      ~indicator,
  #               "Prevention",     "AGYW_PREV",
  #               "Prevention",    "FPINT_SITE",
  #               "Prevention",      "GEND_GBV",
  #               "Prevention",        "KP_MAT",
  #               "Prevention",       "KP_PREV",
  #               "Prevention",      "OVC_SERV",
  #               "Prevention",       "PP_PREV",
  #               "Prevention",     "PrEP_CURR",
  #               "Prevention",      "PrEP_NEW",
  #               "Prevention",       "TB_PREV",
  #               "Prevention",     "VMMC_CIRC",
  #                  "Testing",     "CXCA_SCRN",
  #                  "Testing",     "HTS_INDEX",
  #                  "Testing",    "HTS_RECENT",
  #                  "Testing",      "HTS_SELF",
  #                  "Testing",       "HTS_TST",
  #                  "Testing",   "OVC_HIVSTAT",
  #                  "Testing",     "PMTCT_EID",
  #                  "Testing",      "PMTCT_FO",
  #                  "Testing", "PMTCT_HEI_POS",
  #                  "Testing",    "PMTCT_STAT",
  #                  "Testing",       "TB_STAT",
  #                "Treatment",       "CXCA_TX",
  #                "Treatment",     "PMTCT_ART",
  #                "Treatment",        "TB_ART",
  #                "Treatment",       "TX_CURR",
  #                "Treatment",         "TX_ML",
  #                "Treatment",        "TX_NEW",
  #                "Treatment",        "TX_RTT",
  #                "Treatment",         "TX_TB",
  #   "Viral Load Suppression",       "TX_PVLS",
  #           # "Health Systems",      "EMR_SITE",
  #           # "Health Systems",       "HRH_PRE",
  #           # "Health Systems",     "LAB_PTCQI",
  #           # "Health Systems",    "SC_ARVDISP",
  #           # "Health Systems",       "SC_CURR"
  #   )
  # 
  # #join indicators with %>%
  # lst_ind_uid <- df_ind %>%
  #   left_join(df_ind_datim, by = c("indicator" = "item")) %>%
  #   pull() %>%
  #   paste0(collapse = ";")


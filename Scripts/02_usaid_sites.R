# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  site count of USAID supported facilities
# LICENSE:  MIT
# DATE:     2021-05-11
# UPDATED:  2021-05-13
# NOTE:     adapted from USAID-OHA-SI/findyourbeach (linked below)
# URL:      https://github.com/USAID-OHA-SI/find_your_beach/blob/master/Scripts/01_query_datim.R

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(ICPIutilities)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(Wavelength)
  
  source("Scripts/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()

  authors <- c("Aaron Chafetz")
  
  curr_fy <- 2021
  datim_cy <- curr_fy - 1 


# PULL DATA ---------------------------------------------------------------
  
  #uids and levels for each country
  ctry_list <- get_outable(datim_user(), datim_pwd()) %>% 
    select(operatingunit, operatingunit_uid, countryname, 
           psnu_lvl = prioritization, facility_lvl)
  
  #expand each country by the API type for pmap
  full_list <- expand_grid(countryname = ctry_list$countryname,
              type = c("TX", "HTS", "LAB")) %>% 
    left_join(ctry_list)
  
  #run API across all countries
  df_full <- full_list %>% 
    select(operatingunit_uid, facility_lvl, type) %>% 
    pmap_dfr(~ query_datim(..1, ..2, ..3, datim_user(), datim_pwd()))

  
# PULL COORDINATES --------------------------------------------------------
  
  #pull hierarchy
  df_orgs <- map_dfr(.x = unique(ctry_list$operatingunit_uid),
                     .f = ~ pull_hierarchy(.x, datim_user(), datim_pwd())) 
  
  
# MUNGE AND APPEND DATA ---------------------------------------------------
  
  #combine all technical areas into HTS (modalities are derived from VMMC, TB, PMTCT, INDEX)
  df_full_clean <- df_full %>% 
    mutate(`Technical Area` = ifelse(`Technical Area` %in% c("HTS_INDEX", "PMTCT_STAT",
                                                             "TB_STAT", "VMMC_CIRC"), "HTS_TST", `Technical Area`))
  
  #remove disaggs from lab
  df_full_clean <- df_full_clean %>% 
    filter(`Disaggregation Type` %in% c(NA, "POCT/TestVolume", "Lab/TestVolume")) %>% 
    select(-`Disaggregation Type`)
  
  #aggregate
  df_full_clean <- df_full_clean %>% 
    select(-`Disaggregation Type`) %>% 
    group_by_if(is.character) %>% 
    summarise_if(is.double, sum, na.rm = TRUE) %>% 
    ungroup()

  
  #limit output variables
  df_sel <- df_full_clean %>% 
    select(fundingagency = `Funding Agency`,
           resulttarget = `Targets / Results`,
           indicator = `Technical Area`,
           period = Period,
           orgunituid, 
           region = orglvl_2,
           orglvl_3, orglvl_4,
           value = Value)
  
  #merge with hierarchy/coordinates
  df_sites <- left_join(df_sel, df_orgs)
  
  #add iso codes
  df_sites <- df_sites %>% 
    left_join(iso_map, by = c("countryname" = "operatingunit"))  %>% 
    select(-regional) %>% 
    select(countryname, iso, everything())
  
  #has coordinates?
  df_sites <- df_sites %>% 
    mutate(has_coordinates = !is.na(latitude))
  
  #store API dataset
  outputfile <- "Dataout/02_USAID_sites_SBU.csv"
  write_csv(df_sites, outputfile, na = "")
  

# PREP FOR VIZ ------------------------------------------------------------
  
  #read in API file (so you can start here instead of reruning full API)
  df_sites <- read_csv(outputfile)
  
  #date of API
  api_date <- outputfile %>% 
    file.info() %>% 
    pull(ctime) %>% 
    format("%Y-%m-%d")
  
  df_kpi <- df_sites %>% 
    group_by(indicator) %>% 
    summarise(distinct_facilities = n_distinct(orgunituid, na.rm = TRUE),
              distinct_psnus = n_distinct(psnuuid, na.rm = TRUE)) %>% 
    ungroup() 
    
  df_viz <- df_kpi %>% 
    mutate(x = .5,
           y = x,
           x_label = .5,
           y_label = .75,
           x_sub = .72,
           y_sub = .3,
           indicator = factor(indicator, c("HTS_TST", "TX_CURR", "LAB_PTCQI")),
           ind_display = case_when(indicator == "HTS_TST" ~ "HIV testing services",
                                   indicator == "TX_CURR" ~ "Providing antiretrovial treatment",
                                   indicator == "LAB_PTCQI" ~ "Laboratory-based and/or point-of-care testing"))


# VIZ ---------------------------------------------------------------------

  df_viz %>% 
    ggplot(aes(x, y)) +
    geom_text(aes(label = comma(distinct_facilities)),
              family = "Source Sans Pro Light", color = moody_blue,
              size = 60/.pt) +
    geom_text(aes(x_label, y_label, label = ind_display),
              family = "Source Sans Pro Light", color = trolley_grey, 
              size = 11/.pt) +
    geom_text(aes(x_sub, y_sub, label = "sites"),
              family = "Source Sans Pro Light", color = trolley_grey, 
              size = 11/.pt) +
    expand_limits(x = c(0, 1), y = c(0,1)) +
    facet_grid(~indicator) +
    labs(x = NULL, y = NULL,
         title = "WHERE DOES USAID WORK?",
         caption = glue("Source: DATIM API [{api_date}]
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_blank(),
          panel.background = element_rect(fill = "#e6e7e84f"),
          panel.border = element_rect(color = trolley_grey, fill = NA))
  
  si_save("Images/02_usaid_facilities.png",
          scale = 1.2, width = 10, height = 3) 

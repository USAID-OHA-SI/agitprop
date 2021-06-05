# PROJECT:  agitprop
# AUTHOR:   T. Essam | USAID
# PURPOSE:  USAID pmctc_eid calculations (children born hiv-free)
# LICENSE:  MIT
# DATE:     2021-05-26
# UPDATED: 

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
  
  source("Scripts/99_utilities.R")


# GLOBAL VARIABLES --------------------------------------------------------

  authors <- c("Aaron Chafetz", "Tim Essam")

# IMPORT ------------------------------------------------------------------

  #Current MSD
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd()   
  
  #Archived MSD
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()
  
  msd_source <- df %>% 
    identifypd() %>% 
    msd_period(period = .)
  
  
# MUNGE -------------------------------------------------------------------

  hiv_free_19_21 <- 
    df %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("PMTCT_EID", "PMTCT_HEI_POS"),
           standardizeddisaggregate %in% c("Total Numerator")) %>% 
    mutate(indicator = case_when(numeratordenom == "D" ~ glue("{indicator}_D"),
                                 TRUE ~ indicator)) %>%
    group_by(fiscal_year, indicator,) %>% 
    summarise(across(c(cumulative), sum, na.rm = TRUE), .groups = "drop") %>% 
    spread(indicator, cumulative) %>% 
    mutate(hiv_free = PMTCT_EID - PMTCT_HEI_POS)

  hiv_free_18 <- 
    df_arch %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("PMTCT_EID", "PMTCT_HEI_POS"),
           standardizeddisaggregate %in% c("Total Numerator")) %>% 
    mutate(indicator = case_when(numeratordenom == "D" ~ glue("{indicator}_D"),
                                 TRUE ~ indicator)) %>%
    group_by(fiscal_year, indicator,) %>% 
    summarise(across(c(cumulative), sum, na.rm = TRUE), .groups = "drop") %>% 
    spread(indicator, cumulative) %>% 
    mutate(hiv_free = PMTCT_EID - PMTCT_HEI_POS)
  
  hiv_free_all <- 
    rbind(hiv_free_18, hiv_free_19_21) %>% 
    mutate(tot_hiv_free = sum(hiv_free, na.rm = T),
           tot_pmtct_eid = sum(PMTCT_EID), na.rm = T) %>% 
    filter(fiscal_year >= 2018) %>% 
    mutate(fill_col = if_else(fiscal_year == 2021, 
                              "#419fbe", "#005e7a")) 
  
  total_count <- 
    hiv_free_all %>% 
    summarise(tot = sum(hiv_free, na.rm = T)) %>% 
    pull()/1e6
  
  hiv_free_all %>% 
    ggplot(aes(x = fiscal_year)) +
    geom_col(aes(y = PMTCT_EID), fill = trolley_grey_light) +
    geom_col(aes(y = hiv_free, fill = fill_col), alpha = 1) +
    geom_hline(yintercept = seq(1e5, 4e5, by = 1e5), 
               color = "white", 
               size = 0.15,
               linetype = "dotted") +
    geom_col(data = . %>% filter(fiscal_year == 2021), aes(y = tot_hiv_free, x = 2022), fill = "#004964") +
    geom_vline(xintercept = 2021.5, size = 0.5, linetype = "dotted", color = grey50k)+
    geom_text(aes(y = hiv_free, label = comma(hiv_free)), size = 12/.pt,
              family = "Source Sans Pro SemiBold", vjust = 1.5, color = "white") +
    geom_text(data = . %>% filter(fiscal_year == 2021), aes(y = tot_hiv_free, x = 2022, label = comma(tot_hiv_free)), size = 12/.pt,
              family = "Source Sans Pro SemiBold", vjust = 1.5, color = "white") +
      si_style_xline() +
      scale_fill_identity() +
    scale_x_continuous(labels = c("2018", "2019", "2020", "2021 (through Q2)", "2018-2021"))+
    coord_cartesian(expand = F) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("USAID SUPPORT HAS ENABLED {round(total_count, 2)} MILLION BABIES TO BE BORN HIV-FREE TO MOTHERS LIVING WITH HIV"),
         caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    theme(axis.text.y = element_blank())

  si_save("Images/12_hiv_free_babies_v2.png", scale = 1.1)    
    
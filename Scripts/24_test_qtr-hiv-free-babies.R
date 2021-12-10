# PROJECT:  agitprop
# AUTHOR:   T. Essam | USAID
# PURPOSE:  USAID pmctc_eid calculations (children born hiv-free)
# DATE:     2021-05-26
# UPDATED:  2021-12-09

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


# GLOBAL VARIABLES --------------------------------------------------------

  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  
  msd_source <- source_info()
  
  curr_fy <- source_info(return = "fiscal_year")
  curr_qtr <- source_info(return = "quarter")
  
  
  x_breaks <- seq(2015, curr_fy)
  
  x_labs <- if(curr_qtr == 4){
    seq(2015, curr_fy) %>% 
      as.character()
  } else {
    seq(2015, curr_fy-1) %>% 
      c(., glue("{curr_fy} (through Q{curr_qtr})"))
  }

# IMPORT ------------------------------------------------------------------

  #Current MSD
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd()   
  
  #Archived MSD
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()
  

# MUNGE -------------------------------------------------------------------

  hiv_free_19_21 <- 
    df %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("PMTCT_EID", "PMTCT_HEI_POS"),
           standardizeddisaggregate %in% c("Total Numerator")) %>% 
    clean_indicator() %>% 
    group_by(fiscal_year, indicator,) %>% 
    summarise(across(c(cumulative), sum, na.rm = TRUE), .groups = "drop") %>% 
    spread(indicator, cumulative) %>% 
    mutate(hiv_free = PMTCT_EID - PMTCT_HEI_POS)

  hiv_free_15_18 <- 
    df_arch %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("PMTCT_EID", "PMTCT_HEI_POS", "PMTCT_EID_POS"),
           standardizeddisaggregate %in% c("Total Numerator")) %>% 
    clean_indicator() %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(c(cumulative), sum, na.rm = TRUE), .groups = "drop") %>% 
    spread(indicator, cumulative) %>% 
    rowwise() %>% 
    mutate(hiv_free = PMTCT_EID - sum(PMTCT_HEI_POS, PMTCT_EID_POS, na.rm = TRUE)) %>% 
    ungroup()
  
  hiv_free_all <- 
    bind_rows(hiv_free_15_18, hiv_free_19_21) %>% 
    mutate(tot_hiv_free = sum(hiv_free, na.rm = T),
           tot_pmtct_eid = sum(PMTCT_EID), na.rm = T) %>% 
    mutate(fill_col = if_else(fiscal_year == curr_fy, 
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
    geom_text(aes(y = hiv_free, label = comma(hiv_free)), size = 12/.pt,
              family = "Source Sans Pro SemiBold", vjust = 1.5, color = "white") +
    si_style_xline() +
    scale_fill_identity() +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_labs) +
    coord_cartesian(expand = F) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("USAID SUPPORT HAS ENABLED {round(total_count, 2)} MILLION BABIES TO BE BORN HIV-FREE TO MOTHERS LIVING WITH HIV"),
         caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    theme(axis.text.y = element_blank())

  si_save("Images/12_hiv_free_babies_v1.png", scale = 1.1)    
  
  
  min_yr <- hiv_free_all %>% filter(fiscal_year == min(fiscal_year)) %>% pull(fiscal_year)
  
  x_breaks_w_tot <- seq(2015, curr_fy + 1)
  x_labs_w_tot <- if(curr_qtr == 4){
    seq(2015, curr_fy) %>% 
    c(., glue("2015-{curr_fy}"))
  } else {
    seq(2015, curr_fy-1) %>% 
    c(., glue("{curr_fy} (through Q{curr_qtr})"), glue("2015-{curr_fy}"))
  }
  
  hiv_free_all %>% 
    ggplot(aes(x = fiscal_year)) +
    geom_col(aes(y = cumsum(hiv_free)), fill = trolley_grey_light, alpha = 0.75) +
    geom_col(aes(y = hiv_free), fill = scooter, alpha = 1) +
    geom_col(data = . %>% filter(fiscal_year == curr_fy), aes(y = tot_hiv_free, x = curr_fy + 1), fill = "#004964") +
    geom_hline(yintercept = seq(1e6, 2e6, by = 1e6), 
               color = "white", 
               size = 1,
               linetype = "dotted") +
    geom_vline(xintercept = curr_fy + 0.5, size = 0.5, linetype = "dotted", color = grey50k)+
    geom_text(aes(y = hiv_free, label = comma(hiv_free)), size = 12/.pt,
              family = "Source Sans Pro SemiBold", vjust = 1.5, color = "white") +
    geom_text(data = . %>% filter(fiscal_year == curr_fy), aes(y = tot_hiv_free, x = curr_fy +1, label = comma(tot_hiv_free)), size = 12/.pt,
              family = "Source Sans Pro SemiBold", vjust = 1.5, color = "white") +
    si_style_xline() +
    scale_fill_identity() +
    scale_x_continuous(breaks = x_breaks_w_tot,
                       labels = x_labs_w_tot) +
    coord_cartesian(expand = F) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("SINCE {min_yr}, USAID SUPPORT HAS ENABLED {round(total_count, 2)} MILLION BABIES TO BE BORN HIV-FREE TO MOTHERS LIVING WITH HIV"),
         subtitle = glue("Cumulative results depicted by grey bars"),
         caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    theme(axis.text.y = element_blank())
  
  si_save("Images/12_hiv_free_babies_v2.png", scale = 1.1)
  

# CUMULATIVE? -------------------------------------------------------------

    
    
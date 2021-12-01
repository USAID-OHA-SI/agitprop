# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  scale up of prep for KP
# LICENSE:  MIT
# DATE:     2021-12-01
# UPDATED:  
# NOTE:     adapted from agitprop/09a_usaid_prep_scaleup.R

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
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Karisham Srikanth")
  
  msd_source <- source_info()
  curr_fy <- source_info(return = "fiscal_year")
  curr_pd <- source_info(return = "period")
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd()   
  
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()

# MUNGE -------------------------------------------------------------------

  #bind archived + current MSD and filter for PrEP
  df_prep <- df %>%
    bind_rows(df_arch) %>% 
    filter(fundingagency == "USAID",
           indicator == "PrEP_NEW",
           standardizeddisaggregate %in% c("KeyPopAbr", "Total Numerator"),
           fiscal_year >= 2017)
  
  #curr fy prep (for viz title)
  prep_curr <- df_prep %>% 
    filter(fiscal_year == curr_fy) %>% 
    count(standardizeddisaggregate, wt = cumulative) %>%
    pivot_wider(names_from = standardizeddisaggregate, values_from = "n") %>% 
    mutate(share = KeyPopAbr / `Total Numerator`)
  
  #count number of countries with PrEP
  df_cntry_cnt <- df_prep %>% 
    filter(cumulative != 0,
           standardizeddisaggregate =="KeyPopAbr") %>% 
    distinct(fiscal_year, countryname, standardizeddisaggregate) %>% 
    count(fiscal_year, standardizeddisaggregate, name = "n_countries")
  
  #aggregate result to USAID level
  df_prep <- df_prep %>% 
    group_by(fiscal_year, fundingagency, standardizeddisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd() %>% 
    mutate(value = na_if(value, 0)) %>% 
    select(-period_type) %>% 
    arrange(period) %>% 
    pivot_wider(names_from = standardizeddisaggregate,
                names_glue = "{str_remove(standardizeddisaggregate, ' ')}"
                )
  
# VIZ ---------------------------------------------------------------------
  

  pd_breaks <- df_prep %>%
    distinct(period) %>% 
    # filter(str_detect(period, "Q(1|3)")) %>% 
    pull()
  
  df_viz <- df_prep %>%
    mutate(fill_alpha = ifelse(str_detect(period, "FY21"), 1, .6))
  
  v <- df_viz %>% 
    ggplot(aes(period, KeyPopAbr, group = fundingagency, alpha = fill_alpha)) + 
    geom_col(fill = scooter, na.rm = TRUE) +
    scale_y_continuous(label = label_number_si(), position = "right", expand = c(.01, .01)) +
    scale_x_discrete(breaks = pd_breaks, labels = str_remove(pd_breaks, "FY[:digit:]{2}(?!Q1)")) +
    scale_alpha_identity() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL, 
         title = glue("USAID has scaled up PrEP initiation to Key Populations \\
                      adding {label_number_si()(prep_curr$KeyPopAbr)} in FY{str_sub(curr_fy, -2)}"
                      ) %>% toupper,
         subtitle = "Key Population Pre-Exposure Prophylaxis (PrEP) Quarterly Results",
         caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid()
    

  v +
    annotate("text",
             x = 12.5, y = 27e3, 
             hjust = "center", lineheight = .9,
             label = "PrEP reported semi-annually in \nFY19-20, so Q1/Q3 were reported\n in aggregate with Q2/Q4 reporting",
             family = "Source Sans Pro", size = 9/.pt, color = matterhorn)
   
  
  si_save("Graphics/13_prev_qtr_prep-usaid-scaleup-kp.svg")  
  
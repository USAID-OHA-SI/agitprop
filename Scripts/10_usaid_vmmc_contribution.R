# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  tracking VMMC contributions
# LICENSE:  MIT
# DATE:     2021-05-20
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
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   

# MUNGE -------------------------------------------------------------------

  df_vmmc <- df %>% 
    filter(indicator == "VMMC_CIRC",
           standardizeddisaggregate == "Total Numerator")
  
  df_vmmc <- df_vmmc %>% 
    bind_rows(df_vmmc %>% mutate(fundingagency = "PEPFAR")) %>% 
    group_by(fiscal_year, fundingagency) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd() %>% 
    select(-period_type)
  
  df_vmmc <- df_vmmc %>% 
    filter(fundingagency %in% c("USAID", "PEPFAR")) %>% 
    group_by(period) %>% 
    mutate(share = case_when(fundingagency == "USAID" ~ value/lag(value)),
           total = case_when(fundingagency == "PEPFAR" ~ value)) %>% 
    ungroup()
  

  last_4q <- df_vmmc %>% 
    select(-share, -total) %>% 
    pivot_wider(names_from = fundingagency) %>% 
    slice_max(period, n = 4) %>% 
    summarise(across(c(PEPFAR, USAID), sum, na.rm = TRUE)) %>% 
    mutate(share = USAID/PEPFAR)
  
# VIZ ---------------------------------------------------------------------
  
  msd_source <- df %>% 
    identifypd() %>% 
    msd_period(period = .)
  
  df_vmmc %>% 
    ggplot(aes(period, value)) + 
    geom_col(aes(fill = fundingagency, alpha = fundingagency), position = "identity") +
    geom_text(aes(y = 30000, label = percent(share, 1)), na.rm = TRUE, 
              family = "Source Sans Pro SemiBold", color = "white") +
    geom_errorbar(aes(ymin = total, ymax = total), color = trolley_grey, 
                  size = .9, na.rm = TRUE) +
    scale_fill_manual(values = c("USAID" = moody_blue, "PEPFAR" = trolley_grey_light)) +
    scale_alpha_manual(values = c("USAID" = .8, "PEPFAR" = .4)) +
    scale_y_continuous(label = comma, position = "right", expand = c(.005, .005)) +
    labs(x = NULL, y = NULL, fill = NULL, alpha = NULL,
         title = glue("USAID HAS CONTRIBUTED {percent(last_4q$share,1)} OF THE {clean_number(last_4q$PEPFAR, 1) %>% toupper} PEPFAR VOLUNTARY MALE CIRCUMCISIONS IN THE LAST 4 QUARTERS"),
         caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid()

  si_save("Images/10_vmmc_contribution.png")  
  
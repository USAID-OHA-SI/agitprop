# PROJECT:  agiprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  prep share of pepfar PEPFAR start
# LICENSE:  MIT
# DATE:     2021-06-29
# UPDATED:  2021-08-23

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
  library(janitor)
  library(lubridate)

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

# MUNGE -------------------------------------------------------------------

  #source info
  msd_source <- source_info()
  
  curr_qtr <- identifypd(df, "quarter")
  curr_fy <- df %>% 
    identifypd() %>% 
    str_sub(end = 4)
  
  df_prep <- df %>% 
    bind_rows(df_arch) %>% 
    filter(indicator == "PrEP_NEW",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year >= 2017) 
  
  df_prep <- df_prep %>% 
    bind_rows(df_prep %>% mutate(fundingagency = "PEPFAR")) %>%
    group_by(fiscal_year, fundingagency, indicator) %>% 
    summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(fundingagency %in% c("USAID", "PEPFAR"))
  
  df_prep <- df_prep %>% 
    rename(period = fiscal_year, value = cumulative) %>% 
    mutate(period = str_replace(period, "20", "FY")) %>% 
    arrange(indicator, period) %>% 
    mutate(source = "MSD")

  df_prep <- df_prep %>% 
    group_by(period) %>% 
    mutate(share = value/lag(value)) %>% 
    ungroup()
  
  df_viz <- df_prep %>% 
    mutate(bar_alpha = case_when(fundingagency == "PEPFAR" ~ .6,
                                 period == curr_fy & curr_qtr != 4 ~ .4,
                                 TRUE ~ 1),
           year = glue("20{str_sub(period, 3, 4)}") %>% as.integer,
           total = case_when(fundingagency == "PEPFAR" ~ value))
  
  
  
  df_viz %>%
    ggplot(aes(year, value, fill = fundingagency, alpha = bar_alpha)) +
    geom_col(position = "identity") +
    geom_errorbar(aes(ymin = total, ymax = total), color = trolley_grey_light, 
                  size = .9, na.rm = TRUE) +
    geom_text(aes(label = percent(share, 1)), alpha = 1, na.rm = TRUE, vjust = -1, 
              family = "Source Sans Pro SemiBold", color = trolley_grey) +
    scale_y_continuous(labels = label_number_si(),
                       position = "right", expand = c(.005, .005)) +
    scale_x_continuous(expand = c(.005, .005), n.breaks = unique(df_viz$period) %>% length())+
    scale_alpha_identity() +
    scale_fill_manual(values = c("PEPFAR" = trolley_grey_light,"USAID" = genoa)) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = str_wrap("AS PEPFAR HAS SCRALED UP PREP INITIATIONS, USAID HAS SCALED PROPORTIONALLY", 100),
         subtitle = "PEPFAR Newly Enrolled on Pre-Exposure Prophylaxis (PrEP)",
         caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid()

  si_save("Graphics/09b_prep_share_usaid.svg")


  
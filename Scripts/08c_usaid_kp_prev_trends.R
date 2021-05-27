# PROJECT:  agiprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  treatment scale up since PEPFAR start
# LICENSE:  MIT
# DATE:     2021-05-25
# UPDATED:  2021-05-26

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
  msd_source <- df %>% 
    identifypd() %>% 
    msd_period(period = .)
  
  curr_qtr <- identifypd(df, "quarter")
  curr_fy <- df %>% 
    identifypd() %>% 
    str_sub(end = 4)
  
  df_kp <- df %>% 
    bind_rows(df_arch) %>% 
    filter(indicator == "KP_PREV",
           standardizeddisaggregate == "Total Numerator") 
  
  df_kp <- df_kp %>% 
    bind_rows(df_kp %>% mutate(fundingagency = "PEPFAR")) %>%
    group_by(fiscal_year, fundingagency, indicator) %>% 
    summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(fundingagency %in% c("USAID", "PEPFAR"))
  
  df_kp <- df_kp %>% 
    reshape_msd() %>% 
    arrange(indicator, period) %>% 
    select(-period_type) %>% 
    mutate(source = "MSD")

  df_kp <- df_kp %>% 
    group_by(period) %>% 
    mutate(share = value/lag(value)) %>% 
    ungroup()
  
  df_viz <- df_kp %>% 
    mutate(bar_alpha = case_when(fundingagency == "PEPFAR" ~ .6,
                                 period == curr_fy & curr_qtr != 4 ~ .4,
                                 TRUE ~ 1),
           year = glue("20{str_sub(period, 3, 4)}") %>% as.integer,
           ind_label = "Key Pop Prevention Services",
           total = case_when(fundingagency == "PEPFAR" ~ value))
  
  
  
  df_viz %>%
    ggplot(aes(year, value, fill = fundingagency, alpha = bar_alpha)) +
    geom_col(position = "identity") +
    geom_errorbar(aes(ymin = total, ymax = total), color = trolley_grey_light, 
                  size = .9, na.rm = TRUE) +
    geom_text(aes(label = percent(share, 1)), na.rm = TRUE, vjust = -1, 
              family = "Source Sans Pro SemiBold", color = trolley_grey) +
    scale_y_continuous(labels = unit_format(1, unit = "M", scale = 1e-6),
                       position = "right", expand = c(.005, .005)) +
    scale_x_continuous(expand = c(.005, .005), n.breaks = unique(df_viz$period) %>% length())+
    scale_alpha_identity() +
    scale_fill_manual(values = c("USAID" = denim, "PEPFAR" = trolley_grey_light)) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = str_wrap("USAID HAS SEEN A DECLINING SHARE OF THE PEPFAR'S REACH OF HIV PREVENTION SERVICESOVER TO KEY POPULATION INDIVIDUALS BUT HAS REACHED 1.3 MILLION EACH YEAR SINCE 2017", 100),
         subtitle = "PEPFAR Key Pop Individuals Provided Prevention Services",
         caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid()

  si_save("Images/08c_kp_trends_usaid.png")
  
  
  si_save("Graphics/08c_kp_trends_usaid.svg")


  
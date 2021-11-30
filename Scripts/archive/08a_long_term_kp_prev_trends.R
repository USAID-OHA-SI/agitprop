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
  
  #Source: PEPFAR Spotlight (public)
  df_hist <- read_csv("Data/Country and Regional Targets_Results 2004-2016.csv",
                      na = c("", "NA", "null"),
                      col_types = c(Year = "i",
                                    `Measure Value` = "d",
                                    .default = "c")) %>% 
    clean_names()
  
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
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
    ungroup()
  
  df_kp <- df_kp %>% 
    reshape_msd() %>% 
    arrange(indicator, period) %>% 
    select(-period_type) %>% 
    mutate(source = "MSD")

  df_hist_clean <- df_hist %>% 
    filter(indicator_short_name == "Key Pop Prevention Services",
           measure_name == "Results",
           country_region != "Global",
           dsd_ta == "DSD+TA") %>% 
    group_by(period = year, indicator = indicator_short_name) %>% 
    summarise(value = sum(measure_value, na.rm = TRUE)) %>% 
    ungroup()

  df_hist_clean <- df_hist_clean %>% 
    mutate(indicator = recode(indicator,
                              "Key Pop Prevention Services" = "KP_PREV"),
           period = glue("FY{str_sub(period, 3)}"),
           source = "Spotlight") %>% 
    filter(!period %in% unique(df_kp$period))
  
  df_kp <- bind_rows(df_hist_clean, df_kp)


  df_kp <- df_kp %>% 
    mutate(bar_alpha = case_when(period == curr_fy & curr_qtr != 4 ~ .6,
                                 TRUE ~ 1),
           year = glue("20{str_sub(period, 3, 4)}") %>% as.integer,
           ind_label = "Key Pop Prevention Services")
  
  
  
  df_viz <- df_kp %>% 
    filter(source != "Spotlight") 
  
  df_viz %>%
    ggplot(aes(year, value)) +
    geom_col(aes(alpha = bar_alpha), fill = denim) +
    scale_y_continuous(labels = unit_format(1, unit = "M", scale = 1e-6),
                       position = "right", expand = c(.005, .005)) +
    scale_x_continuous(expand = c(.005, .005), n.breaks = nrow(df_viz))+
    scale_alpha_identity() +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "PEPFAR HAS REACH OVER 2 MILLION TARGETED KEY POPULATION INDIVIDUALS EACH YEAR SINCE 2017 WITH HIV PREVENTION SERVICES",
         subtitle = "PEPFAR Key Pop Individuals Provided Prevention Services",
         caption = glue("Source: Spotlight FY10-14, {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid()

  si_save("Images/08a_kp_trends.png")
  
  si_save("Graphics/08a_kp_trends.svg", scale = 1.2)

  

  
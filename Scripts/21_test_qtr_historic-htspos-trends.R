# PROJECT:  agiprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  positive testing since PEPFAR start
# LICENSE:  MIT
# DATE:     2021-11-30
# UPDATED:  2021-12-06
# NOTE:     adapted from agitprop/04a_usaid_tx_trends.R

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


# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  

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
  msd_source <- source_info()
  
  #identify periods for plot
  curr_pd <- source_info(return = "period")
  curr_qtr <- source_info(return = "quarter")
  
  #append archive MSD to current version
  df_pos <- df %>% 
    bind_rows(df_arch) 
  
  #create a PEPFAR duplicate and aggregate up to country/global level
  df_pos <- df_pos %>%
    bind_rows(df_pos %>% mutate(fundingagency = "PEPFAR")) %>% 
    filter(indicator %in% c("HTS_TST_POS"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, fundingagency, indicator) %>% 
    summarise(value = sum(cumulative, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(fundingagency %in% c("PEPFAR", "USAID"),
           value != 0) %>% 
    arrange(indicator, fundingagency, fiscal_year) %>% 
    mutate(source = "MSD")

  #limit the historic data for TX_CURR/NEW data and aggregate
  df_hist_clean <- df_hist %>% 
    filter(indicator_short_name %in% c("HIV Tested Positive"),
           measure_name == "Results",
           country_region != "Global",
           dsd_ta == "DSD+TA") %>% 
    group_by(fiscal_year = year, indicator = indicator_short_name) %>% 
    summarise(value = sum(measure_value, na.rm = TRUE)) %>% 
    ungroup()

  #clean align names and remove any overlapping years
  df_hist_clean <- df_hist_clean %>% 
    mutate(indicator = recode(indicator,
                              "HIV Tested Positive" = "HTS_TST_POS"),
           fundingagency = "PEPFAR",
           source = "Spotlight") %>% 
    filter(!fiscal_year %in% unique(df_pos$fiscal_year))
  
  #combine historic data to MSD
  df_pos <- bind_rows(df_hist_clean, df_pos)


  #adjust for viz
  df_pos_viz <- df_pos %>% 
    mutate(bar_alpha = ifelse(fiscal_year == max(fiscal_year) & curr_qtr != 4, .6, 1))
  
  #add usaid share
  df_pos_viz <- df_pos_viz %>% 
    pivot_wider(names_from = fundingagency) %>% 
    mutate(usaid_share = USAID/PEPFAR) %>%
    pivot_longer(c(USAID, PEPFAR), names_to = "fundingagency",
                 values_drop_na = TRUE) %>% 
    mutate(usaid_share = case_when(fundingagency == "USAID" ~ usaid_share)) %>% 
    arrange(indicator, fundingagency, fiscal_year)
  
  #data point for context
  df_curr_val <- df_pos_viz %>% 
    filter(fiscal_year == max(fiscal_year)) %>%
    select(fiscal_year, indicator, fundingagency, value) %>% 
    pivot_wider(names_from = fundingagency) %>% 
    mutate(usaid_share = USAID/PEPFAR)

# VIZ ---------------------------------------------------------------------

  v <- df_pos_viz %>% 
    ggplot(aes(fiscal_year, value)) +
    geom_col(aes(alpha = bar_alpha, fill = fct_rev(fundingagency)),
             position = "identity") +
    geom_hline(yintercept = seq(1e6, 3e6, 1e6), color = "white") +
    geom_text(data = . %>% filter(fiscal_year != max(fiscal_year)),
              aes(label = percent(usaid_share, 1)), na.rm = TRUE,
              family = "Source Sans Pro", color = "white", vjust = 1.1) +
    geom_text(data = . %>% filter(fiscal_year == max(fiscal_year)),
              aes(label = percent(usaid_share, 1)), na.rm = TRUE,
              family = "Source Sans Pro", color = burnt_sienna, vjust = -.8) +
    scale_y_continuous(labels = unit_format(1, unit = "M", scale = 1e-6),
                       breaks =  seq(1e6, 3e6, 1e6),
                       position = "right", expand = c(.005, .005)) +
    scale_x_continuous(n.breaks = length(unique(df_pos_viz$fiscal_year)), expand = c(.005, .005))+
    scale_fill_manual(values = c("PEPFAR" = burnt_sienna_light, "USAID" = burnt_sienna)) +
    scale_alpha_identity() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "AS PEPFAR REACHED EPIDEMIC CONTROL, THERE HAVE BEEN LESS HIV POSITIVE PATIENTS IDENTIFIED",
         subtitle = glue("As of {curr_pd}, USAID identified \\
                          {percent(df_curr_val$usaid_share, 1)} \\
                          of PEPFAR's \\
                          {number(df_curr_val$PEPFAR, .1, scale = 1e-6)} million \\
                          HIV positive patients."),
         caption = glue("Source: Spotlight FY04-14, {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_nolines() +
    theme(legend.position = "none")

  v_ann <- v +
    annotate("text",
             x = 2014.2, y = .5e6, family = "Source Sans Pro",
             hjust = "right", size = 9/.pt, color = matterhorn,
             label = "No agency attribution\n of results before 2015") +
    annotate("text",
             x = 2020, y = 3e6, family = "Source Sans Pro",
             hjust = "left",
             color = matterhorn, size = 10/.pt,
             label = "All of PEPFAR") +
    annotate("curve",
             x = 2020, y = 3e6, xend = 2019.8, yend = 2.8e6,
             arrow = arrow(length = unit(0.05, "inches"),
                           type = "closed"),
             curvature = .4,
             color = suva_grey) +
    annotate("text",
             x = 2015.8, y = -1e5, family = "Source Sans Pro",
             hjust = "right",
             color = matterhorn, size = 10/.pt,
             label = "USAID") +
    annotate("curve",
             x = 2015.8, y = -1e5, xend = 2016.2, yend = 3e5,
             arrow = arrow(length = unit(0.05, "inches"),
                           type = "closed"),
             curvature = .4,
             color = "white")

  
  si_save("Graphics/21_test_qtr_historic-htspos-trends.svg")
  
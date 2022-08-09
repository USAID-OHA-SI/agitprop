# PROJECT:  agiprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  treatment scale up since PEPFAR start
# REF ID:   36402aea 
# LICENSE:  MIT
# DATE:     2021-05-14
# UPDATED:  2021-10-07

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

  source("Scripts/archive/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  
  ref_id <- "36402aea"
  

# IMPORT ------------------------------------------------------------------
  
  #Current MSD
  df <- si_path() %>% 
    return_latest("OU_IM_FY20") %>% 
    read_msd()
  
  #Archived MSD
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()

# MUNGE -------------------------------------------------------------------

  #source info
  curr_pd <- identifypd(df)
  curr_fy <- identifypd(df, "year")
  msd_source <- source_info()
  
  df_tx <- df %>% 
    bind_rows(df_arch) %>% 
    filter(funding_agency == "USAID",
           indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator",
           fiscal_year <= curr_fy) %>% 
    group_by(fiscal_year, funding_agency, indicator) %>% 
    summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
    ungroup()
  
  df_tx <- df_tx %>% 
    rename(period = fiscal_year, value = cumulative) %>% 
    mutate(period = str_replace(period, "20", "FY")) %>% 
    arrange(indicator, period) %>% 
    mutate(source = "MSD")

  df_tx <- df_tx %>% 
    mutate(bar_alpha = case_when(period == max(period) & str_detect(curr_pd, "Q4", negate = TRUE) ~ .6,
                                 TRUE ~ 1),
           year = glue("20{str_sub(period, 3, 4)}") %>% as.integer)
  
  df_tx <- df_tx %>% 
    mutate(ind_label = case_when(indicator == "TX_CURR" ~ "Currently receiving antiretroviral therapy",
                                 TRUE ~ "Newly enrolled on antiretroviral therapy"))
  
  title_info <- df_tx %>% 
    filter(indicator == "TX_CURR",
           period %in% c(min(period), max(period))) %>% 
    select(year, value) %>% 
    mutate(added = (value - lag(value)) %>% clean_number(1),
           yrs = year - lag(year)) %>% 
    filter(year == max(year))
  
  #data point for context
  curr_val <-  df_tx %>% 
    filter(period == max(period),
           indicator == "TX_CURR") %>%
    select(indicator, value) %>% 
    mutate(value = clean_number(value, 1)) %>% 
    pull(value)
  
df_tx %>% 
    ggplot(aes(year, value)) +
    geom_col(aes(alpha = bar_alpha, fill = ind_label),
             position = "identity") +
    geom_hline(yintercept = seq(2e6, 6e6, 2e6), color = "white") +
    scale_y_continuous(labels = label_number_si(),
                       position = "right", expand = c(.005, .005)) +
    scale_x_continuous(expand = c(.005, .005),
                       n.breaks = unique(df_tx$period) %>% length())+
    geom_text(aes(label = clean_number(value, 1), vjust = -0.3,
                  family = "Source Sans Pro")) +
    expand_limits(y = 72e5) +
    scale_fill_manual(values = c(genoa_light, genoa)) +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("USAID HAS ADDED {title_info$added} PATIENTS ONTO TREATMENT IN THE PAST {title_info$yrs} YEARS"),
         subtitle = glue("As of {curr_pd}, USAID provides treatment for {curr_val} patients"),
         caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')} | Ref ID: {ref_id}")) +
    si_style_nolines()

# 
   si_save("Images/04b_tx_trends_usaid.png")
   si_save("Graphics/04b_tx_trends_usaid.svg")
  
  
  
  

  
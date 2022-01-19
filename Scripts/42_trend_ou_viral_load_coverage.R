# PROJECT:  agitprop
# AUTHOR:   B.Kagniniwa | USAID
# PURPOSE:  VLC Trend by OU
# LICENSE:  MIT
# DATE:     2021-12-07
# 

# DEPENDENCIES ----

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
  library(lubridate)
  library(gt)
  

# GLOBAL VARS ----

  dir_merdata <- si_path()
  dir_graphics <- "./Graphics"
  
  file_ou_im_curr <- dir_merdata %>% 
    return_latest(pattern = "OU_IM_FY19-22_\\d{8}.*")
  
  file_ou_im_prev <- dir_merdata %>% 
    return_latest(pattern = "OU_IM_FY15-18_\\d{8}.*")
  
  epi_cntries <- c("Kenya", "Uganda", "Botswana", 
                   "Lesotho", "Eswatini", "Namibia")
  
# IMPORT ----
  
  #MER data
  df_msd1 <- file_ou_im_prev %>% read_msd()
  df_msd2 <- file_ou_im_curr %>% read_msd()
  
  df_msd <- df_msd1 %>% 
    bind_rows(df_msd2)
  
  # df_msd <- file_ou_im %>% 
  #   c(file_ou_im_prev) %>% 
  #   map_dfr(read_msd)
  
  df_msd <- df_msd %>% 
    clean_indicator() %>% 
    clean_agency() %>% 
    clean_countries(colname = "operatingunit")
  
  df_msd %>% glimpse()
  
  df_msd %>% 
    distinct(fiscal_year) %>% 
    arrange(fiscal_year) %>% 
    pull()
  
  curr_pd <- df_msd %>% identifypd()
  curr_fy <- df_msd %>% identifypd(pd_type = "year")
  
# MUNGE MER ----

  df_tx_ou <- df_msd %>% 
    filter(
      fiscal_year != curr_fy + 1,
      fundingagency == "USAID",
      indicator %in% c("TX_CURR", "TX_PVLS_D"),
      standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")
    ) %>% 
    group_by(fiscal_year, fundingagency, operatingunit, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    dplyr::select(-period_type) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    group_by(fundingagency, operatingunit) %>% 
    mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period),
           VLC = TX_PVLS_D / TX_CURR_LAG2) %>% 
    ungroup() %>% 
    relocate(TX_CURR_LAG2, .before = TX_CURR) %>% 
    filter(!is.na(TX_PVLS_D), TX_PVLS_D > 0)
    
  
  df_tx_global <- df_msd %>% 
    filter(
      fiscal_year != curr_fy + 1,
      fundingagency == "USAID",
      indicator %in% c("TX_CURR", "TX_PVLS_D"),
      standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")
    ) %>% 
    group_by(fiscal_year, fundingagency, operatingunit, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    group_by(fundingagency, operatingunit) %>% 
    mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period)) %>% 
    relocate(TX_CURR_LAG2, .before = TX_CURR) %>% 
    filter(!is.na(TX_PVLS_D), TX_PVLS_D > 0) %>% 
    group_by(period, fundingagency) %>% 
    summarise(across(starts_with("TX"), sum, na.rm = T), .groups = "drop") %>% 
    mutate(VLC = TX_PVLS_D / TX_CURR_LAG2)
  
# VIZ ----

  period_labels <- df_tx_ou %>% 
    filter(!str_detect(period, "FY17")) %>% 
    select(period) %>% 
    distinct(period) %>% 
    mutate(period = str_replace(period, "FY", "20"),
           period = case_when(
             str_sub(period, 1, 4) < 2019 ~ str_replace(period, "Q", "<br/>Q"),
             str_sub(period, 1, 4) >= 2019 & str_detect(period, "Q1") ~ str_replace(period, "Q", "<br/>Q"),
             TRUE ~ str_replace(period, "\\d{4}Q", "<br/>Q")
           )) %>% 
    pull()
  
  period_labels2 <- df_tx_ou %>% 
    filter(!str_detect(period, "FY17")) %>% 
    select(period) %>% 
    distinct(period) %>% 
    mutate(period = str_replace(period, "FY", "20"),
           period = case_when(
             str_sub(period, 1, 4) < 2019 ~ str_replace(period, "Q", "<br/>Q"),
             str_sub(period, 1, 4) >= 2019 & str_detect(period, "Q1") ~ str_replace(period, "Q", "<br/>Q"),
             TRUE ~ ""
           )) %>% 
    pull()
  
  period_labels3 <- df_tx_ou %>% 
    filter(!str_detect(period, "FY17"),
           str_detect(period, "Q4")) %>% 
    dplyr::select(period) %>% 
    distinct(period) %>% 
    mutate(period = str_sub(period, 1, 4)) %>% 
    pull()
  
  # Nigeria
  df_tx_ou %>% 
    filter(fundingagency == "USAID",
           operatingunit == "Nigeria") %>% 
    ggplot(aes(x = period, y = VLC)) +
    geom_col(fill = scooter) +
    geom_label(aes(label = percent(VLC, 1)),
               size = 6, fontface = "bold",
               fill = scooter, color = "white") +
    scale_x_discrete(labels = period_labels) +
    scale_y_continuous(labels = percent, position = "right") +
    labs(x = "", y = "") +
    si_style_ygrid() +
    theme(axis.text = element_text(family = "Source Sans Pro",
                                   size = 12, color = usaid_black))
  # OUs
  df_tx_ou %>% 
    filter(fundingagency == "USAID",
           operatingunit %ni% c("Angola", "Asia Region", "Ethiopia", 
                                "WAR", "WHR", "Vietnam", "Cote d'Ivoire",
                                "South Sudan", "DR"),
           !str_detect(period, "FY17"), 
           str_detect(period, "Q4$")) %>% 
    mutate(epi_color = case_when(
      operatingunit %in% epi_cntries ~ trolley_grey_light,
      TRUE ~ "white"
    )) %>% #view
    ggplot(aes(x = period, y = VLC)) +
    geom_rect(aes(fill = epi_color),
              xmin = -Inf, xmax = Inf,
              ymin = 0, ymax = 1,
              alpha = .3,
              color = NA) +
    geom_col(fill = scooter) +
    geom_hline(yintercept = 0, size = .3, color = usaid_black) +
    geom_hline(yintercept = 0.9, size = .3, lty = "dotted", color = usaid_black) +
    geom_text(aes(label = percent(VLC, 1)),
               size = 0, vjust = -.50, color = trolley_grey) +
    scale_x_discrete(labels = period_labels3) +
    #scale_y_continuous(breaks = c(0, .25, .50, .75, .90, 1), labels = percent, position = "right") +
    scale_y_continuous(breaks = c(0, .5, .90), labels = percent, position = "right") +
    scale_fill_identity() +
    facet_wrap(~operatingunit, ncol = 4) +
    labs(x = "", y = "",
         title = "USAID - Viral Load Coverage Trend by OU",
         subtitle = "Only Botswana, Eswatini, Kenya, Lesotho, Namibia, and Uganda have reached Epi Control",
         caption = "Source: FY21Q4c - VLC = TX_PVLS_D / lag(TX_CURR, 2)\nProduced by OHA/SIEI/SI/Core Analytics on 2022-01-19") +
    si_style_ygrid() +
    theme(axis.line.x = element_blank(),
          axis.text.y = element_text(family = "Source Sans Pro", size = 7, face = "bold", color = usaid_black),
          axis.text.x = element_markdown(family = "Source Sans Pro", size = 12, face = "bold", color = usaid_black),
          strip.text = element_text(family = "Source Sans Pro", size = 12, color = usaid_black))
  
  si_save(file.path(dir_graphics, "USAID - OU VLC Trend.png"),
         plot = last_plot(),
         path = NULL,
         scale = 1,
         width = 10,
         height = 5.625,
         dpi = 320)
  
  # Global 
  df_tx_global %>% 
    filter(fundingagency == "USAID",
           !str_detect(period, "FY17"),
           str_detect(period, "Q4$")) %>% 
    ggplot(aes(x = period, y = VLC)) +
    geom_col(fill = scooter) +
    geom_hline(yintercept = 0, size = 1, color = usaid_black) +
    geom_text(aes(label = percent(VLC, 1)),
              size = 8, vjust = -.2, fontface = "bold", color = usaid_black) +
    scale_x_discrete(labels = period_labels3) +
    scale_y_continuous(labels = percent, position = "right", expand = c(0, 0.08)) +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(family = "Source Sans Pro",
                                     size = 20, face = "bold",
                                     color = usaid_black))
  
  ggsave(file.path(dir_graphics, "USAID - VLC Trend.png"),
         plot = last_plot(),
         path = NULL,
         scale = 1,
         width = 6,
         height = 5,
         dpi = 320)
  
  
  # Table
  df_tx_ou %>% 
    filter(fundingagency == "USAID",
           !str_detect(period, "FY17")) %>% 
    select(period, operatingunit, VLC) %>%
    pivot_wider(names_from = period, values_from = VLC) %>% 
    gt()
  
  
  
  
  
  
  
  
  
  
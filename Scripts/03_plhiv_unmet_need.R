# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  alignment with epidemic
# LICENSE:  MIT
# DATE:     2021-05-11
# UPDATED:  2021-05-12
# NOTE:     adapted from USAID-OHA-SI/groundhogday (link below)
# URL:      https://github.com/USAID-OHA-SI/groundhog_day/blob/master/Scripts/FY21Q1_GLOBAL_ART-Saturation-USAID-PSNUs.R

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
  library(ggrepel)
  
  source("Scripts/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------

  authors <- c("Aaron Chafetz")

  saturation <- .95*.95

  msd_source <- msd_period()
  

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_rds()   
  
  df_nat <- si_path() %>% 
    return_latest("NAT") %>% 
    read_rds()  
  
# MUNGE -------------------------------------------------------------------
  
  #curr fy
    curr_fy <- identifypd(df, "year")
  
  #identify USAID districts (remove KP districts)
  usaid_tx_psnus <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate %in% c("Total Numerator", "KeyPop/HIVStatus"),
           fundingagency == "USAID",
           str_detect(psnu, "_Military", negate = TRUE),
           fiscal_year == curr_fy) %>% 
    count(operatingunit, psnuuid, standardizeddisaggregate, wt = cumulative) %>% 
    spread(standardizeddisaggregate, n, fill = 0) %>% 
    mutate(kp_share = `KeyPop/HIVStatus` / `Total Numerator`) %>% 
    filter(kp_share < .9) %>%
    distinct(psnuuid) %>% 
    pull()
  
  #extract PLHIV figures
  df_plhiv <- df_nat %>% 
    filter(indicator %in% c("PLHIV", "TX_CURR_SUBNAT"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex"),
           fiscal_year %in% c(curr_fy-1, curr_fy)) %>%
    rename(countryname = countrynamename) %>% 
    group_by(fiscal_year, indicator, operatingunit, countryname, snu1, psnu, psnuuid) %>% 
    summarise(value = sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = c(indicator, fiscal_year)) %>% 
    rename_all(tolower)
  
  #extract FY21 TX_CURR
  df_tx_pepfar <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           str_detect(psnu, "_Military", negate = TRUE),
           fiscal_year == curr_fy) %>%
    group_by(operatingunit, countrynamename, snu1, psnu, psnuuid) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename_with(~ glue("tx_curr_{curr_fy}_{.x}"), .cols = c(cumulative, targets))

  #join NAT + MSD data together
  df_combo <- full_join(df_plhiv, df_tx_pepfar)
  
  #filter down to just USAID districts
  df_combo_usaid <- df_combo %>% 
    filter(psnuuid %in% usaid_tx_psnus)
  
  #create coverage
  df_combo_usaid <- df_combo_usaid %>% 
    mutate(art_cov = !!sym(glue("tx_curr_subnat_{curr_fy}")) / !!sym(glue("plhiv_{curr_fy}")),
           art_cov_capped = ifelse(art_cov > 1.2, 1.21, art_cov)) %>% 
    group_by(operatingunit) %>% 
    mutate(target_share = !!sym(glue("tx_curr_subnat_{curr_fy}"))/sum(!!sym(glue("tx_curr_subnat_{curr_fy}")), na.rm = TRUE)) %>% 
    ungroup()
  
  #overall saturation
  df_combo_usaid <- df_combo_usaid %>%
    filter(operatingunit != "Democratic Republic of the Congo") %>% 
    group_by(operatingunit) %>% 
    mutate(art_cov_ou = sum(!!sym(glue("tx_curr_subnat_{curr_fy}")), na.rm = TRUE)/sum(!!sym(glue("plhiv_{curr_fy}")), na.rm = TRUE),
           art_cov_ou = ifelse(is.nan(art_cov_ou), NA, art_cov_ou),
           art_cov_ou_m = case_when(!!sym(glue("plhiv_{curr_fy}")) == max(!!sym(glue("plhiv_{curr_fy}")), na.rm = TRUE) ~ art_cov_ou)) %>%
    ungroup() %>% 
    mutate(plhiv_share = !!sym(glue("plhiv_{curr_fy}"))/sum(!!sym(glue("plhiv_{curr_fy}")), na.rm = TRUE))
  
  #clean up names
  df_viz <- df_combo_usaid %>% 
    mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                     operatingunit =="Dominican Republic" ~ "DR",
                                     TRUE ~ operatingunit))
  
  
  #flag
  df_viz <- df_viz %>% 
    mutate(flag = case_when(art_cov < saturation ~ moody_blue,
                            TRUE ~ trolley_grey),
           flag_plus = case_when(art_cov < saturation & target_share < .15 ~ moody_blue,
                                 art_cov < saturation & target_share > .15 ~ scooter,
                                 TRUE ~ trolley_grey),
           flag_label = case_when(art_cov < saturation & target_share > .2 ~ psnu),
           flag_plhiv_plus = case_when(art_cov < saturation & plhiv_share < .01 ~ moody_blue,
                                       art_cov < saturation & plhiv_share >= .01 ~ scooter,
                                       TRUE ~ trolley_grey),
           flag_plhiv_label = case_when(art_cov < saturation & plhiv_share >= .01 ~ psnu),
           flag_plhiv_label = flag_plhiv_label %>% 
             str_remove("^[:lower:]{2} ") %>% 
             str_remove_all(" (County|District|Metropolitan|Municipality)") %>% 
             str_remove("City of ") %>% 
             na_if("Data reported above PNSU Level")
    )
  
  #drop regional missions
  df_viz <- df_viz %>% 
    filter(str_detect(operatingunit, "Region", negate = TRUE))
  
  #create high vs low burden
  df_viz <- df_viz %>% 
    group_by(operatingunit) %>% 
    mutate(burden = ifelse(sum(!!sym(glue("plhiv_{curr_fy}")), na.rm = TRUE) > 500000, "High PLHIV Burden", "Low")) %>% 
    ungroup() 
  
  #ou range
  df_viz <- df_viz %>% 
    group_by(operatingunit) %>% 
    mutate(art_cov_ou_min = min(art_cov_capped, na.rm = TRUE),
           art_cov_ou_min = case_when(!!sym(glue("plhiv_{curr_fy}")) == max(!!sym(glue("plhiv_{curr_fy}")), na.rm = TRUE) ~ art_cov_ou_min),
           art_cov_ou_max = max(art_cov_capped, na.rm = TRUE),
           art_cov_ou_max = case_when(!!sym(glue("plhiv_{curr_fy}")) == max(!!sym(glue("plhiv_{curr_fy}")), na.rm = TRUE) ~ art_cov_ou_max)) %>% 
    ungroup()
  
  
  
# PLOT --------------------------------------------------------------------

  df_viz %>% 
    ggplot(aes(art_cov_capped, fct_reorder(operatingunit, art_cov_ou_m, na.rm = TRUE, .desc = TRUE),
               fill = flag_plhiv_plus), na.rm = TRUE) +
    geom_linerange(aes(xmin = art_cov_ou_min, xmax = art_cov_ou_max), na.rm = TRUE, size = .5, color = "gray80") +
    annotate("rect", xmin = 0, xmax = saturation, ymin = 0, ymax = 10, fill = "#808080", alpha = .05)+
    geom_vline(aes(xintercept = saturation), linetype = "dotted", color = trolley_grey, na.rm = TRUE) +
    geom_vline(aes(xintercept = 1.21), color = trolley_grey_light, na.rm = TRUE) +
    geom_errorbar(aes(xmin = art_cov_ou_m, xmax = art_cov_ou_m), size = 1.5/.pt, color = trolley_grey, na.rm = TRUE) +
    geom_jitter(data = filter(df_viz, flag_plhiv_plus != scooter), aes(size = plhiv_2021), height = .3,  shape = 21, alpha = .6, color = "white", na.rm = TRUE) +
    geom_jitter(data = filter(df_viz, flag_plhiv_plus == scooter), aes(size = plhiv_2021), height = .3,  shape = 21, alpha = .6, color = scooter, stroke = 1/.pt, na.rm = TRUE) +
    geom_text_repel(aes(label = flag_plhiv_label), na.rm = TRUE,
                    family = "Source Sans Pro", color = "#505050", size = 3,
                    segment.color = trolley_grey_light) +
    facet_grid(burden ~ ., scales = "free_y", space = "free") +
    scale_x_continuous(label = percent_format(1), breaks = seq(0, 1.2, by = .25),
                       expand = c(.005, .005)) +
    scale_fill_identity() +
    labs(x = NULL, y = NULL,
         title = toupper("Estimated ART coverage in USAID supported treament 'districts'"),
         subtitle = glue("USAID 'districts' <span style='color:{moody_blue}'>below estimated saturation</span> and those with the <span style='color:{scooter}'>largest share of PLHIV</span>"),
         caption = glue("Estimated ART Coverage = FY21 TX_CURR_SUBNAT / FY21 PLHIV
         High PLHIV Burden OUs = Total PLHIV ({{curr_fy}}) in USAID districts is greater than 500,000
         DRC removed with no PLHIV estimates;Regional programs also removed
         KP only districts (+90% of the TX_CURR results were in the KP disagg) were excluded
         Source: {{msd_source}} + NAT_SUBNAT
         SI analytics: {paste(authors, collapse = '/')}
         US Agency for International Development")) +
    si_style_nolines() +
    theme(legend.position = "none",
          panel.spacing.y = unit(.5, "lines"),
          plot.subtitle = element_markdown())
  
  si_save("Images/03a_ART_Coverage.png")
  
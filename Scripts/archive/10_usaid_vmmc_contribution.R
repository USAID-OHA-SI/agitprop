# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  tracking VMMC contributions
# LICENSE:  MIT
# DATE:     2021-05-20
# UPDATED:  2021-10-12

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
  
  source("Scripts/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Ben Kasdan")
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd() 
  
  df_hist <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()   
  

# MUNGE -------------------------------------------------------------------

  df_vmmc <- df %>% 
    bind_rows(df_hist) %>%
    filter(indicator == "VMMC_CIRC",
           standardizeddisaggregate == "Total Numerator")
  
  df_vmmc <- df_vmmc %>% 
    bind_rows(df_vmmc %>% mutate(fundingagency = "PEPFAR")) %>% 
    group_by(fiscal_year, fundingagency) %>% 
    summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(period = glue("FY{str_sub(fiscal_year, -2)}")) %>% 
    filter(cumulative > 0)
  
  df_vmmc <- df_vmmc %>% 
    filter(fundingagency %in% c("USAID", "PEPFAR")) %>% 
    group_by(period) %>% 
    mutate(share = case_when(fundingagency == "USAID" ~ cumulative/lag(cumulative)),
           total = case_when(fundingagency == "PEPFAR" ~ cumulative)) %>% 
    ungroup()
  

  curr_contribution <- df_vmmc %>% 
    select(-share, -total) %>% 
    pivot_wider(names_from = fundingagency,
                values_from = cumulative) %>% 
    filter(period == max(period)) %>%  
    summarise(across(c(PEPFAR, USAID), sum, na.rm = TRUE)) %>% 
    mutate(share = USAID/PEPFAR)
  
  
  df_vmmc_cum <- df %>% 
    bind_rows(df_hist) %>% 
    filter(indicator == "VMMC_CIRC",
           standardizeddisaggregate == "Total Numerator")
    
  df_vmmc_cum <- df_vmmc_cum %>% 
    bind_rows(df_vmmc_cum %>% mutate(fundingagency = "PEPFAR")) %>% 
    group_by(fiscal_year, fundingagency) %>% 
    summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
    ungroup()
  
  df_vmmc_cum <- df_vmmc_cum %>% 
    filter(fundingagency %in% c("USAID", "PEPFAR")) %>% 
    arrange(fundingagency, fiscal_year) %>% 
    group_by(fundingagency) %>% 
    mutate(rolling_total = cumsum(cumulative)) %>% 
    ungroup() %>% 
    group_by(fiscal_year) %>% 
    mutate(#share = case_when(fundingagency == "USAID" ~ cumulative/lag(cumulative)),
           total = case_when(fundingagency == "PEPFAR" ~ rolling_total)) %>% 
    ungroup()
  
  
# VIZ ---------------------------------------------------------------------
  
  msd_source <- source_info()
  
  df_vmmc %>% 
    ggplot(aes(period, cumulative)) + 
    geom_col(aes(fill = fundingagency, alpha = fundingagency), position = "identity") +
    geom_text(aes(label = percent(share, 1)), na.rm = TRUE, vjust = -1, 
              family = "Source Sans Pro SemiBold", color = trolley_grey) +
    geom_errorbar(aes(ymin = total, ymax = total), color = trolley_grey_light, 
                  size = .9, na.rm = TRUE) +
    scale_fill_manual(values = c("USAID" = moody_blue, "PEPFAR" = trolley_grey_light)) +
    scale_alpha_manual(values = c("USAID" = .8, "PEPFAR" = .4)) +
    scale_y_continuous(labels = unit_format(1, unit = "M", scale = 1e-6),
                       breaks = seq(0, 4e6, 1e6), 
                       position = "right", expand = c(.005, .005)) +
    expand_limits(y = 4e6) +
    labs(x = NULL, y = NULL, fill = NULL, alpha = NULL,
         title = glue("USAID HAS CONTRIBUTED {percent(curr_contribution$share,1)} OF THE {clean_number(curr_contribution$PEPFAR, 1) %>% toupper} PEPFAR VOLUNTARY MALE CIRCUMCISIONS THIS YEAR"),
         caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid()

  # si_save("Images/10_vmmc_contribution.png")  
  si_save("Graphics/10_vmmc_contribution.svg")  
  
  
  cum_tot <- df_vmmc_cum %>% 
    filter(fiscal_year == max(fiscal_year)) %>% 
    select(fundingagency, rolling_total) %>% 
    mutate(rolling_total = clean_number(rolling_total, 1) %>% str_replace("M", " MILLION")) %>% 
    spread(fundingagency, rolling_total)
  
  df_vmmc_cum %>% 
    ggplot(aes(fiscal_year, rolling_total)) + 
    geom_blank(aes(y = rolling_total*1.1)) +
    geom_col(aes(fill = fundingagency, alpha = fundingagency), position = "identity") +
    geom_text(data = filter(df_vmmc_cum, fundingagency == "USAID"),
                            aes(label = clean_number(rolling_total, 1)), na.rm = TRUE, vjust = -1,
              family = "Source Sans Pro SemiBold", color = trolley_grey) +
    geom_errorbar(aes(ymin = total, ymax = total), color = trolley_grey_light, 
                  size = .9, na.rm = TRUE) +
    scale_fill_manual(values = c("USAID" = burnt_sienna, "PEPFAR" = trolley_grey_light)) +
    scale_alpha_manual(values = c("USAID" = .8, "PEPFAR" = .4)) +
    scale_y_continuous(labels = unit_format(1, unit = "M", scale = 1e-6),
                       position = "right", expand = c(.005, .005)) +
    scale_x_continuous(n.breaks = unique(df_vmmc_cum$fiscal_year) %>% length) +
    expand_limits(y = 1.25e6) +
    labs(x = NULL, y = NULL, fill = NULL, alpha = NULL,
        title = glue("USAID HAS CONDUCTED {cum_tot$USAID} VOLUNTARY MEDICAL MALE CIRCUMCISIONS SINCE 2015"),
         caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid()
  
  si_save("Graphics/10b_vmmc_cumulative.svg")
  
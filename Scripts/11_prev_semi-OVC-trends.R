# Project: agitprop
# File: 11_prev_semi-OVC-trends
# Author: T Essam | K Srikanth | A Chafetz
# Purpose: Show USAID contribution to OVC_SERV
# License: MIT
# Date: 2021-11-24

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
  library(googlesheets4)


# GLOBALS -----------------------------------------------------------------

  load_secrets()
  authors <- c("Karishma Srikanth", "Aaron Chafetz", "Tim Essam")
  
  msd_path <- si_path() %>% return_latest("OU_IM_FY19")
  msd_path2 <- si_path() %>% return_latest("OU_IM_FY15")
  
  #source info
  curr_pd <- source_info(msd_path, return = "period")
  curr_fy <- source_info(msd_path, return = "fiscal_year")
  msd_source <- source_info(msd_path)


# IMPORT ------------------------------------------------------------------

#Current MSD
  df <- read_msd(msd_path)
  df_arch <- read_msd(msd_path2)


#  MUNGE OVC --------------------------------------------------------------

  df_ovc_sh <- 
    df %>% 
    filter(indicator %in% c("OVC_SERV"),
           standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex/DREAMS", "Age/Sex/Preventive"),
           trendscoarse == "<18",
           fiscal_year <= curr_fy) %>% 
    bind_rows(df_arch %>% filter(indicator %in% c("OVC_SERV"),
                                 standardizeddisaggregate == "Age/Sex",
                                 trendscoarse == "<18")) 
  
  df_ovc_sh_all <- 
    df_ovc_sh %>% 
    bind_rows(df_ovc_sh %>% mutate(fundingagency = "PEPFAR")) %>% 
    group_by(fiscal_year, fundingagency) %>% 
    summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(period = glue("FY{str_sub(fiscal_year, -2)}")) %>% 
    filter(cumulative > 0)
  
  df_ovc_sh_usaid <-
    df_ovc_sh_all %>% 
    filter(fundingagency %in% c("USAID", "PEPFAR")) %>% 
    group_by(period) %>% 
    mutate(share = case_when(fundingagency == "USAID" ~ cumulative/lag(cumulative)),
           total = case_when(fundingagency == "PEPFAR" ~ cumulative)) %>% 
    ungroup()
  
  curr_contribution <- df_ovc_sh_usaid %>% 
    select(-share, -total) %>% 
    pivot_wider(names_from = fundingagency,
                values_from = cumulative) %>% 
    filter(period == max(period)) %>%  
    summarise(across(c(PEPFAR, USAID), sum, na.rm = TRUE)) %>% 
    mutate(share = USAID/PEPFAR)
  

#  PLOT USAID SHARE OF OVC_SERV -------------------------------------------

  df_ovc_sh_usaid %>% 
    ggplot(aes(period, cumulative)) + 
    geom_col(aes(fill = fundingagency, alpha = fundingagency), position = "identity") +
    geom_hline(yintercept = seq(1e6, 6e6, 1e6), color = "white", size = 0.25) +
    geom_text(aes(label = percent(share, 1)), na.rm = TRUE, vjust = -1, 
              family = "Source Sans Pro SemiBold", color = trolley_grey) +
    geom_errorbar(aes(ymin = total, ymax = total), color = trolley_grey_light, 
                  size = .9, na.rm = TRUE) +
    scale_fill_manual(values = c("USAID" = denim_light, "PEPFAR" = trolley_grey_light)) +
    scale_alpha_manual(values = c("USAID" = 1, "PEPFAR" = .4)) +
    scale_y_continuous(labels = unit_format(1, unit = "M", scale = 1e-6),
                       breaks = seq(0, 6e6, 1e6), 
                       position = "right", expand = c(.005, .005)) +
    expand_limits(y = 4e6) +
    labs(x = NULL, y = NULL, fill = NULL, alpha = NULL,
         title = glue("USAID HAS CONTRIBUTED {percent(curr_contribution$share,1)} OF ALL PEPFAR SERVICES OFFERED TO ORPHANS AND VULNERABLE CHILDREN 
                      UNDER 18 THIS YEAR"),
         caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid(text_scale = 1.25)

  si_save("Images/11_prev_semi_OVC_SERV-trends.png", scale = 1.25)
  
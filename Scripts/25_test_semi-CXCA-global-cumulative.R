# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | T.Essam | K. Srikanth | USAID
# PURPOSE:  Global cumulative CX_CA screening and treatment trends
# LICENSE:  MIT
# DATE:     2021-07-19
# UPDATED:  2021-12-03
# NOTE:     Created based on request from FO "2 visuals for AHOP deck" -- 2021-07-16


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
  library(googlesheets4)
  library(mindthegap)
  library(geomtextpath)

# GLOBALS -----------------------------------------------------------------

  load_secrets()
  authors <- c("Karishma Srikanth", "Aaron Chafetz", "Tim Essam")
  
  msd_path <- si_path() %>% return_latest("OU_IM_FY19")
  msd_path2 <- si_path() %>% return_latest("OU_IM_FY15")
  
  #source info
  curr_pd <- source_info(msd_path, return = "period")
  curr_fy <- source_info(msd_path, return = "fiscal_year")
  msd_source <- source_info(msd_path)
  
  pull_figures <- function(df, indic, partner, metric) {
    df %>% 
      filter(indicator == indic, 
             partner_type == partner, 
             fiscal_year == curr_fy) %>% 
      select({{metric}}) %>% 
      pull()
  }
  
  
# IMPORT ------------------------------------------------------------------
  
  #Current MSD
  df <- read_msd(msd_path)
  df_arch <- read_msd(msd_path2)
  

# MUNGE -------------------------------------------------------------------

  df_cxca <- df %>% 
    bind_rows(df_arch) %>% 
    filter(fundingagency == "USAID",
           str_detect(indicator, "CXCA"),
           standardizeddisaggregate == "Total Numerator",
           fiscal_year <= curr_fy, 
           operatingunit != "Haiti")
  
  df_cxca <- df_cxca %>% 
    bind_rows(df_cxca %>% mutate(operatingunit = "USAID")) %>% 
    group_by(fiscal_year, fundingagency, indicator, operatingunit) %>% 
    summarise(across(matches("2|4"), sum, na.rm = TRUE)) %>% 
    pivot_longer(cols = matches("qtr"),
                 names_to = "period") %>% 
    spread(indicator, value) %>% 
    mutate(tx_rate = CXCA_TX / CXCA_SCRN_POS,
           period = paste0("FY", str_sub(fiscal_year, 3, 4), "Q", str_sub(period, 4))) %>% 
    group_by(operatingunit) %>% 
    mutate(order_var = sum(CXCA_SCRN, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(ou_order = fct_reorder(operatingunit, order_var, .desc = T))
  

# VIZ ---------------------------------------------------------------------

  # all breaks if needed
  cust_breaks <- c(paste0("FY", rep(seq(18, 21, 1), each = 2), "Q", seq(2, 4, 2)))
  
 a <-  df_cxca %>% filter(operatingunit == "USAID",  period != "FY18Q2") %>% 
    ggplot(aes(x = period, y = CXCA_SCRN)) +
    geom_col(fill = golden_sand_light) +
    facet_wrap(~ou_order, scales = "free_y") +
    si_style_xline() +
   geom_hline(yintercept = seq(1e5, 3e5, 1e5), color = "white", size = 0.5) +
   scale_y_continuous(position = "right", labels = label_number_si()) +
   labs(x = NULL, y = NULL)
  
  
  b <- df_cxca %>% 
    filter(operatingunit == "USAID", period != "FY18Q2") %>% 
    ggplot(aes(x = period, y = tx_rate, group = operatingunit)) +
    geom_area(aes(y = 1), fill = denim_light, alpha = 0.5)+
    geom_area(fill = golden_sand_light)+
    geom_line(color = golden_sand, size = 2) +
    # geom_textpath(aes(label = "Treatment rate"), hjust = 0.95, vjust = -1, include_line = F)+
    geom_text(aes(label = "Treatment rate", y = 1, x = "FY21Q2"), vjust = -1) +
    geom_hline(yintercept = 1, size = 0.25, linetype = "dotted") +
    geom_label(aes(label = percent(tx_rate, 1))) +
    si_style_xline() +
    # coord_cartesian(expand = F) +
    scale_x_discrete(expand = expansion(add = 0.25))+
    scale_y_continuous(expand = expansion(mult = 0), lim = c(0, 1.5)) +
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank())

  cowplot::plot_grid(a, b, ncol = 1, align = "hv", axis = "bt", rel_heights = c(5, 2))
 

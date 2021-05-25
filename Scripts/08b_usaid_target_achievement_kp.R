# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  USAID achievement
# LICENSE:  MIT
# DATE:     2021-05-25
# UPDATED: 

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
  
  source("Scripts/99_utilities.R")


# GLOBAL VARIABLES --------------------------------------------------------
  
  ind_sel <- c("PrEP_NEW", "KP_PREV", "HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")

  authors <- c("Aaron Chafetz", "Tim Essam")
  
  #msd_source <- msd_period()
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd()   


# MUNGE -------------------------------------------------------------------

  curr_fy <- identifypd(df, "year")
  curr_qtr <- identifypd(df, "quarter")
  curr_pd <- identifypd(df)
  msd_source <-  msd_period(period = curr_pd)
  trgt_rng <- 1*(curr_qtr/4)
  
  
  df_achv <- df %>% 
    filter(fundingagency == "USAID", 
           indicator %in% ind_sel,
           standardizeddisaggregate %in% c("KeyPop/Result", "KeyPop/HIVStatus", "KeyPopAbr", "KeyPop"),
           fiscal_year == curr_fy) %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    calc_achv(curr_qtr)
  
  df_viz <- df_achv %>% 
    mutate(x = .5, 
           y = .5,
           goal = ifelse(indicator == "TX_CURR", 1, trgt_rng),
           indicator = factor(indicator, ind_sel),
           lab = ifelse(indicator == ind_sel[1],
                        glue("Results - {clean_number(cumulative, 1)}\nTargets - {clean_number(targets,1)}"),
                        glue("{clean_number(cumulative, 1)}\n{clean_number(targets,1)}"))) 
  
  df_viz %>% 
    ggplot(aes(x, y)) +
    geom_point(aes(size = achievement, color = achv_color), shape = 15, alpha = .3) +
    geom_point(aes(size = goal), shape = 0, stroke = 1.3, color = trolley_grey) +
    geom_text(aes(label = percent(achievement, 1)),
              family = "Source Sans Pro SemiBold", color = "gray30") +
    geom_text(aes(y = .1, label = lab),
              family = "Source Sans Pro", color = trolley_grey) +
    facet_grid(~indicator) +
    scale_size(range = c(10, 40)) +
    scale_color_identity() +
    si_style_nolines() +
    expand_limits(x = c(0, 1), y = c(0, .8)) +
    labs(x = NULL, y = NULL,
         title = "SELECT USAID KEY POPULATION INDICATOR PERFORMANCE ACROSS ALL COUNTRIES",
         subtitle = glue("as of {curr_pd}, goal of being at around {percent(trgt_rng)} of the FY target"),
         caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    theme(legend.position = "none",
          strip.text.x = element_text(hjust = .5, size = 14),
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  si_save("Images/08b_achievement_kp.png", height = 4)
  
  ggsave("Graphics/08b_achievement_kp.svg", height = 4, width = 10)
  
  
  
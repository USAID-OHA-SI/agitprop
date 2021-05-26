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
  library(waffle) #devtools::install_github("hrbrmstr/waffle")
  
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
    mutate(achv_round = round(achievement*100),
           achv_round = ifelse(achv_round > 100, 100, achv_round),
           gap = 100-achv_round) %>% 
    pivot_longer(c(achv_round, gap), names_to = "status") %>% 
    mutate(achv_color = ifelse(status == "gap", "#EBEBEB", achv_color),
           achv_color = ifelse(achv_color == trolley_grey_light, trolley_grey, achv_color),
           achv_alpha = ifelse(status == "gap", .1, 1),
           indicator = factor(indicator, ind_sel),
           ind_lab = case_when(indicator == "PrEP_NEW" ~ "Newly enrolled on antiretroviral pre-exposure prophylaxis",
                               indicator == "KP_PREV" ~ "Key pop individuals provided prevention services",
                               indicator == "HTS_TST" ~ "Receiving HIV testing service and results",
                               indicator == "HTS_TST_POS" ~ "Receiving HIV testing services and positive results",
                               indicator == "TX_NEW" ~ "Newly enrolled on antiretroviral therapy",
                               indicator == "TX_CURR"~ "Currently receiving antiretroviral therapy"),
           ind_lab = str_wrap(ind_lab, width = 25),
           val_lab = ifelse(indicator == ind_sel[1],
                            glue("Results - {clean_number(cumulative)}\nTargets - {clean_number(targets)}"),
                            glue("{clean_number(cumulative)}\n{clean_number(targets)}")),
           full_lab = glue("{ind_lab}\n\n{val_lab}")) %>% 
    arrange(indicator) %>% 
    mutate(full_lab = fct_inorder(full_lab))
  
  
  df_viz %>% 
    ggplot(aes(fill = achv_color, values = value, alpha = achv_alpha)) +
    geom_waffle(color = "white", size = 1, n_rows = 10, flip = TRUE) +
    geom_text(aes(x = 5, y  = 12, label = percent(achievement, 1), color = achv_color),
              family = "Source Sans Pro SemiBold", size = 14/.pt) +
    facet_wrap(~full_lab, nrow = 1, strip.position = "bottom") +
    expand_limits(y = 14) +
    scale_x_discrete() +
    scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                       expand = c(0,0)) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_alpha_identity() +
    coord_equal() +
    labs(x= NULL, y = NULL,
         title = "SELECT USAID KEY POPULATION INDICATOR PERFORMANCE ACROSS ALL COUNTRIES",
         subtitle = glue("as of {curr_pd}, goal of being at around {percent(trgt_rng)} of the FY target"),
         caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_nolines() +
    theme(axis.text.y = element_blank(),
          strip.text.x = element_text(hjust = .5),
          panel.spacing = unit(1, "pt")) +
    guides(fill = guide_legend(reverse = TRUE))
  
  si_save("Images/08b_achievement_kp.png", height = 4)
  
  si_save("Graphics/08b_achievement_kp.svg", height = 4)
  
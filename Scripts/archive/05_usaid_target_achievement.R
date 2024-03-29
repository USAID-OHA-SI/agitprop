# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  USAID achievement
# REF ID:   e4394ee2 
# LICENSE:  MIT
# DATE:     2021-05-19
# UPDATED:  2021-10-05

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
  library(waffle) #devtools::install_github("hrbrmstr/waffle")
  
  source("Scripts/99_utilities.R")

ref_id <- "e4394ee2"



# GLOBAL VARIABLES --------------------------------------------------------
  
  ind_sel <- c("PrEP_NEW", "VMMC_CIRC", "HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "OVC_SERV")

  authors <- c("Aaron Chafetz", "Tim Essam")
  
  #msd_source <- msd_period()
  
  load_secrets()
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY21") %>% 
    read_psd()   
  
  get_metadata()


# MUNGE -------------------------------------------------------------------

  curr_fy <- identifypd(df, "year")
  curr_qtr <- identifypd(df, "quarter")
  curr_pd <- identifypd(df)
  msd_source <-  source_info()
  trgt_rng <- 1*(curr_qtr/4)
  
  
  df_achv <- df %>% 
    filter(funding_agency == "USAID", 
           indicator %in% ind_sel,
           standardizeddisaggregate == "Total Numerator",
           fiscal_year == metadata$curr_fy) %>% 
    resolve_knownissues(store_excl = TRUE)
  
  df_achv <- df_achv %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    adorn_achievement(curr_qtr)
  

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
                               indicator == "VMMC_CIRC" ~ "Voluntary medical male circumcision for HIV prevention",
                               indicator == "HTS_TST" ~ "Receiving HIV testing service and results",
                               indicator == "HTS_TST_POS" ~ "Receiving HIV testing services and positive results",
                               indicator == "TX_NEW" ~ "Newly enrolled on antiretroviral therapy",
                               indicator == "TX_CURR"~ "Currently receiving antiretroviral therapy"),
           ind_lab = str_wrap(ind_lab, width = 25),
           val_lab = ifelse(indicator == ind_sel[1],
                            glue("Results - {clean_number(cumulative)}\nTargets - {clean_number(targets)}"),
                            glue("{clean_number(cumulative)}\n{clean_number(targets)}")),
           full_lab = glue("{ind_lab}\n\n{val_lab}"),
           achievement = case_when(status == "achv_round" ~ achievement)) %>% 
    arrange(indicator) %>% 
    mutate(full_lab = fct_inorder(full_lab))
  
  
  df_viz %>% 
    ggplot(aes(fill = achv_color, values = value, alpha = achv_alpha)) +
    geom_waffle(color = "white", size = 1, n_rows = 10, flip = TRUE, na.rm = TRUE) +
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
         title = "SELECT USAID INDICATOR PERFORMANCE ACROSS ALL COUNTRIES",
         subtitle = glue("as of {curr_pd}, goal of being at around {percent(trgt_rng)} of the FY target"),
         caption = glue(metadata$caption)) +
    si_style_nolines() +
    theme(axis.text.y = element_blank(),
          strip.text.x = element_text(hjust = .5),
          panel.spacing = unit(1, "pt")) +
    guides(fill = guide_legend(reverse = TRUE))
  
  si_save("Images/05b_achievement.png", height = 4)
  
  si_save("Graphics/05b_achievement.svg", height = 4)
  
  
  df_target_viz %>% 
    ggplot(aes(y = fct_reorder(agency, value))) + 
    #geom_col(aes(x = total), fill = trolley_grey_light) + 
    geom_col(aes(total, alpha = 0.8), fill = trolley_grey_light) +
    geom_errorbar(aes(y = agency, xmin = total, xmax =total),
                  color = trolley_grey) +
    geom_col(aes(value, fill = fill_color, alpha = 0.8)) +
    geom_text(aes(x = value + 18000000, 
                  label = label_number(0.1, scale_cut = cut_short_scale())(value)),
              family = "Source Sans Pro", color = nero, vjust = -0.5) +
    geom_text(aes(x = value + 18000000,
                  label = percent(share, 0.1),
                  family = "Source Sans Pro", color = trolley_grey, vjust = 1),
              size = 3.5) +
    scale_fill_identity() +
    scale_color_identity()+
    scale_alpha_identity() +
    si_style_xgrid() +
    scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale())) +
    labs(x = NULL,
         y = NULL,
         title = "USAID accounts for almost 42% of total COP/ROP23 Targets" %>% toupper(),
         caption = glue("Source: COP23 COP Matrix Report | Ref id: {ref_id}"))
  
  si_save("Graphics/COP23_GHPortfolio_TargetShare.svg")
  si_save("Images/COP23_GHPortfolio_TargetShare.png")
  
  df_viz %>% 
    mutate(total = 100) %>% 
    filter(!is.na(achievement)) %>% 
   # pivot_wider(names_from = "funding_agency", values_from = "n") %>% 
    # mutate(usaid_share = USAID/PEPFAR) %>% 
    ggplot(aes(y = fct_reorder(indicator, cumulative))) + 
    geom_col(aes(total), fill = trolley_grey_light) +
    geom_errorbar(aes(y = indicator, xmin = total, xmax =total),
                  color = trolley_grey) +
    geom_col(aes(value, alpha = 0.8, fill = achv_color)) +
    scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale()))+
    geom_text(aes(x = value,
                  label = val_lab,
              family = "Source Sans Pro", color = nero)) +
    geom_text(aes(x = total,
                  label = glue("{value}%"),
                  family = "Source Sans Pro", color = trolley_grey)) +
    # geom_text(aes(x = PEPFAR,
    #               label = label_number(0.1, scale_cut = cut_short_scale())(PEPFAR)),
    #           family = "Source Sans Pro", color = nero, vjust = -0.5) +
    # geom_text(aes(x = USAID + 3000000,
    #               label = percent(usaid_share, 0.1),
    #               family = "Source Sans Pro", color = trolley_grey, vjust = 1),
    #           size = 3.5) +
    scale_fill_identity() +
    scale_color_identity()+
    scale_alpha_identity() +
    si_style_xgrid() +
    labs(x = NULL, y = NULL,
         title = "USAID % achievement towards FY23 targets, as of Q2" %>% toupper(),
         caption = metadata$caption)
  
  si_save("Graphics/target_achv.svg")
  
  
  # #old method of boxes
  # df_viz <- df_achv %>% 
  #   mutate(x = .5, 
  #          y = .5,
  #          goal = ifelse(indicator == "TX_CURR", 1, trgt_rng),
  #          indicator = factor(indicator, ind_sel),
  #          lab = ifelse(indicator == ind_sel[1],
  #                       glue("Results - {clean_number(cumulative, 1)}\nTargets - {clean_number(targets,1)}"),
  #                       glue("{clean_number(cumulative, 1)}\n{clean_number(targets,1)}"))) 
  # 
  # df_viz %>% 
  #   ggplot(aes(x, y)) +
  #   geom_point(aes(size = achievement, color = achv_color), shape = 15, alpha = .3) +
  #   geom_point(aes(size = goal), shape = 0, stroke = 1.3, color = trolley_grey) +
  #   geom_text(aes(label = percent(achievement, 1)),
  #             family = "Source Sans Pro SemiBold", color = "gray30") +
  #   geom_text(aes(y = .1, label = lab),
  #             family = "Source Sans Pro", color = trolley_grey) +
  #   facet_grid(~indicator) +
  #   scale_size_area(max_size = 40) +
  #   # scale_size(range = c(10, 40)) +
  #   scale_color_identity() +
  #   si_style_nolines() +
  #   expand_limits(x = c(0, 1), y = c(0, .8)) +
  #   labs(x = NULL, y = NULL,
  #        title = "SELECT USAID INDICATOR PERFORMANCE ACROSS ALL COUNTRIES",
  #        subtitle = glue("as of {curr_pd}, goal of being at around {percent(trgt_rng)} of the FY target"),
  #        caption = glue("Source: {msd_source}
  #                       SI analytics: {paste(authors, collapse = '/')}
  #                    US Agency for International Development")) +
  #   theme(legend.position = "none",
  #         strip.text.x = element_text(hjust = .5, size = 14),
  #         axis.text.x = element_blank(),
  #         axis.text.y = element_blank())
  # 
  # si_save("Images/05_achievement.png", height = 4)
  # 
  # ggsave("Graphics/05_achievement.svg", height = 4, width = 10)
  
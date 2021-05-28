# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  95s progress - PHIA
# LICENSE:  MIT
# DATE:     2021-05-13
# UPDATED:  2021-05-25

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
  library(janitor)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam"
               )
  dodge <- position_dodge(width=0.5)
  
  level <- .95

  date_pulled <- return_latest("Data", "PHIA_90s") %>% 
    file.info() %>% 
    pull(ctime) %>% 
    format("%Y-%m-%d")
  
# IMPORT ------------------------------------------------------------------
  
  #PHIA - https://phia-data.icap.columbia.edu/visualization#
  # HIV Prevalence, 90-90-90(Labratory or self-reported ARV)
  
  df_phia <- return_latest("Data", "PHIA_90s") %>%
    read_csv() %>%
    clean_names()
  
  df_phia_prev <- return_latest("Data", "PHIA_prev") %>%
    read_csv() %>%
    clean_names()
  
# MUNGE -------------------------------------------------------------------
  
  #join on prev
  df_phia <- df_phia_prev %>%
    mutate(prevalence = value/100) %>%
    select(country, prevalence) %>%
    left_join(df_phia, .)
  
  #remove conditional
  df_phia <- df_phia %>%
    filter(indicator_type == "Overall")
  
  #arrange indicator
  df_phia <- df_phia %>%
    mutate(color = case_when(subindicator == "Diagnosed" ~ old_rose, 
                             subindicator == "On treatment" ~ golden_sand, 
                             subindicator == "Virally suppressed" ~ scooter),
           subindicator_color = glue("<span style='color:{color}'>{subindicator}</span>"),
           subindicator = fct_inorder(subindicator) %>% fct_rev(),
           subindicator_color = fct_inorder(subindicator_color) %>% fct_rev)
  
  #convert number to percent
  df_phia <- df_phia %>%
    mutate(across(c(value, confidence_lower, confidence_upper), ~ ./100))
  
  #add year to country & order, group by prev
  df_phia <- df_phia %>%
    mutate(country_lab = glue("{country} ({year})"),
           order_diagnosed = ifelse(subindicator == "Diagnosed", value, 0),
           country_lab = fct_reorder(country_lab, order_diagnosed, max),
           prev_group = ifelse(prevalence > .1, "High Prevalence (>10%)", "Low"))
  
  #add goal 95-95-95 line 
    df_phia <- df_phia %>% 
      mutate(goal = case_when(subindicator == "Diagnosed" ~ .95,
                              subindicator == "On treatment" ~ .95^2,
                              subindicator == "Virally suppressed" ~ .95^3)) 


# VIZ ---------------------------------------------------------------------

  df_phia %>% 
    ggplot(aes(value, country_lab, color = color)) +
    geom_vline(aes(xintercept = goal), linetype = "dotted", color = trolley_grey) +
    geom_linerange(aes(xmin=confidence_lower, xmax=confidence_upper, color = color), alpha = .25, size = 2) +
    geom_point(aes(fill = color), shape = 21, color = grey30k, stroke = 0.25, size = 3) +
    annotate("rect", xmin = 0, xmax = .95, ymin = .5, ymax = Inf, fill = "#808080", alpha = .05) +
    facet_grid(prev_group ~ fct_rev(subindicator_color), scales = "free_y") +
    expand_limits(x = c(0, 1)) +
    scale_x_continuous(labels = percent, expand = c(.005, .005)) +
    scale_color_identity(guide = "none", aesthetics = c("color", "fill")) +
    labs(x = NULL, y = NULL, color = NULL, fill = NULL,
         title = "PHIA SURVEY RESULTS SHOW MUCH GROUND TO BE MADE UP TO REACH 95-95-95",
         caption = glue("Source: PHIA 90-90-90 Lab or self-reported ARV [{date_pulled}]
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"),
         size = element_blank()) +
    si_style_xgrid() +
    theme(panel.spacing = unit(1, "line"),
          strip.text.x = element_markdown(family = "Source Sans Pro SemiBold"),
          legend.position = "none")
  
  
  si_save("Images/03c_phia_90s.png") 
  si_save("Graphics/03c_phia_90s.svg")
  
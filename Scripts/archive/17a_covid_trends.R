# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  COVID trends
# LICENSE:  MIT
# DATE:     2021-06-01
# UPDATED:  2021-12-06

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
  library(lubridate)
  library(COVIDutilities)


# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam")

  #quarter starts
    qtrs <- seq.Date(as.Date("2019-10-01"), today(), by = "3 months")

  #current quarter
    curr_qtr <- source_info(return = "period")
    curr_qtr_start <- curr_qtr %>% convert_qtr_to_date() %>% as.Date()
    curr_qtr_end <- curr_qtr %>% convert_qtr_to_date() %>% as.Date() %m+% months(3)
    
# IMPORT ------------------------------------------------------------------
  
  #Government Response (Oxford - https://covidtracker.bsg.ox.ac.uk/about-api)
    df_stringency <- pull_stringency_index()
    
  #COVID cases (JHU)
    df_covid <- pull_jhu_covid()

# VIZ ---------------------------------------------------------------------

  df_viz <- df_covid %>% 
    tidylog::left_join(df_stringency)
  
  df_viz <- df_viz %>% 
    group_by(countryname) %>% 
    mutate(max_val = max(daily_cases, na.rm = TRUE)) %>% 
    ungroup()
  
  #clean up Kazakhstan misentries
  df_viz <- df_viz %>% 
    mutate(daily_cases = ifelse(daily_cases <=0, NA, daily_cases),
           daily_cases = ifelse(countryname == "Kazakhstan" & date == "2021-07-23", NA, daily_cases))
  
  viz_ous <- df_viz %>% 
    filter(max_val > 1000,
           !countryname %in% c("China"),
           date >= "2020-03-01") %>%
    group_by(countryname) %>% 
    summarise(max_val = max(max_val, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(desc(max_val)) %>% 
    slice_max(order_by = max_val, n = 30) %>% 
    pull(countryname)
    
    
  df_viz %>% 
    filter(countryname %in% viz_ous) %>% 
    ggplot(aes(date, daily_cases)) +
    annotate(geom = "rect",
             xmin = curr_qtr_start,
             xmax = curr_qtr_end,
             ymin = 0,
             ymax = Inf,
             color = trolley_grey_light, alpha = .1) +
    geom_col(fill = burnt_sienna, alpha = .8, na.rm = TRUE) +
    geom_hline(aes(yintercept = 0), size = 0.5, color = grey20k) +
    geom_line(aes(y = rollingavg_7day), color = si_palettes$burnt_siennas[7], #size = 1,
              na.rm = TRUE) +
    # geom_vline(xintercept = qtrs, size = 0.5, color = grey20k) +
    geom_rug(aes(color = color), sides="b", na.rm = TRUE) +
    facet_wrap(~fct_reorder(countryname, daily_cases, max, na.rm = TRUE, .desc = TRUE), scales = "free_y") + 
    scale_y_continuous(label = comma) +
    scale_x_date(date_labels = "%b '%y",
                 breaks = c(as.Date("2020-03-01"), today())) +
    scale_color_identity() +
    labs(x = NULL, y = NULL, fill = "Stringency Index",
         title = glue("EVEN AT {curr_qtr}, COVID IS STILL LOOMING LARGE IN MANY PEPFAR COUNTRIES, LIKELY IMPACTING \nMER RESULTS AND COLLECTION"),
         subtitle = "Limited to countries that ever experienced more than 1,000 daily cases",
         caption = glue("Source: Source: JHU COVID-19 feed +  stringency index from Blavatnik School of Government at Oxford University [{today()}]
                      SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_nolines() +
    theme(axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(size = 8),
          plot.caption = element_text(size = 7),
          panel.spacing.x = unit(.5, "line"),
          panel.spacing.y = unit(.5, "line"))
  
  
  si_save("Graphics/17_covid_ctry_trends.svg")
  
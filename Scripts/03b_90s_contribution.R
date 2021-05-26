# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  90s/95s contribution 
# LICENSE:  MIT
# DATE:     2021-05-12
# UPDATED:  2021-05-26

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
  library(lubridate)
  
source("Scripts/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam")

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd()   

# MUNGE -------------------------------------------------------------------
  
  #identify period
  curr_pd <- identifypd(df)
  # curr_yr <- identifypd(df, "year")
  curr_qtr <- identifypd(df, "quarter")
    
  #aggregate to agency level for select indicators and reshape
  df_agg <- df %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_PVLS"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator","Age/Sex/ARVDispense/HIVStatus"),
           otherdisaggregate %in% c(NA, "ARV Dispensing Quantity - 3 to 5 months",
                                    "ARV Dispensing Quantity - 6 or more months")) %>% 
    clean_indicator() %>% 
    mutate(indicator = ifelse(standardizeddisaggregate == "Age/Sex/ARVDispense/HIVStatus", "TX_MMDo3", indicator)) %>% 
    group_by(fundingagency, fiscal_year, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd() %>% 
    arrange(indicator, period) %>% 
    select(-period_type)

  #create CY date
  df_agg <- df_agg %>% 
    mutate(period_date = period %>% 
             str_remove("FY") %>% 
             yq() %m-% months(3)) %>% 
    filter(year(period_date) == max(year(period_date)))
  
  #create cumulative where app
  df_agg <- df_agg %>% 
    group_by(indicator) %>% 
    mutate(cy_cumulative = case_when(str_detect(indicator, "HTS") ~ cumsum(value),
                                     TRUE ~ value)) %>% 
    ungroup() %>% 
    filter(period == max(period))

  curr_cy <- df_agg %>% 
    distinct(period_date) %>% 
    pull() %>% 
    year()
  
  
  #round numbers
  df_agg <- df_agg %>% 
    mutate(round = case_when(indicator == "HTS_TST" ~ comma(cy_cumulative/1000000, 1),
                             TRUE ~ comma(cy_cumulative/1000000, .1)))

  #reshape
  df_agg <- df_agg %>% 
    mutate(type = ifelse(indicator %in% c("HTS_TST_POS", "TX_MMDo3", "TX_PVLS_D"), "extra", "value"),
           indicator = case_when(indicator == "HTS_TST_POS" ~ "HTS_TST", 
                                 indicator == "TX_MMDo3" ~ "TX_CURR",
                                 indicator == "TX_PVLS_D" ~ "TX_PVLS",
                                 TRUE ~ indicator)) %>%
    select(-value) %>% 
    spread(type, cy_cumulative) %>% 
    mutate(vlc = case_when(indicator == "TX_PVLS" ~ value/extra))
  
  #clean number
  df_agg <- df_agg %>% 
    mutate(across(c(extra, value), ~ clean_number(., 1)),
           across(c(extra, value), ~ str_replace(., "\\.[:digit:]K", " thousand")),
           across(c(extra, value), ~ str_replace(., "M", " million")),
           vlc = percent(vlc, 1))
    


# VIZ ---------------------------------------------------------------------

  #text
  pd_text <- case_when(curr_qtr == 2 ~ "During the first three months of",
                       curr_qtr == 3 ~ "Half way through",
                       curr_qtr == 4 ~ "Through nine months of",
                       curr_qtr == 1 ~ "In")
  
  #values for viz
  df_viz <- df_agg %>% 
    mutate(x = .1,
           y = .5,
           text = case_when(indicator == "HTS_TST" ~ 
                              glue("{pd_text} {curr_cy}, USAID administered<br>
                                   <span style='color:{old_rose}'>**{value}** HIV tests</span>, helping <span style='color:{old_rose}'>**{extra} people**</span> to learn<br>
                                   their <span style='color:{old_rose}'>HIV-positive</span> status."),
                            indicator == "TX_CURR" ~
                              glue("Around the world, USAID currently supports life-saving<br>
                                   <span style='color:{old_rose}'>HIV treatment</span> for <span style='color:{old_rose}'>**{value} people**</span>, of which <span style='color:{old_rose}'>**{extra}<br>
                                    people**</span> are receiving <span style='color:{old_rose}'>multi-month drug dispensing</span>."),
                            indicator == "TX_PVLS" ~
                              glue("And of these, <span style='color:{old_rose}'>**{vlc}**</span> are <span style='color:{old_rose}'>virally suppressed</span> ensuring<br>
                                   better health and reduced onward transmission of HIV.")))
  
  
  
  df_viz %>% 
    ggplot() +
    geom_richtext(aes(x, y, label = text),
                  family = "Source Sans Pro", color = denim, size = 26/.pt, hjust = 0,
                  fill = NA, label.color = NA, # remove background and outline
                  label.padding = grid::unit(rep(0, 4), "pt")) + # remove padding 
    facet_grid(indicator ~., scales = "free_y") +
    expand_limits(x = c(0, 40), y = c(0, 1)) +
    labs(x = NULL, y = NULL,
         caption = glue("Note: Data represent calendar year
                        Source: {msd_period(period = curr_pd)}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_nolines() +
    theme(strip.text = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.spacing.y = unit(1, "pt"),
          # plot.background = element_rect(fill = si_palettes$denims[1]),
          plot.caption = element_text(color = "grey40"))
  
  
  
  si_save("Images/03b_usaid_mer_90s.png") 
  si_save("Graphics/03b_usaid_mer_90s.svg") 
  

  
# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  MMD
# LICENSE:  MIT
# DATE:     2021-05-14
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
  
  authors <- c("Aaron Chafetz", "Tim Essam")

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd()   


# MUNGE MMD ---------------------------------------------------------------

  #keep just TX_CURR/MMD and reshape
  df_mmd <- df %>% 
    filter(fundingagency == "USAID",
           indicator == "TX_CURR",
           operatingunit != "South Africa",
           fiscal_year >= 2020,
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>% 
    mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                         TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
                                         )) %>%
    group_by(fiscal_year, countryname, indicator, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd() %>% 
    filter(value > 0)
  
  #create group for o3mo and o6mo via reshaping for plotting
  df_mmd <- df_mmd %>% 
    mutate(countryname = recode(countryname,
                                "Democratic Republic of the Congo" = "DRC",
                                "Dominican Republic" = "DR")) %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = otherdisaggregate) %>% 
    rowwise() %>% 
    mutate(#unknown = total - sum(`Less than 3 months`, `3 to 5 months`, `6 or more months`, na.rm = TRUE),
           #unknown = ifelse(unknown < 0, 0, unknown),
           o3mmd = sum(`3 to 5 months`, `6 or more months`, na.rm = TRUE)
           ) %>%
    ungroup() %>% 
    rename(o6mmd = `6 or more months`) %>% 
    select(-`Less than 3 months`, -`3 to 5 months`) %>% 
    pivot_longer(-c(period, countryname, indicator, total), 
                 names_to = "otherdisaggregate",
                 values_to = "tx_mmd") %>% 
    rename(tx_curr = total) 
  

# MMD FOR AGENCY ROLL UP --------------------------------------------------

  #aggregate up to agency level
  df_mmd_agency <- df_mmd %>% 
    mutate(otherdisaggregate = recode(otherdisaggregate,
                                      "o3mmd" = "MMD - 3 months or more",
                                      "o6mmd" = "MMD - 6 months or more")) %>% 
    group_by(period, otherdisaggregate) %>% 
    summarise(across(c(tx_curr, tx_mmd), sum,na.rm = TRUE)) %>% 
    ungroup() %>%
    mutate(share = tx_mmd / tx_curr)
  
  #adjust for viz
  df_mmd_agency <- df_mmd_agency %>% 
    mutate(bar_color = ifelse(otherdisaggregate == "MMD - 3 months or more", scooter, genoa),
           otherdisaggregate_md = glue("<span style='color:{bar_color}'>{otherdisaggregate}</span>"))

# MMD FOR COUNTRY TRENDS --------------------------------------------------

  #country trends for just o3mo
  df_mmd_ou <- df_mmd %>% 
    filter(otherdisaggregate == "o3mmd") %>%
    arrange(countryname, period) 
  
  #add on USAID total rows
  df_mmd_ou <- df_mmd_ou  %>%
    mutate(countryname = "USAID") %>% 
    group_by(period, countryname, indicator, otherdisaggregate) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
    bind_rows(df_mmd_ou, .)
    
  #create share on +3mo
  df_mmd_ou <- df_mmd_ou %>% 
    mutate(share = tx_mmd/tx_curr) 
  
  #data points for plotting
  df_mmd_ou <- df_mmd_ou %>% 
    mutate(max_tx = ifelse(period == max(period), tx_curr, 0),
           max_mmd = ifelse(period == max(period), tx_mmd, 0)) %>% 
    group_by(countryname) %>% 
    mutate(endpoints = case_when(period %in% c(max(period), min(period))~share),
           max_tx = max(max_tx),
           max_mmd = max(max_mmd)) %>% 
    ungroup() %>% 
    mutate(country_lab = case_when(max_tx == max(max_tx) ~ 
                                     glue("{countryname}<br><span style = 'font-size:8pt'>{clean_number(max_mmd)} / {clean_number(max_tx)} <span style = 'font-size:6pt'>(+3 MMD/TX_CURR)</span>"),
                                   TRUE ~ glue("{countryname}<br><span style = 'font-size:8pt'>{clean_number(max_mmd)} / {clean_number(max_tx)}</span>"))) %>% 
    filter(max_tx > 0)
  
  #identify the MMD share for the largets 9 countires
  top <- df_mmd_ou %>% 
    filter(countryname != "USAID",
           period == max(period)) %>% 
    arrange(desc(tx_curr)) %>% 
    slice_max(n = 11, order_by = tx_curr) %>% 
    summarise(across(c(tx_curr, tx_mmd), sum, na.rm = TRUE),
              n = n()) %>% 
    mutate(share = percent(tx_mmd/tx_curr, 1))
  
  #top focus countries
  top_cntry <- df_mmd_ou %>% 
    filter(period == max(period)) %>% 
    slice_max(order_by = tx_curr, n = top$n + 1) %>% 
    pull(countryname)
  
# VIZ ---------------------------------------------------------------------
  
  msd_source <- source_info()
  
  df_mmd_agency %>% 
    ggplot(aes(period, tx_mmd)) + 
    geom_col(aes(y = tx_curr), fill = trolley_grey_light, alpha = .5) +
    geom_col(aes(fill = bar_color)) +
    geom_text(aes(label = percent(share, 1)), vjust = -1,
                  family = "Source Sans Pro", color = trolley_grey) +
    geom_errorbar(aes(ymax = tx_curr, ymin = tx_curr), color = trolley_grey) +
    # facet_wrap(~otherdisaggregate_md) +
    facet_wrap(~otherdisaggregate) +
    scale_fill_identity() +
    scale_y_continuous(labels = label_number_si(),
                       position = "right", expand = c(.005, .005)) +
    labs(x = NULL, y = NULL,
         title = "USAID HAS WORKED TO ENSURE MORE PATIENTS HAVE ACCESS TO MULTI MONTH DISPENSING (MMD)",
         subtitle = "South Africa, representing a third of USAID's treatment portfolio, has been excluded",
         caption = glue("MMD 3 months or more = 3-5 months and 6 months or more  
                        Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid() +
    theme(legend.position = "none",
          # strip.text.x = element_markdown(family = "Source Sans Pro SemiBold", size = 13)
          strip.text.x = element_text(family = "Source Sans Pro SemiBold", size = 13))
  

  si_save("Graphics/11a_mmd_trends.svg")  
  
  

  
  df_mmd_ou %>%
    filter(countryname %in% top_cntry) %>%
    mutate(clr = ifelse(countryname == "USAID", genoa, scooter)) %>% 
    ggplot(aes(period, share, group = country_lab, color = clr, fill = clr)) +
    geom_area(alpha = .4, size = .9) +
    geom_point(aes(y = endpoints), na.rm = TRUE) +
    facet_wrap(~fct_reorder(country_lab, tx_curr, max, .desc = TRUE)) +
    scale_y_continuous(label = percent, 
                       breaks = seq(0, 1, .5)) +
    scale_x_discrete(breaks = c("FY20Q1", "FY20Q3", "FY21Q1", "FY21Q3")) +
    scale_color_identity(aesthetics = c("color","fill")) +
    labs(x = NULL, y = NULL,
         title = glue("IN {max(df_mmd_ou$period)}, USAID HAS {top$share} OF TREATMENT PATIENTS ON +3 MONTHS OF MMD IN THE LARGEST {top$n} COUNTRIES"),
         subtitle = "South Africa, representing a third of USAID's treatment portfolio, has been excluded",
         caption = glue("MMD 3 months or more = 3-5 months and 6 months or more  | Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing.y = unit(.5, "line"),
          panel.spacing.x = unit(.5, "line"),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 7),
          panel.grid.major.y = element_line(color = "#E8E8E8"),
          panel.grid.minor.y = element_line(color = "#E8E8E8"),
          strip.text = element_markdown())

  si_save("Graphics/11b_mmd_trends_by_country.svg")  

 
  
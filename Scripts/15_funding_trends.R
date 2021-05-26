# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  funding trends highlights
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
  
  authors <- c("Aaron Chafetz", "Tim Essam")

  fsd_source <- "FSD COP17-20 [2021-05-14]"
    
# IMPORT ------------------------------------------------------------------
  
  df_fsd <- si_path() %>% 
    return_latest("Financial_Structured_Dataset_COP17") %>% 
    read_msd()   
  
  df_msd <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd() 

  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()

# MUNGE -------------------------------------------------------------------

  #current period
  curr_qtr <- identifypd(df_msd, "quarter")
  curr_year <- identifypd(df_msd, "year")
  
  msd_source <- df_msd %>% 
    identifypd() %>% 
    msd_period(period = .)
  
  #remove M&O & keep PEPFAR and USAID
  df_fsd <- df_fsd %>% 
    bind_rows(df_fsd %>% mutate(fundingagency = "PEPFAR")) %>% 
    filter(record_type != "Management and Operations",
           fundingagency %in% c("USAID", "PEPFAR"))
  
  #agg fiscal year totals
  df_cop_tot <- df_fsd %>% 
    group_by(fiscal_year,fundingagency) %>% 
    summarise(across(cop_budget_total, sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(fiscal_year) %>% 
    mutate(share = case_when(fundingagency == "USAID" ~ cop_budget_total/lag(cop_budget_total)),
           total = case_when(fundingagency == "PEPFAR" ~ cop_budget_total)) %>% 
    ungroup()
  
  #C&T only budget
  df_ct <- df_fsd %>% 
    filter(program == "C&T",
           fundingagency == "USAID") %>% 
    count(fiscal_year, program, wt = cop_budget_total, name = "cop_budget_total")

  #Treatment data to combine with C&T budget
  df_tx <- df_msd %>% 
    bind_rows(df_arch) %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("TX_NEW", "TX_CURR"),
           standardizeddisaggregate == "Total Numerator") %>% 
    count(fiscal_year, indicator, wt = cumulative, name = "cumulative")

  #create Net New
  df_nn <- df_tx %>% 
    filter(indicator == "TX_CURR") %>% 
    mutate(net_new = cumulative - lag(cumulative, order_by = fiscal_year)) %>% 
    select(-cumulative)
  
  #bind net new on to TX data
  df_tx <- df_tx %>% 
    left_join(df_nn)
  
  #adj for viz
  df_tx <- df_tx %>% 
    mutate(alpha_fill = case_when(fiscal_year == curr_year & curr_qtr != 4 ~ .6,
                             TRUE ~ 1),
           txcurr_lab = case_when(indicator == "TX_CURR" ~ clean_number(cumulative, 1)))
  
  
# VIZ ---------------------------------------------------------------------

  #USAID Total budget out of PEPFAR
  df_cop_tot %>% 
    ggplot(aes(fiscal_year, cop_budget_total, fill = fundingagency)) + 
    geom_col(aes(fill = fundingagency, alpha = fundingagency), position = "identity") +
    geom_text(aes(label = percent(share, 1)), na.rm = TRUE, vjust = -1, 
              family = "Source Sans Pro SemiBold", color = trolley_grey) +
    geom_errorbar(aes(ymin = total, ymax = total), color = trolley_grey_light, 
                  size = .9, na.rm = TRUE) +
    scale_fill_manual(values = c("USAID" = genoa, "PEPFAR" = trolley_grey_light)) +
    scale_alpha_manual(values = c("USAID" = .8, "PEPFAR" = .4)) +
    scale_y_continuous(labels = unit_format(1, unit = "B", scale = 1e-9),
                       #breaks = seq(0, 1.25e6,.25e6), 
                       position = "right", expand = c(.005, .005)) +
    labs(x = NULL, y = NULL, fill = NULL, alpha = NULL,
         title = "USAID'S BUDGET AND SHARE OF PEPFAR BUDGET HAS REMAINED CONSISTENT OVER THE LAST FEW YEARS",
         subtitle = "Annual Country Operation Plan (COP) budgets including commodities",
         caption = glue("Note: M&O excluded
                        Source: {fsd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid()
  
  
  si_save("Images/15a_fundingtrends.png")
  si_save("Graphics/15a_fundingtrends.svg")  
  
  
  #C&T budget trends
  v1 <- df_ct %>% 
    ggplot(aes(fiscal_year, cop_budget_total)) +
    geom_blank(aes(y = cop_budget_total *1.01)) +
    geom_col(fill = burnt_sienna) +
    geom_hline(yintercept = 0, color = trolley_grey) +
    geom_text(aes(label = clean_number(cop_budget_total, 2)),
              vjust = -.9, color = burnt_sienna, family = "Source Sans Pro SemiBold") +
    labs(x = NULL, y = NULL,
         subtitle = "Flat Care and Treatment Budgets") +
    si_style_nolines() +
    theme(axis.text.y = element_blank())
  
  #Treatment trends
  v2 <- df_tx %>% 
    filter(fiscal_year >= 2018) %>% 
    ggplot(aes(fiscal_year, cumulative, fill = indicator, alpha = alpha_fill)) +
    geom_blank(aes(y = cumulative * 1.05)) +
    geom_col(position = "identity", width = .85) +
    geom_errorbar(aes(ymin = net_new, ymax = net_new), 
                  width = .9, size = .9, color = golden_sand) +
    geom_text(aes(label = txcurr_lab), vjust = -.9, na.rm = TRUE,
              color = denim, family = "Source Sans Pro SemiBold") +
    geom_hline(yintercept = 0, color = trolley_grey) +
    expand_limits(x = 2022) +
    scale_fill_manual(values = c("TX_CURR" = denim, "TX_NEW" = scooter)) +
    scale_alpha_identity() +
    labs(x = NULL, y = NULL, fill = NULL,
         subtitle = "Gains to Treatment Portfolio") +
    si_style_nolines() +
    theme(axis.text.y = element_blank(),
          legend.position = "none")
  
  #combine
  v1 + v2 + plot_annotation(
    title = "USAID PROVIDING FOR MORE PATIENTS EACH YEAR DESPITE A FLAT BUDGET FOR CARE AND TREATMENT",
    caption = glue("Source: {msd_source} + {fsd_source}
                      SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")
  ) & 
    theme(plot.title = element_text(family = "Source Sans Pro",
                                    size = 14,
                                    face = "bold",
                                    color =  "#202020",
                                    hjust = 0),
          plot.caption = element_text(family = "Source Sans Pro",
                                      size = 9,
                                      color = "#909090",
                                      hjust = 1, vjust = 1))
  
  si_save("Images/15b_ct_budget_usaid.png")
  si_save("Graphics/15b_ct_budget_usaid.svg")
  
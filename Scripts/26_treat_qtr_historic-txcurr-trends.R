# PROJECT:  agiprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  treatment scale up since PEPFAR start
# LICENSE:  MIT
# DATE:     2021-11-30
# UPDATED:  2024-12-09
# NOTE:     adapted from agitprop/04a_usaid_tx_trends.R

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(systemfonts)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(janitor)
  library(lubridate)


# GLOBAL VARIABLES --------------------------------------------------------
  #setwd("~/GitHub/agitprop")
  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  
 metadata <-  si_path() %>% 
    return_latest("OU_IM_FY22") %>% 
    get_metadata()
 
 data_folder <- "Data"
  

# IMPORT ------------------------------------------------------------------
  
  # lets ignore the very old historic results and focus on FY15 to present
  
  # Source: PEPFAR Spotlight (public)
  df_hist <- data_folder %>% 
   return_latest("Country and Regional Targets") %>% 
   read_csv(na = c("", "NA", "null"),
                      col_types = c(Year = "i",
                                    `Measure Value` = "d",
                                    .default = "c")) %>%
    clean_names()
  
  #Current MSD
    df_msd <- si_path() %>% 
    return_latest("OU_IM_FY22") %>% 
    read_psd()
    
    
  
  #Archived MSD - download historic FY15 MSD from Pano
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_psd()

# MUNGE -------------------------------------------------------------------

  #append archive MSD to current version
  df_tx <- df_msd %>% 
    bind_rows(df_arch) 
  
  #exclude Nigeria due to 2023 data issue
  # df_tx<-df_tx %>% 
  #   filter(operatingunit!="Nigeria")
  
  #create a PEPFAR duplicate and aggregate up to country/global level
  df_tx <- df_tx %>%
    bind_rows(df_tx %>% mutate(funding_agency = "PEPFAR")) %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, funding_agency, indicator) %>% 
    summarise(value = sum(cumulative, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(funding_agency %in% c("PEPFAR", "USAID"),
           value != 0) %>% 
    arrange(indicator, funding_agency, fiscal_year) %>% 
    mutate(source = metadata$source)
  


  # limit the historic data for TX_CURR/NEW data and aggregate
  df_hist_clean <- df_hist %>%
    # filter(country_region!="Nigeria") %>% 
    filter(indicator_short_name %in% c("Patients Currently Receiving ART",
                                       "Patients Newly Receiving ART"),
           measure_name == "Results",
           country_region != "Global",
           dsd_ta == "DSD+TA") %>%
    group_by(fiscal_year = year, indicator = indicator_short_name) %>%
    summarise(value = sum(measure_value, na.rm = TRUE)) %>%
    ungroup()

  # clean align names and remove any overlapping years
  df_hist_clean <- df_hist_clean %>%
    mutate(indicator = recode(indicator,
                              "Patients Currently Receiving ART" = "TX_CURR",
                              "Patients Newly Receiving ART" = "TX_NEW"),
           funding_agency = "PEPFAR",
           source = "Spotlight") %>%
    filter(!fiscal_year %in% unique(df_tx$fiscal_year))

  # combine historic data to MSD
  df_tx <- bind_rows(df_hist_clean, df_tx)

  #adjust for viz
  df_tx_viz <- df_tx %>% 
    mutate(bar_alpha = ifelse(fiscal_year == max(fiscal_year) & metadata$curr_qtr != 4, .6, 1),
           ind_label = case_when(indicator == "TX_CURR" ~ "Currently receiving antiretroviral therapy",
                                 TRUE ~ "Newly enrolled on antiretroviral therapy"))
  #add usaid share
  df_tx_viz <- df_tx_viz %>% 
    pivot_wider(names_from = funding_agency) %>% 
    mutate(usaid_share = USAID/PEPFAR) %>%
    pivot_longer(c(USAID, PEPFAR), names_to = "funding_agency",
                 values_drop_na = TRUE) %>% 
    mutate(usaid_share = case_when(funding_agency == "USAID" ~ usaid_share)) %>% 
    arrange(indicator, funding_agency, fiscal_year)
  
  #data point for context
  df_curr_val <- df_tx_viz %>% 
    filter(fiscal_year == max(fiscal_year),
           indicator == "TX_CURR") %>%
    select(fiscal_year, indicator, funding_agency, value) %>% 
    pivot_wider(names_from = funding_agency) %>% 
    mutate(usaid_share = USAID/PEPFAR)

# VIZ ---------------------------------------------------------------------

  v <- df_tx_viz %>% 
    filter(indicator == "TX_CURR",
           funding_agency == "PEPFAR") %>% 
    ggplot(aes(fiscal_year, value)) +
    geom_col(aes(alpha = bar_alpha, fill = fct_rev(funding_agency)),
             position = "identity") +
    # geom_hline(yintercept = seq(3e6, 18e6, 3e6), color = "white") +
    scale_y_continuous(labels = unit_format(1, unit = "M", scale = 1e-6),
                       breaks =  seq(3e6, 3e7, 3e6),
                       position = "right", expand = c(.005, .005)) +
    geom_text(aes(label = clean_number(value)), na.rm = TRUE,
              family = "Source Sans Pro", color = trolley_grey, vjust = -1) +
    # scale_x_continuous(expand = c(.005, .005))+
    scale_x_continuous(
      breaks = seq(2002, 2024, by = 2),    # Show labels for every 2 years
      labels = seq(2002, 2024, by = 2),    # Use the same year labels
      expand = c(.005, .005)
    ) +
    scale_fill_manual(values = c("PEPFAR" = moody_blue_light, "USAID" = moody_blue)) +
    scale_alpha_identity() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "PEPFAR HAS VASTLY SCALED UP LIFE SAVING ART IN THE LAST 15+ YEARS",
         subtitle = glue("As of {metadata$curr_pd}, PEPFAR provided treatment to almost \\
                          {number(df_curr_val$PEPFAR, scale = 1e-6)} million \\
                          patients living with HIV."),
         caption = glue("Source: Spotlight FY04-14, {metadata$source} (including FY15-18)
                        Note: Excludes Nigeria in FY23 due to data quality issues" ))+
    si_style_nolines() +
    theme(legend.position = "none")

  v_ann <- v +
    # annotate("text",
    #          x = 2014.2, y = 8.9e6, family = "Source Sans Pro",
    #          hjust = "right", size = 9/.pt, color = matterhorn,
    #          label = "No agency attribution\n of results before 2015") +
    annotate("text",
             x = 2018.5, y = 17e6, family = "Source Sans Pro",
             hjust = "left",
             color = matterhorn, size = 10/.pt,
             label = "All of PEPFAR") +
    annotate("curve",
             x = 2018.4, y = 17e6, xend = 2018, yend = 14e6,
             arrow = arrow(length = unit(0.05, "inches"),
                           type = "closed"),
             curvature = .4,
             color = suva_grey)
  #+
    # annotate("text",
    #          x = 2019.9, y = -1e6, family = "Source Sans Pro",
    #          hjust = "right",
    #          color = matterhorn, size = 10/.pt,
    #          label = "USAID") +
    # annotate("curve",
    #          x = 2019.4, y = -1e6, xend = 2019, yend = 1e6,
    #          arrow = arrow(length = unit(0.05, "inches"),
    #                        type = "closed"),
    #          curvature = .4,
    #          color = "white")

  #si_save("Graphics/26_treat_qtr_historic-txcurr-trends-4-22.svg")
  

  init_or_clean<-ifelse(str_detect(metadata$source, "i"), "i",
                    ifelse(str_detect(metadata$source, "c"), "c", "error"))
  
#  glue("Images/26_treat_qtr_historic-txcurr-trends-no-percent_{curr_pd}{init_or_clean}") %>%
#  si_save()
  

  v_ann +
    geom_text(data = . %>% filter(fiscal_year != max(fiscal_year)),
              aes(label = percent(usaid_share, 1)), na.rm = TRUE,
              family = "Source Sans Pro", color = "white", vjust = 2) +
    geom_text(data = . %>% filter(fiscal_year == max(fiscal_year)),
              aes(label = percent(usaid_share, 1)), na.rm = TRUE,
              family = "Source Sans Pro", color = moody_blue, vjust = -.8)

  
  #si_save("Graphics/27_treat_qtr_historic-txcurr-trends-share-4-22.svg")
  glue("Images/26_treat_qtr_historic-txcurr-trends_nigeria_{metadata$curr_pd}_PEPFAR") %>%
    si_save()

  
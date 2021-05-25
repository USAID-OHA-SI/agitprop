# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  scale up of prep
# LICENSE:  MIT
# DATE:     2021-05-20
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
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd()   
  
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()

# MUNGE -------------------------------------------------------------------

  #bind archived + current MSD and filter for PrEP
  df_prep <- df %>%
    bind_rows(df_arch) %>% 
    filter(fundingagency == "USAID",
           indicator == "PrEP_NEW",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year >= 2017)
  
  #curr fy prep (for viz title)
  prep_cum <- df_prep %>% 
    filter(fiscal_year >= identifypd(df, "year")) %>% 
    count(wt = cumulative)
  
  #count number of countries with PrEP
  df_cntry_cnt <- df_prep %>% 
    filter(cumulative > 0) %>% 
    distinct(fiscal_year, countryname) %>% 
    count(fiscal_year, name = "n_countries")
  
  #aggregate result to USAID level
  df_prep <- df_prep %>% 
    group_by(fiscal_year, fundingagency) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd() %>% 
    select(-period_type) %>% 
    arrange(period)
  


  
# CREATE FULL LIST OF PERIODS ---------------------------------------------

#PrEP in and out of quarterly and semi-annual reporting so need a complete set  
  #current period
  curr_pd <- identifypd(df)
  
  #current period as number
  curr_pd_num <- curr_pd %>% 
    str_remove("FY") %>% 
    str_replace("Q", ".") %>% 
    as.numeric()
  
  #identify current fiscal year for max date
  curr_fy <- str_sub(curr_pd, 3,4) %>% as.numeric()
  
  #propagate list of periods not in prep to add to df
  full_pds <- expand_grid(fiscal_year = c(17:curr_fy),
                           quarter = c(1:4)) %>% 
    unite(period, c(fiscal_year, quarter), sep = ".") %>% 
    mutate(period = as.numeric(period)) %>% 
    filter(period <= curr_pd_num) %>% 
    mutate(period = period %>% 
             paste0("FY", .) %>% 
             str_replace("\\.", "Q")) 
  
  extra_pds <- full_pds %>% 
    filter(!period %in% unique(df_prep$period))
  
  
# VIZ ---------------------------------------------------------------------
  
  msd_source <- df %>% 
    identifypd() %>% 
    msd_period(period = .)
  
  pd_breaks <- full_pds %>% 
    filter(str_detect(period, "Q1")) %>% 
    pull()
  
  df_viz <- df_prep %>% 
    bind_rows(extra_pds) %>% 
    arrange(period)
  
  df_viz %>% 
    ggplot(aes(period, value, group = fundingagency)) + 
    geom_area(fill = scooter, color = scooter, alpha = .2, size = 1, na.rm = TRUE) +
    geom_vline(xintercept = pd_breaks, color = "white", 
               size = .9, linetype = "dotted") +
    geom_point(shape = 21, fill = "white", color = scooter, stroke = 1.5, na.rm = TRUE) +
    annotate(geom = "text", x = 7, y = 1.e5, label = "DRAFT", size = 72, color = trolley_grey_light) +
    scale_y_continuous(label = clean_number, position = "right") +
    scale_x_discrete(breaks = pd_breaks, labels = str_remove(pd_breaks, "Q1")) +
    labs(x = NULL, y = NULL, 
         title = glue("USAID has initiated {clean_number(prep_cum, 1)} \\
                      onto PrEP this year across \\
                      {filter(df_cntry_cnt, fiscal_year == max(fiscal_year)) %>% pull()} \\
                      countries, up from {filter(df_cntry_cnt, fiscal_year == 2017) %>% pull()} \\
                      in 2017") %>% toupper,
         subtitle = "Pre-Exposure Prophylaxis (PrEP) Quarterly Results",
         caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid()
    
  si_save("Images/09_prep_scaleup.png")  
  
  si_save("Graphics/09_prep_scaleup.svg", scale = 1.2, 
          width = 10, height = 7)  
  
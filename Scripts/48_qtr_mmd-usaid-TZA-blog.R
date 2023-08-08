# PROJECT:  agitprop
# AUTHOR:   N.Petrovic | USAID
# PURPOSE:  MMD
# REF ID:   38d6fb8b
# LICENSE:  MIT
# DATE:     2023-07-14
# NOTE:     adapted from 29_treat_qtr_mmd-usaid.R to use Spotlight 
#           data only and TZ only


# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(vroom)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(gophr)
  
# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Nada Petrovic")

  # Reference ID to be used for searching GitHub
  ref_id <- "38d6fb8b"
  
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  
  
  # IMPORT ------------------------------------------------------------------
  
  df_all <- file.path(si_path(), "Spotlight_TZ_MMD_Comms_Req.zip") %>%
    vroom()  

  
  # MUNGE -------------------------------------------------------------------
  
  #glimpse(df)
  
  df <-df_all %>%
     filter(Country == "Tanzania",
           str_detect(Indicator, "TX_CURR")) %>%
    select(-c(`Numerator (Operand)`, Description, `Operating Unit`, ends_with("T"))) %>%
    pivot_longer(where(is.double),
                 names_to = "period") %>%
    pivot_wider(names_from = Indicator) %>%
    filter(substring(period,1,4)>=2020) %>%
    mutate(period = period %>%
             str_remove("-R") %>%
             str_replace("20", "FY"))


# MUNGE MMD ---------------------------------------------------------------

  #keep just TX_CURR/MMD
  df_mmd <- df %>% 
    #pivot_longer(cols=starts_with("TX_CURR_ARVDisp"), names_to="otherdisaggregate", values_to="tx_mmd")
    rowwise() %>% 
    mutate(o3mmd = sum(`TX_CURR_ARVDisp_three_five_mo`, `TX_CURR_ARVDisp_six_more_mo`, na.rm = TRUE)) %>%
    ungroup() %>% 
    rename(o6mmd = `TX_CURR_ARVDisp_six_more_mo`) %>% 
    rename(tx_curr = "TX_CURR") %>% 
    select(-`TX_CURR_ARVDisp_less_three_mo`, -`TX_CURR_ARVDisp_three_five_mo`) %>% 
    pivot_longer(cols = c("o6mmd","o3mmd"), 
                 names_to = "otherdisaggregate",
                 values_to = "tx_mmd") %>%
    mutate(share = tx_mmd/tx_curr) %>%
    arrange(desc(period)) %>%
    mutate(max_tx = ifelse(period == max(period) & otherdisaggregate == "o6mmd", tx_curr, 0),
           max_mmd = ifelse(period == max(period) & otherdisaggregate == "o6mmd", tx_mmd, 0)) %>% 
    mutate(endpoints = case_when(period %in% c(max(period), min(period))~share),
           max_tx = max(max_tx),
           max_mmd = max(max_mmd)) %>% 
    filter(max_tx > 0) %>% 
    mutate(fill_color = case_when(otherdisaggregate == "o6mmd" ~ scooter,
                                  TRUE ~ scooter_med),
           lab_share = case_when(period == max(period) ~ share))
  
# VIZ ---------------------------------------------------------------------
  
  #Country Trends
  df_mmd %>%
    ggplot(aes(period, share, group = otherdisaggregate, color = fill_color, fill = fill_color)) +
    geom_area(alpha = .4, size = .9, position = "identity") +
    geom_point(aes(y = endpoints), na.rm = TRUE) +
    geom_text(aes(label = percent(lab_share, 1)), na.rm = TRUE,
              hjust = -.2, vjust = .1,family = "Source Sans Bold", size=5) +
    scale_y_continuous(lim=c(0,.9), label = percent, 
                       breaks = seq(0, 1, .5), expand = c(0,0)) +
    scale_x_discrete(breaks = c("FY20Q1", "FY20Q3", "FY21Q1", "FY21Q3", "FY22Q1", "FY22Q3","FY23Q1")) +
    scale_color_identity(aesthetics = c("color","fill")) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = glue("PEPFAR RAPIDLY SCALED UP <span style = 'color: #1e87a5;'>6+ MONTH MMD</span> IN TANZANIA STARTING IN EARLY FY22"),
         subtitle = glue("Percent of individuals on <span style = 'color: #5BB5D5;'><b>3+ month<b></span> and <span style = 'color: #1e87a5;'><b>6+ month<b></span> MMD out of total cohort"),
         caption = glue("Source: https://data.pepfar.gov/datasets")) +
    si_style_ygrid() +
    theme(axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          panel.grid.major.y = element_line(color = "#E8E8E8"),
          panel.grid.minor.y = element_line(color = "#E8E8E8"),
          strip.text = element_markdown(),
          plot.caption=element_text(vjust = -2),
          plot.title = element_markdown(size = 15),
          plot.subtitle = element_markdown(size = 13))    
  
si_save(glue("Graphics/MMD_transition_TZ_blog.svg"))
si_save(glue("Graphics/MMD_transition_TZ_blog.png"))
#  si_save(glue("Images/29b_treat_qtr_mmd-countries_{curr_pd}.png"))        
  
  
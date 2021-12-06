# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  USAID KP_PREV work
# LICENSE:  MIT
# DATE:     2021-12-02
# UPDATED: 

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
  library(ggbeeswarm)
  library(ggrepel)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  
  msd_source <- source_info()
  curr_fy <- source_info(return = "fiscal_year")

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   
  
  #Archived MSD
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()
  

# MUNGE -------------------------------------------------------------------

  df_kp <- df %>%
    bind_rows(df_arch) %>% 
    filter(fundingagency == "USAID",
           indicator == "KP_PREV",
           standardizeddisaggregate == "Total Numerator",
           cumulative != 0) %>% 
    count(fiscal_year, indicator, operatingunit, wt = cumulative, name = "cumulative")

  top <- df_kp %>% 
    filter(fiscal_year == max(fiscal_year)) %>% 
    arrange(desc(cumulative)) %>% 
    mutate(cumshare = cumsum(cumulative)/sum(cumulative)) %>% 
    slice_head(n = 6) %>% 
    pull(operatingunit)
  
  df_kp <- df_kp %>% 
    mutate(lab = case_when(operatingunit %in% top & fiscal_year == max(fiscal_year) ~ operatingunit),
           fill_color = case_when(operatingunit %in% top ~ operatingunit))
  
  df_kp %>% 
    ggplot(aes(indicator, cumulative)) +
    geom_beeswarm(data = . %>% filter(operatingunit %ni% top), aes(size = cumulative), 
                  color = trolley_grey_light,  alpha = .7, cex = 5) +
    geom_beeswarm(aes(size = cumulative, color = fill_color), cex = 5, alpha = .9, na.rm = TRUE) + 
    geom_text_repel(aes(label = lab, color = fill_color),  na.rm = TRUE,
                    family = "Source Sans Pro", size = 11/.pt) +
    facet_grid(~fiscal_year) +
    scale_y_log10(label = label_number_si()) +
    scale_color_si("siei",discrete = TRUE) +
    scale_size(range = c(2, 8)) +
    si_style_ygrid() +
    labs(x = NULL, y = NULL,
         title = glue("USAID had 6 countries make up over 50% of its total key pops reached in FY{str_sub(curr_fy, start = -2)}, with significant growth from West Africa Region and Cameroon in the last few years") %>% toupper,
         subtitle = "Cumulative Key Pops Reached with Interventions (log scale)",
         caption = glue("Source: {msd_source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    theme(axis.text.x = element_blank(),
          legend.position = "none",
          panel.spacing.x = unit(.5, "lines"))

  
  si_save("Graphics/17_prev_semi_kpprev-usaid-ous.svg")    
  
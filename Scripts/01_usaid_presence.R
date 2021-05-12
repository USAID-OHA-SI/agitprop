# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  USAID footprint globally (geographic coverage)
# LICENSE:  MIT
# DATE:     2021-05-11
# UPDATED:
# NOTE:     adapted from USAID-OHA-SI/lastmile (linked below)
# URL:      https://github.com/USAID-OHA-SI/lastmile/blob/master/Scripts/99_FY20Q4_USAID_PEPFAR_Countries.R

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
  library(sf)
  library(rnaturalearth)
  library(gisr)

  source("Scripts/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  msd_source <- msd_period()
  authors <- c("Aaron Chafetz")

# IMPORT ------------------------------------------------------------------
  
  #MSD
    df_ou <- si_path() %>% 
      return_latest("OU_IM") %>% 
      read_rds()   
  
  #shapefile
    spdf <- ne_countries(type = "sovereignty", 
                         scale = 110, 
                         returnclass = "sf") %>% 
      select(sovereignt, admin, name, adm0_a3) %>% 
      filter(admin != "Antarctica") %>% # Remove Antarctica
      clean_countries(colname = "admin")
  

# MUNGE -------------------------------------------------------------------

  #current FY
    curr_fy <- identifypd(df_ou, "year")
    
  #identify all places USAID has targets
    df_cntry <- df_ou %>% 
      filter(fundingagency == "USAID",
             fiscal_year == curr_fy,
             !is.na(targets)) %>% 
      distinct(operatingunit, countrynamename)
    
  # Join MSD with shapefiles
    spdf_ou <- df_cntry %>% 
      left_join(spdf, 
                by = c("countrynamename" = "admin"))
  
  
## VIZ ---------------------------------------------------------
  
  ## Global Map
  ggplot() +
    geom_sf(data = spdf, fill = NA, color = trolley_grey, size = .4) +
    geom_sf(data = spdf_ou, aes(geometry = geometry),
            fill = "#1B68B3",
            color = "white",
            size = .2) +
    labs(title = glue("USAID SUPPORTS PEPFAR PROGRAMMING ACROSS {nrow(spdf_ou)} COUNTRIES"),
         caption = glue("Source: {msd_source}
                     SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_map() +
    theme(
      legend.direction = "horizontal",
      plot.title = element_text(hjust =  0),
      plot.caption = element_text(hjust = 1),
    )
  
  si_save("Images/01_usaid_presence.png",
          scale = 1.2, width = 10, height = 7) 
  
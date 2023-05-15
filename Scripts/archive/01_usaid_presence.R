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
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(sf)
  library(rnaturalearth)
  library(gisr)
  library(rmapshaper)
  library(gt)

  source("Scripts/archive/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam")
  
 filepath <-  si_path() %>% 
    return_latest("OU_IM_FY21")
 
 get_metadata(filepath)

# IMPORT ------------------------------------------------------------------
  
  #MSD
    df_ou <- si_path() %>% 
      return_latest("OU_IM_FY21") %>% 
      read_psd()   
  
  #shapefile
    spdf <- ne_countries(type = "sovereignty", 
                         scale = 110, 
                         returnclass = "sf") %>% 
      select(sovereignt, admin, name, adm0_a3, continent, subregion) %>% 
      filter(admin != "Antarctica") %>% # Remove Antarctica
      clean_countries(colname = "admin")
  

# MUNGE -------------------------------------------------------------------

  #source info
    msd_source <- df_ou %>% 
      identifypd() %>% 
      msd_period(period = .)
    
  #current FY
    curr_fy <- identifypd(df_ou, "year")
    
  #identify all places USAID has targets
    df_cntry <- df_ou %>% 
      filter(funding_agency == "USAID",
             fiscal_year == metadata$curr_fy,
             !is.na(targets)) %>% 
      distinct(operatingunit, country)
  
    # Define a projection to make Greenland a bit smaller & allow for zooom
    spdf <- ms_simplify(spdf, keep = 0.75)
      
    spdf_rob <- st_transform(spdf, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
  # Join MSD with shapefiles
    spdf_ou <- spdf_rob %>% 
      right_join(df_cntry, 
                by = c("admin" ="country"))
  
  
## VIZ ---------------------------------------------------------
  # continents
  cont_count <- spdf_ou %>% st_drop_geometry() %>% distinct(continent) %>% nrow()
    
  ## Global Map
  map <- ggplot() +
    geom_sf(data = spdf_rob, fill = "white", color = trolley_grey, size = .4) +
    geom_sf(data = spdf_ou, aes(geometry = geometry),
            fill = "#1B68B3",
            color = "white",
            size = .2) +
    labs(title = glue("USAID SUPPORTS PEPFAR PROGRAMMING ACROSS {cont_count} CONTINENTS IN {nrow(spdf_ou)} COUNTRIES"),
         caption = glue("Source: {metadata$source}
                     SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_map() +
      # coord_sf(xlim = c(-1e7, 1.5e7), ylim = c(-4e6, 6e6), clip = "on", expand = T) +
    theme(
      legend.direction = "horizontal",
      panel.background = element_rect(fill = "#bfddff40", color = "NA"),
      plot.title = element_text(hjust =  0),
      plot.caption = element_text(hjust = 1),
      )
  
    
  
  # Create a table at the bottom listing the countries
tbl <-  spdf_ou %>% st_drop_geometry() %>% count(continent, name) %>% 
      select(Continent = continent, 
             name) %>% 
      group_by(Continent) %>% 
      summarise(`Countries Supported` = paste(name, collapse = ", ")) %>% 
   mutate(`Countries Supported` = str_wrap(`Countries Supported`, width = 100))

      
  gt_grob <- gridExtra::tableGrob((tbl), rows = NULL, theme = gridExtra::ttheme_minimal(base_family = "Source Sans Pro",
                                                                             core = list(fg_params = list(hjust=0, x=0.1, 
                                                                                                          fontsize = 12)
                                                                                         )
                                                                             )
                                  )

  map / gt_grob
  
  ggsave("Graphics/01_usaid_presence_text.svg", plot = gt_grob, scale = 1.2, width = 10, height = 7)
    
  ggsave("Graphics/01_usaid_presence.svg", plot = map, scale = 1.2, width = 10, height = 7)
  
  
  si_save("Images/01_usaid_presence.png",
          scale = 1.2, width = 10, height = 7) 
  
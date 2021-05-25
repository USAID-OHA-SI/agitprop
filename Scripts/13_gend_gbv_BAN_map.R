# PROJECT:  agiprop
# AUTHOR:   T. Essam | USAID
# PURPOSE:  GBV support globally
# LICENSE:  MIT
# DATE:     2021-05-14
# UPDATED:  2021-05-24

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
  library(janitor)
  library(lubridate)
  
  source("Scripts/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------

  authors <- c("Aaron Chafetz", "Tim Essam")

# IMPORT ------------------------------------------------------------------

  #Current MSD
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd() %>% 
    filter(indicator == "GEND_GBV", 
           standardizeddisaggregate == "Total Numerator")
  
  spdf <- ne_countries(type = "sovereignty", 
                       scale = 110, 
                       returnclass = "sf") %>% 
    select(sovereignt, admin, name, adm0_a3, continent, subregion) %>% 
    filter(admin != "Antarctica") %>% # Remove Antarctica
    clean_countries(colname = "admin")
  
  #current FY
  curr_fy <- identifypd(df, "year")
  




  
# MUNGE -------------------------------------------------------------------  
  
  # For captions
  msd_source <- df %>% 
    identifypd() %>% 
    msd_period(period = .)
  
  # Munge countries for map
  df_gbv <- 
    df %>% 
    filter(fundingagency == "USAID", fiscal_year == 2021) %>% 
    group_by(countryname, operatingunit, fiscal_year) %>% 
    summarise(across(matches("tar|cumul"), sum, na.rm = T))

  
   df_gbv_glb <-  df %>% 
    filter(fundingagency == "USAID") %>% 
    group_by(fiscal_year) %>% 
    summarise(across(matches("tar|cumul"), sum, na.rm = T)) %>% 
    mutate(ach = cumulative/targets,
          fill_col = if_else(fiscal_year == 2021, "#fd9873", burnt_sienna)) 
   
   fy21_ach <- df_gbv_glb %>% filter(fiscal_year == 2021) %>% pull(cumulative)/1e3
  
   
   # Define a projection to make Greenland a bit smaller & allow for zooom
   spdf <- ms_simplify(spdf, keep = 0.75) %>% 
     st_transform(., "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
   
   spdf_renamed <- spdf %>% 
     mutate(sovereignt = case_when(
       sovereignt == "Ivory Coast" ~ "Cote d'Ivoire",
       sovereignt == "Swaziland" ~ "Eswatini",
       sovereignt == "United Republic of Tanzania" ~ "Tanzania",
       TRUE ~ sovereignt
     ))
   
   # Check PEPFAR OUs to the shapefile
   intersect(df_gbv %>% ungroup() %>% distinct(countryname) %>% pull(), 
             spdf_renamed %>% st_drop_geometry() %>% distinct(sovereignt) %>% 
               pull()) %>% 
     length() == dim(df_gbv)[1]
   
   cntry_count <- df_gbv %>% distinct(countryname) %>% pull() %>% length()
   
   gbv_geo <- 
     spdf_renamed %>% 
     left_join(., df_gbv, by = c("sovereignt" = "countryname"))

   
# PLOT --------------------------------------------------------------------
  
  # High level, what does it look like?
   
    df_gbv_glb %>% 
    ggplot(aes(x = fiscal_year)) +
    geom_col(aes(y = targets), fill = grey10k) +
    geom_col(aes(y = cumulative, fill = fill_col)) +
      geom_hline(yintercept = seq(1e5, 3e5, by = 1e5), color = "white", size = 0.5) +
      geom_text(aes(y = cumulative, label = percent(ach, 1)), size = 12/.pt, font = "SourceSansPro", color = color_plot_text, vjust = -0.25)+
    si_style_xline() +
    scale_fill_identity() +
      scale_y_continuous(labels = unit_format(1, unit = "K", scale = 1e-3),
                         position = "right", expand = c(.005, .005)) +
      scale_x_continuous(expand = c(.005, .005), breaks = c(2019, 2020, 2021)) +
      labs(x = NULL, y = NULL, title = glue("USAID HAS PROVIDED POST-GENDER BASED VIOLENCE (GBV) CLINICAL CARE TO MORE THAN {comma(fy21_ach, 1)}K IN FY21"),
           caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"))

   si_save("Images/13_gbv_reached.png")
   
   # Map of FY21 Numbers?
   ggplot() +
     geom_sf(data = gbv_geo, fill = "white", color = trolley_grey, size = .25) +
     geom_sf(data = gbv_geo %>% filter(!is.na(cumulative)), aes(fill = cumulative),
             size = .4, 
             color = "white") +
     scale_fill_si(palette = "burnt_siennas", discrete = F, labels = comma)+
     labs(title = glue("USAID PROVIDES GENDER-BASED VIOLENCE SERVICES IN {cntry_count} COUNTRIES"),
          caption = glue("Source: {msd_source}
                     SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"),
          fill = "Total individuals receiving GBV clinical care") +
     si_style_map() +
     theme(
       legend.direction = "horizontal",
       panel.background = element_rect(fill = "#bfddff40", color = "NA"),
       plot.title = element_text(hjust =  0),
       plot.caption = element_text(hjust = 1),
       legend.key.width = unit(10, "lines"),
       legend.key.height = unit(0.8, "lines"),
       legend.position = c(0.5, 0.05),
       legend.title = element_text(size = 10, color = "grey45"),
       legend.text = element_text(color = "grey45"),
       legend.key.size = unit(1.2, "lines"),
     ) +
     guides(fill = guide_colorbar(barheight = unit(4, units = "mm"),  
                               barwidth = unit(100, units = "mm"),
                               direction = "horizontal",
                               ticks.colour = "white",
                               title.position = "top",
                               title.hjust = 0.5)) 

      si_save("Images/13_map_gbv_coverage.png")
      
  
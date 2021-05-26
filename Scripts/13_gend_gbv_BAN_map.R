# PROJECT:  agiprop
# AUTHOR:   T. Essam | USAID
# PURPOSE:  GBV support globally
# LICENSE:  MIT
# DATE:     2021-05-14
# UPDATED:  2021-05-24

# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(glitr)
  library(gisr)
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
  library(rnaturalearth)
  
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
  
# MUNGE -------------------------------------------------------------------  
  
  #current FY
  curr_fy <- identifypd(df, "year")
  
  # For captions
  msd_source <- 
    df %>% 
    identifypd() %>% 
    msd_period(period = .)
  
  # Munge countries for map
  df_gbv <- 
    df %>% 
    filter(fundingagency == "USAID", fiscal_year == 2021) %>% 
    group_by(countryname, operatingunit, fiscal_year) %>% 
    summarise(across(matches("tar|cumul"), sum, na.rm = T))
  
  # How many mechanisms support GBV in FY21?
  df_gbv_mech_count <- 
    df %>% 
    filter(fundingagency == "USAID", fiscal_year == 2021) %>% 
    group_by(countryname, operatingunit, fiscal_year, mech_code) %>% 
    summarise(across(matches("tar|cumul"), sum, na.rm = T)) %>% 
    ungroup() %>% 
    distinct(mech_code, countryname) %>% 
    count(countryname)

  # Grab distinct list of African countries to filter the spatial data 
  ou_gbv_afr <- 
    df_gbv %>% 
    ungroup() %>% 
    filter(!countryname %in% c("Papua New Guinea", "Dominican Republic", "Haiti")) 
  
  # Global numbers for the achievement plot
   df_gbv_glb <-
    df %>% 
    filter(fundingagency == "USAID") %>% 
    group_by(fiscal_year) %>% 
    summarise(across(matches("tar|cumul"), sum, na.rm = T)) %>% 
    mutate(ach = cumulative/targets,
          fill_col = if_else(fiscal_year == 2021, "#fd9873", burnt_sienna)) 
   
   #What is achievement for FY21
   fy21_ach <- 
     df_gbv_glb %>% 
     filter(fiscal_year == 2021) %>% 
     pull(cumulative)/1e3
  
   # Map of just africa, add in PNG like haiti in DREAMS slide
   gbv_geo_afr <- 
     rnaturalearth::ne_countries(continent = "africa", returnclass = "sf") %>% 
     select(admin) %>% 
     clean_countries(colname = "admin") %>% 
     left_join(., ou_gbv_afr, by = c("admin" = "countryname")) 

   cntry_count <- 
     df_gbv %>% 
     distinct(countryname) %>% 
     pull() %>% 
     length()

  # Set shapefile for just PNG, include Indoensia for a cleaner map 
   gbv_geo_png <- 
     ne_countries(type = "sovereignty", 
                               scale = 10, 
                               returnclass = "sf") %>% 
     filter(admin %in% c("Papua New Guinea", "Indonesia")) %>% 
     select(admin) %>% 
     left_join(., df_gbv %>% filter(str_detect(countryname, "Papua")), 
               by = c("admin" = "countryname")) %>% 
     mutate(fill_col = if_else(admin != "Indonesia", "#ffb790", trolley_grey_light))
   
  # Repeat for Haiti and DR, Caribbean small map; Fill colors may have to be adapted
  # depending on what scale you are using
   gbv_geo_dr <- 
     ne_countries(type = "sovereignty", 
                               scale = 10, 
                               returnclass = "sf") %>% 
     filter(admin %in% c("Dominican Republic", "Haiti")) %>% 
     select(admin) %>% 
     left_join(., df_gbv %>% filter(str_detect(countryname, "Dominican|Haiti")), 
               by = c("admin" = "countryname")) %>% 
     mutate(fill_col = "#FFC9A2")
   
   
   
# PLOT --------------------------------------------------------------------
  
  # High level, what does it look like?
    df_gbv_glb %>% 
    ggplot(aes(x = fiscal_year)) +
    geom_col(aes(y = targets), fill = grey10k) +
    geom_col(aes(y = cumulative, fill = fill_col)) +
      geom_hline(yintercept = seq(1e5, 3e5, by = 1e5), color = "white", size = 0.5) +
      geom_text(aes(y = cumulative, label = percent(ach, 1)), 
                size = 12/.pt, 
                font = "SourceSansPro", 
                color = color_plot_text, 
                vjust = -0.25)+
    si_style_xline() +
    scale_fill_identity() +
      scale_y_continuous(labels = unit_format(1, unit = "K", scale = 1e-3),
                         position = "right", expand = c(.005, .005)) +
      scale_x_continuous(expand = c(.005, .005), breaks = c(2019, 2020, 2021)) +
      labs(x = NULL, y = NULL, title = glue("USAID HAS PROVIDED POST-GENDER BASED VIOLENCE (GBV) CLINICAL CARE TO MORE THAN {comma(fy21_ach, 1)}K IN FY21"),
           caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"))

   si_save("Graphics/13_gbv_reached.svg")
   
   # Map of FY21 Numbers?
   ggplot() +
     geom_sf(data = gbv_geo_afr, fill = "white", color = trolley_grey_light, size = 0.25) +
     geom_sf(data = gbv_geo_afr %>% filter(!is.na(cumulative)), aes(fill = cumulative),
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
       legend.position = c(0.15, 0.96),
       legend.title = element_text(size = 10, color = "grey45"),
       legend.text = element_text(color = "grey45"),
       legend.key.size = unit(1.2, "lines"),
     ) +
     guides(fill = guide_colorbar(barheight = unit(3, units = "mm"),  
                               barwidth = unit(75, units = "mm"),
                               direction = "horizontal",
                               ticks.colour = "white",
                               title.position = "top",
                               title.hjust = 0.5)) 

      si_save("Graphics/13_map_gbv_afr_coverage.svg",
              height = 5,
              width = 4.6, sc)
    
    #PNG
      ggplot() +
        geom_sf(data = gbv_geo_png, aes(fill = fill_col),
                size = .4, 
                color = "white") +
        scale_fill_identity() +
        coord_sf(xlim = c(133, 157), ylim = c(-12, 0)) +
        si_style_map() 
       
      si_save("Graphics/13_map_gbv_png_coverage.svg", 
              height = 1,
              width = 2.3) 
      
    #HTI and DR
      ggplot() +
        geom_sf(data = gbv_geo_dr, aes(fill = fill_col),
                size = .4, 
                color = "white") +
        scale_fill_identity() +
        si_style_map() 
      
      si_save("Graphics/13_map_gbv_dr_coverage.svg", 
              height = 1,
              width = 4) 

        
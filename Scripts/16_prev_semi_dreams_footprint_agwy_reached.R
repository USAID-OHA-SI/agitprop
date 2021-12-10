# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  DREAMS MAP and BAN of AGWY_PREV
# LICENSE:  MIT
# DATE:     2021-05-11
# UPDATED:  2021-12-09
# NOTE:     adapted from USAID-OHA-SI/groundhogday (linked below)
# URL:      https://github.com/USAID-OHA-SI/groundhog_day/blob/master/Scripts/FY21Q1_Qreview_remakes.R#L165-L200

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
  library(patchwork)
  
  source("Scripts/99_utilities.R")

  merdata <- glamr::si_path("path_msd")
  rasdata <- glamr::si_path("path_raster")
  shpdata <- glamr::si_path("path_vector")
  datim   <- glamr::si_path("path_datim")  
  
  load_secrets()

# GLOBAL VARIABLES --------------------------------------------------------

  authors <- c("Aaron Chafetz", "Tim Essam")

# IMPORT ------------------------------------------------------------------

#MSD
  msd_path <- si_path() %>% 
    return_latest("PSNU_IM_DREAMS_FY19-22") 
  
 df_ou <- read_msd(msd_path) %>% resolve_knownissues()

  #source info
  msd_source <- source_info(msd_path)
  curr_fy <- source_info(return = "fiscal_year")
  curr_pd <- source_info(return = "period")
    
# VIZ ---------------------------------------------------------------------

  drms_ban <- 
    df_ou %>% 
    filter(indicator == "AGYW_PREV", 
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"), 
           fiscal_year == curr_fy) %>% 
    group_by(fiscal_year, standardizeddisaggregate) %>% 
    summarise(across(matches("cum"), sum, na.rm = T)) %>% 
    spread(standardizeddisaggregate, cumulative) %>% 
    mutate(pct_complete = `Total Numerator`/`Total Denominator`) %>% 
    arrange(desc(pct_complete))
  
  
  drms_ban_plot <- 
    drms_ban %>% 
    mutate(num_lab = "Total Adolsecent Girls and Young Women (AGWY) Completing Partial Package",
           tot_num = `Total Numerator`,
           tot_denom = `Total Denominator`,
           denom_lab = "Total AGWY Reached by DREAMS") %>% 
  ggplot() +
  geom_text(aes(x = 0.5, y = 1, label = comma(tot_num)), size = 48/.pt) +
    geom_text(aes(x = 0.5, y = .75, label = str_wrap(num_lab, width = 40))) +
    geom_text(aes(x = 1.5, y = 1, label = comma(tot_denom)), size = 48/.pt) +
    geom_text(aes(x = 1.5, y = .75, label = str_wrap(denom_lab, width = 40))) +
    si_style_void() +
    scale_x_continuous(limits = c(0.25, 2)) +
    scale_y_continuous(limits = c(0.5, 2)) 
  

  drms_ban_bar <-  
    drms_ban %>% 
      ggplot() +
      geom_col(aes(x = 1, y = 1), fill = trolley_grey_light) +
      geom_col(aes(x = 1, y = pct_complete), fill = scooter) +
      geom_text(aes(x = 1, y = pct_complete, label = percent(pct_complete)), hjust = -0.25, size = 12/.pt) +
      coord_flip(clip = "off", expand = F) +
      si_style_void()
      
  p <- drms_ban_plot / drms_ban_bar
  ggsave("Graphics/06_dreams_BAN.svg", plot = p, height = 7, width = 10, scale = 1.25, device = "svg")
  
  
  # Completion across OUS
  drms_ban_ou <- 
    df_ou %>% 
    filter(indicator == "AGYW_PREV", 
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"), 
           fiscal_year == 2021) %>% 
    group_by(fiscal_year, standardizeddisaggregate, operatingunit) %>% 
    summarise(across(matches("cum"), sum, na.rm = T)) %>% 
    spread(standardizeddisaggregate, cumulative) %>% 
    mutate(pct_complete = `Total Numerator`/`Total Denominator`,
           ou_order = fct_reorder(operatingunit, `Total Denominator`)) %>% 
    arrange(desc(pct_complete))
  
  drms_ban_ou %>% 
    ggplot(aes(y = ou_order)) +
    geom_col(aes(x = `Total Denominator`), fill = trolley_grey_light) +
    geom_col(aes(x = `Total Numerator`), fill = scooter, alpha = 0.75) +
    geom_vline(xintercept = seq(1e5, 8e5, by = 1e5), color = "white") +
    geom_text(aes(x = `Total Numerator`, label = percent(pct_complete, 1)), size = 9/.pt, family = "Source Sans Pro", color = color_plot_text) +
    si_style_nolines() +
    scale_x_continuous(labels = unit_format(1, unit = "K", scale = 1e-3),
                       position = "top", 
                      breaks = seq(1e5, 8e5, by = 1e5)) +
    coord_cartesian(expand = F) +
    labs(x = NULL, y = NULL, title = "")
    
  ggsave("Graphics/06_drms_ou_graph.svg", height = 4, width = 4, scale = 1.5)
  

#SLIDE: USAID supports DREAMS implementation across the majority of DREAMS SNUs in FY20
  
  ou_dreams_afr <- 
    df_ou %>% 
    distinct(operatingunit) %>% 
    mutate(country = if_else(operatingunit == "Cote d'Ivoire", "Ivory Coast", operatingunit)) %>%
    filter(country != "Haiti") %>% pull()

  
  #ous <- get_outable(datim_user(), datim_pwd())
  ous <- 
    get_outable(datim_user(), datim_pwd()) %>% 
    mutate(countryname_iso = if_else(countryname_iso == "SSD", "SDS", countryname_iso))
  
  map <- rnaturalearth::ne_countries(continent = "africa", returnclass = "sf") %>% 
    left_join(., ous, by = c("sov_a3" = "countryname_iso")) %>% 
    mutate(pepfar_fill = case_when(
      sovereignt %in% c("United Republic of Tanzania", "Swaziland") ~ denim,
      sovereignt %in% ou_dreams_afr ~ denim,
      TRUE ~ grey20k)
    ) 
  
  # Set up terrain  
  afr <- map %>% st_drop_geometry() %>% distinct(sovereignt) %>% pull()
  terr <- get_terrain(afr, terr = rasdata, mask = T)
  
  afr_map <- ggplot() +
    # geom_tile(data = filter(terr, value < 210), aes(x = x, y = y, alpha = value)) + 
    # scale_alpha(name = "", range = c(0.6, 0), guide = F) +
    geom_sf(data = map, aes(fill = pepfar_fill), color = "white", size = 0.1, alpha = 0.8) +
    scale_fill_identity() +
    si_style_map() +
    labs(x = NULL, y = NULL,
         title = "THE DREAMS (Determined, Resilient, Empowered, AIDS-free, Mentored and Safe) PARNTERSHIP SERVES 15 COUNTRIES",
         caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) 
  
  ggsave("Graphics/05_AFR_dreams_map.svg", plot = afr_map, height = 5,
         width = 4.6)
  
  
  # Add in Haiti
  hti <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large") %>% 
    filter(sovereignt %in% c("Haiti", "Dominican Republic")) %>% 
    mutate(ou_fill = if_else(sovereignt == "Haiti", denim, grey20k)) 
  
  hti_terr <- get_terrain(hti %>% st_drop_geometry() %>% distinct(sovereignt) %>% pull(),
                          terr = rasdata, mask = T)
  
  hti_map <- ggplot(hti) +
    # geom_tile(data = filter(hti_terr, value < 210), aes(x = x, y = y, alpha = value)) + 
    # scale_alpha(name = "", range = c(0.6, 0), guide = F) +
    geom_sf(aes(fill = ou_fill), color = "white", size = 0.1, alpha = 0.8) +
    scale_fill_identity() +
    si_style_map() 
  
  ggsave("Graphics/06_HTI_dreams_map.svg", plot = hti_map,
         height = 1,
         width = 2.3)
  
  
  
  
# PROJECT:  agitprop
# AUTHOR:   B.Kagniniwa | USAID
# PURPOSE:  GEND_GBC Distribution by Country
# LICENSE:  MIT
# DATE:     2021-12-07

# DEPENDENCIES ----

  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(cowplot)
  library(ggtext)
  library(glue)
  library(lubridate)
  library(gt)
  library(sf)
  library(raster)
  library(gisr)
  library(rmapshaper)
  library(rnaturalearth)


# GLOBAL VARS ----

  dir_merdata <- si_path()
  dir_graphics <- "./Graphics"
  
  file_ou_im_curr <- dir_merdata %>% 
    return_latest(pattern = "OU_IM_FY19-22_\\d{8}.*")
  
  file_psnu_im_curr <- dir_merdata %>% 
    return_latest(pattern = "PSNU_IM_FY19-22_\\d{8}.*")
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Baboyma Kagniniwa")

# IMPORT ----

  #MER data
  df_msd <- file_ou_im_curr %>% read_msd()
  
  curr_fy <- df_msd %>% identifypd(pd_type = "year")
  curr_pd <- df_msd %>% identifypd(pd_type = "full")
  
  msd_source <- "FY21Q4i"
  
  # Spatial Data
  spdf <- ne_countries(type = "sovereignty", 
                       scale = 110, 
                       returnclass = "sf") %>% 
    dplyr::select(sovereignt, admin, name, adm0_a3, continent, subregion) %>% 
    filter(admin != "Antarctica") %>% # Remove Antarctica
    clean_countries(colname = "admin")
  
  spdf <- ms_simplify(spdf, keep = 0.75) %>% 
    st_transform(., "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  
  # AF Mainland + Madagascar
  spdf_af <- spdf %>% filter(continent == "Africa") 
  
  spdf_af %>% gview
  
  # Haiti + DR
  spdf_dr <- spdf %>% filter(admin %in% c("Haiti", "Dominican Republic")) 
  
  spdf_dr %>% gview
  
  # PNG Only
  spdf_png <- spdf %>% filter(admin %in% c("Papua New Guinea")) 
  
  spdf_png %>% gview
  
  
  # Terrain Raster
  tr_ras <- get_raster()
  
  # AF - Terrain Raster
  tr_ras_af <- spdf_af %>% 
    st_transform(4326) %>% 
    extent() %>% 
    extend(.2) %>% 
    crop(tr_ras, .) %>% 
    projectRaster(., crs = crs(spdf))
  
  # HI + DR - Terrain Raster
  tr_ras_dr <- spdf_dr %>% 
    st_transform(4326) %>% 
    extent() %>% 
    extend(.2) %>% 
    crop(tr_ras, .) %>% 
    projectRaster(., crs = crs(spdf))
  
  # PNG - Terrain Raster
  tr_ras_png <- spdf_png %>% 
    st_transform(4326) %>% 
    extent() %>% 
    extend(.2) %>% 
    crop(tr_ras, .) %>% 
    projectRaster(., crs = crs(spdf))

  tr_ras_png %>% plot()
  
  
# MUNGING ----
  
  # GDV
  df_gdv <- df_msd %>% 
    filter(fiscal_year == curr_fy,
           #fundingagency != "DEDUP",
           fundingagency == "USAID",
           indicator == "GEND_GBV",
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, fundingagency, operatingunit, countryname, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    dplyr::select(-c(period_type, fundingagency, operatingunit)) 
  
  df_gdv <- df_gdv %>% 
    group_by(countryname, indicator) %>% 
    summarise(value = sum(value, na.rm = T), .groups = "drop") %>% 
    mutate(period = "FY21") %>% 
    bind_rows(df_gdv, .) 
  
  cntry_count <- df_gdv %>% 
    filter(period == "FY21", !is.na(value)) %>% 
    distinct(countryname) %>% 
    nrow()
  
  # SPDF
  spdf_gdv <- spdf %>% 
    left_join(df_gdv, by = c("admin" = "countryname"))
  
  spdf_gdv_cum = spdf_gdv %>% 
    filter(period == "FY21", !is.na(value)) %>%
    mutate(
      lbl_color = case_when(
        value > 10000 ~ "white",
        TRUE ~ usaid_black),
      admin = case_when(
        admin == "Democratic Republic of the Congo" ~ "DRC",
        admin == "Dominican Republic" ~ "DR",
        admin == "Papua New Guinea" ~ "PNG",
        admin == "Cote d'Ivoire" ~ "CIV",
        TRUE ~ admin))
  
  spdf_gdv_cum %>% dview()
  
  spdf_gdv_cum_main = spdf_gdv_cum %>% 
    filter(admin %ni% c("Haiti", "DR", "PNG"))
  
  spdf_gdv_cum_dr = spdf_gdv_cum %>% 
    filter(admin %in% c("Haiti", "DR"))
  
  spdf_gdv_cum_png = spdf_gdv_cum %>% 
    filter(admin %in% c("PNG"))
  

  
# VIZ ----

  basemap <- terrain_map(countries = spdf_af,
                         adm0 = spdf_af,
                         terr = tr_ras_af,
                         mask = TRUE)
  
  basemap_dr <- terrain_map(countries = spdf_dr,
                            adm0 = spdf_dr,
                            terr = tr_ras_dr,
                            mask = TRUE)
  
  basemap_png <- terrain_map(countries = spdf_png,
                             adm0 = spdf_png,
                             terr = tr_ras_png,
                             mask = TRUE)
  
  
  # Overview ----
  ggplot() +
    geom_sf(data = spdf_gdv_cum, 
            fill = "white", 
            color = trolley_grey, 
            size = .25) +
    geom_sf(data = spdf_gdv_cum, 
            aes(fill = value),
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
  
  
  
  # Main Map ----
  #map_main <- ggplot() +
  map_main <- basemap +
    geom_sf(data = spdf_gdv_cum_main, 
            aes(fill = value),
            size = .4, color = "white", alpha = .7) +
    scale_fill_si(palette = "burnt_siennas", discrete = F, labels = comma)+
    si_style_map() +
    labs(x = "", y = "", fill = "Total individuals receiving GBV clinical care") +
    theme(
      legend.direction = "horizontal",
      #panel.background = element_rect(fill = "#bfddff40", color = "NA"),
      plot.title = element_text(hjust =  0),
      plot.caption = element_text(hjust = 1),
      legend.key.width = unit(10, "lines"),
      legend.key.height = unit(0.8, "lines"),
      legend.position = c(0.00, 0.00),
      #legend.position = "bottom",
      legend.title = element_text(size = 10, color = "grey45"),
      legend.text = element_text(color = "grey45"),
      legend.key.size = unit(1.2, "lines"),
    ) +
    guides(fill = guide_colorbar(barheight = unit(4, units = "mm"),  
                                 barwidth = unit(70, units = "mm"),
                                 direction = "horizontal",
                                 ticks.colour = "white",
                                 title.position = "top",
                                 title.hjust = 0.5)) 
  
  # Main map 2 ----
  map_main2 <- ggplot() +
    geom_sf(data = spdf_af, fill = NA, size = 1.1, color = grey20k) +
    geom_sf(data = spdf_af, fill = NA, size = .2, color = usaid_black) +
    geom_sf(data = spdf_gdv_cum_main, 
            aes(fill = value),
            size = .4, color = "white", alpha = .7) +
    geom_sf(data = spdf_af, fill = NA, size = .4, color = "white") +
    geom_sf_text(data = spdf_gdv_cum_main, 
                 aes(label = admin, color = lbl_color), size = 3) +
    scale_fill_si(palette = "burnt_siennas", discrete = F, labels = comma)+
    scale_color_identity() +
    si_style_map() +
    labs(x = "", y = "", fill = "Total individuals receiving GBV clinical care") +
    theme(
      legend.direction = "horizontal",
      plot.title = element_text(hjust =  0),
      plot.caption = element_text(hjust = 1),
      legend.key.width = unit(10, "lines"),
      legend.key.height = unit(0.8, "lines"),
      legend.position = c(0.0, 0),
      legend.title = element_text(size = 10, color = "grey45"),
      legend.text = element_text(color = "grey45"),
      legend.key.size = unit(1.2, "lines"),
    ) +
    guides(fill = guide_colorbar(barheight = unit(4, units = "mm"),  
                                 barwidth = unit(70, units = "mm"),
                                 direction = "horizontal",
                                 ticks.colour = "white",
                                 title.position = "top",
                                 title.hjust = 0.5)) 
  
  
  # Haiti and DR
  #map_dr <- ggplot() +
  map_dr <- basemap_dr +
    geom_sf(data = spdf_gdv_cum_dr, 
            aes(fill = value),
            size = .4, color = "white",
            alpha = .7, show.legend = F) +
    scale_fill_si(palette = "burnt_siennas", discrete = F, labels = comma) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(panel.border = element_rect(colour = grey10k, fill = NA, size=.5),
          plot.margin = margin(c(t = 0, r = 0, b = 0, l = 0), unit = "pt"))
  
  
  # Haiti and DR
  #map_png <- ggplot() +
  map_png <- basemap_png +
    geom_sf(data = spdf_gdv_cum_png, 
            aes(fill = value),
            size = .4, color = "white",
            alpha = .7, show.legend = F) +
    scale_fill_si(palette = "burnt_siennas", discrete = F, 
                  labels = comma) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(panel.border = element_rect(colour = grey10k, fill = NA, size=.5),
          panel.grid = element_blank(),
          plot.margin = margin(c(t = 0, r = 0, b = 0, l = 0), unit = "pt"),
          plot.background = element_rect(fill = NA))
  
  
  # Combine Plots
  map_all <- ggdraw() +
    draw_plot(map_main) +
    draw_plot(map_png, 
              x = 0.23, y = 0.15,
              width = 0.20, height = 0.15) +
    draw_plot(map_dr, 
              x = 0.23, y = 0.30,
              width = .20, height = .12)
  
  # Combine Plots #2
  map_all2 <- ggdraw() +
    draw_plot(map_main) +
    draw_plot(map_png, 
              x = 0.23, y = 0.15,
              width = 0.20, height = 0.15) +
    draw_plot(map_dr, 
              x = 0.23, y = 0.30,
              width = .20, height = .12)
  
  # Export Map
  ggsave(file.path(dir_graphics, "USAID - GEND_GBV_Distribution.png"),
         plot = map_all2,
         path = NULL,
         #scale = 1,
         #width = 5.625,
         #height = 10,
         dpi = 320)
  
  
  
  
  
  
  
  
  
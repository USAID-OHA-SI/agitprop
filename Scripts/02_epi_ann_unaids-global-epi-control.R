# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | T.Essam | K. Srikanth | USAID
# PURPOSE:  Global epidemic control curves 
# LICENSE:  MIT
# DATE:     2021-07-19
# UPDATED:  2021-12-03
# NOTE:     Created based on request from FO "2 visuals for AHOP deck" -- 2021-07-16


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
  library(googlesheets4)
  library(mindthegap)


# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets()
  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  
  plot_title <- "STEADY DECLINE IN THE GLOBAL NUMBER OF <span style= 'color:#2057a7;'> 
  NEW HIV INFECTIONS</span> AND <span style = 'color:#c43d4d;'> AIDS-RELATED DEATHS </span> SINCE EARLY 2000s"
  
  source <- source_note
  
  # Creates a new dataframe so you can call geom_richtext() in the viz flow. This allows for markedup annotations.
  note_df <- tibble(
    label = c("**HIV epidemic control is the point at which<br> the number of </span><span style= 'color:#2057a7;'>new HIV infections</span>
    falls<br> below the number of <span style= 'color:#c43d4d;'>AIDS-related<br>deaths</span>**"),
    year = c(2010), 
    value = c(0.4e6))
  
  # To preview annotation blob
  # ggplot(note_df, aes(year, value, label = label, hjust = 0)) +
  # geom_richtext(fill = NA, label.color = NA, # remove background and outline
  #               label.padding = grid::unit(rep(0, 4), "pt"))
  
  
# IMPORT ------------------------------------------------------------------
  # Grab data from google drive
  epi_glbl <- range_speedread("1Vt54N2W51WEWY0XlOr6vP4pa3F-7AwpXxyRx9Y9Hcq4", sheet = "HIV Estimates - Integer") %>% 
    filter(country == "Global", str_detect(indicator, "(AIDS Related|New HIV Infections)"))

  # Check that filters have not changed
  epi_glbl %>% count(country) %>% prinf()
  
  epi_viz <- 
    epi_glbl %>% 
    filter(age == "15+", stat == "est") %>% 
    spread(indicator, value) %>% 
    janitor::clean_names() %>% 
    mutate(epi_gap = new_hiv_infections - aids_related_deaths)
  
## FT VIZ ---------------------------------------------------------
  # continent
  epi_gap_end <- epi_viz %>% filter(year == max(year)) %>% pull(epi_gap)
  
  viz <- 
    epi_viz %>% 
    ggplot(aes(x = year)) +
    geom_area(aes(y = new_hiv_infections), fill = "#C6D5E9", alpha = 0.95) +
    geom_area(aes(y = -aids_related_deaths), fill = "#F1CED2",  alpha = 0.95) +
    geom_line(aes(y = new_hiv_infections), color = denim, size = 1) +
    geom_line(aes(y = -aids_related_deaths), color = old_rose, size = 1) +
    geom_line(aes(y = epi_gap), color = "white", size = 0.25) +
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = new_hiv_infections, fill = denim), shape = 21, color = "white", size = 3)+
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = -aids_related_deaths, fill = old_rose), shape = 21, color = "white", size = 3) + 
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = new_hiv_infections, color = denim, 
                  label = paste0(round(new_hiv_infections/1000000, digits = 3), "M")),
              hjust = -0.3, size = 12/.pt,
              family = "Source Sans Pro SemiBold") +
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = -aids_related_deaths, color = old_rose, 
                  label = paste0(abs(aids_related_deaths/1000) %>% comma(1.0), "K")),
              hjust = -0.3, size = 12/.pt,
              family = "Source Sans Pro SemiBold") +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(label = ~ label_number_si()(abs(.))) +
    scale_x_continuous(breaks = seq(1990, 2024, 5)) +
    geom_hline(yintercept = 0, color = grey80k) +
    si_style_ygrid(text_scale = 1.15) +
    labs(x = NULL, y = NULL,
         title = plot_title,
         caption =  glue("\n{source}
                     SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    coord_cartesian(expand = T, clip = "off") +
    theme(plot.title = element_markdown())
  
  viz_ann <- 
    viz +
    geom_richtext(data = note_df, aes(x = year, y = value, label = label),
                  fill = NA, label.color = NA, # remove background and outline
                  label.padding = grid::unit(rep(0, 4), "pt"), hjust = 0, 
                  size = 12/.pt, color = grey90k) +
    annotate("curve",
             x = 2017, y = epi_gap_end + 0.5e6, xend = 2020, yend = epi_gap_end,
             arrow = arrow(length = unit(0.03, "inches"),
                           type = "closed"),
             curvature = -.4,
             color = suva_grey) +
    annotate(geom = "text", x = 2014, y = epi_gap_end + 0.5e6, label = c("Epidemic control gap"),
             family = "Source Sans Pro", color = suva_grey, size = 14/.pt) +
    annotate(geom = "text", x = 1995, y = 2.7e6, label = c("New HIV Infections"), hjust = 0,
             family = "Source Sans Pro SemiBold", color = denim, size = 14/.pt) +
    annotate(geom = "text", x = 1997, y = -1.5e6, label = c("AIDS-related Deaths"), hjust = 0,
             family = "Source Sans Pro SemiBold", color = old_rose, size = 14/.pt) +
    labs(x = NULL, y = NULL,
         title = plot_title,
         caption =  glue("\n{source}
                     SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    coord_cartesian(expand = T, clip = "off") +
    theme(plot.title = element_markdown())
  
  ggsave("Images/02_epi_ann_global_epi_control_v1.png", scale = 1.2, width = 10, height = 7)
  ggsave("Graphics/02_epi_ann_global_eip_control_v1.svg", scale = 1.2, width = 10, height = 7)
  

# EXCEL VIZ ---------------------------------------------------------------

  viz_excel <- 
    epi_viz %>% 
    ggplot(aes(x = year)) +
    geom_area(aes(y = new_hiv_infections), fill = "#C6D5E9") +
    geom_area(aes(y = aids_related_deaths), fill = "#F1CED2") +
    geom_line(aes(y = new_hiv_infections), color = "white", size = 3) +
    geom_line(aes(y = new_hiv_infections), color = denim, size = 1) +
    geom_line(aes(y = aids_related_deaths), color = "white", size = 3) +
    geom_line(aes(y = aids_related_deaths), color = old_rose, size = 1) +
    
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = new_hiv_infections, fill = denim), shape = 21, color = "white", size = 3)+
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = aids_related_deaths, fill = old_rose), shape = 21, color = "white", size = 3) + 
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = new_hiv_infections, color = denim, 
                  label = paste0(round(new_hiv_infections/1000000, digits = 3), "M")),
              hjust = -0.3, size = 12/.pt,
              family = "Source Sans Pro SemiBold") +
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = aids_related_deaths, color = old_rose, 
                  label = paste0(abs(aids_related_deaths/1000) %>% comma(1.0), "K")),
              hjust = -0.3, size = 12/.pt,
              family = "Source Sans Pro SemiBold") +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(label = ~ label_number_si()(abs(.))) +
    scale_x_continuous(breaks = seq(1990, 2024, 5)) +
    geom_hline(yintercept = 0, color = grey80k) +
    si_style_ygrid(text_scale = 1.15) +
    labs(x = NULL, y = NULL,
         title = plot_title,
         caption =  glue("\n{source}
                     SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    coord_cartesian(expand = T, clip = "off") +
    theme(plot.title = element_markdown())
  
  viz_excel_ann <- 
    viz_excel +
    geom_richtext(data = note_df, aes(x = 2010, y = value - 0.5e5, label = label),
                  fill = NA, label.color = NA, # remove background and outline
                  label.padding = grid::unit(rep(0, 4), "pt"), hjust = 0, 
                  size = 12/.pt, color = grey90k) +
    annotate(geom = "text", x = 1994, y = 2.4e6, label = c("New HIV Infections"), hjust = 0,
             family = "Source Sans Pro SemiBold", color = denim, size = 14/.pt) +
    annotate(geom = "text", x = 2000.5, y = 1.2e6, label = c("AIDS-related Deaths"), hjust = 0,
             family = "Source Sans Pro SemiBold", color = old_rose, size = 14/.pt) +
    labs(x = NULL, y = NULL,
         title = plot_title,
         caption =  glue("\n{source}
                     SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    coord_cartesian(expand = T, clip = "off") +
    theme(plot.title = element_markdown())
  
  ggsave("Images/02_epi_ann_global_eip_control_v2.png", scale = 1.2, width = 10, height = 7)
  ggsave("Graphics/02_epi_ann_global_eip_control_v2.svg", scale = 1.2, width = 10, height = 7)
 
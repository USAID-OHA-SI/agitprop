# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | T.Essam | K. Srikanth | USAID
# PURPOSE:  Global epidemic control curves 
# LICENSE:  MIT
# REF ID:   c4d74f41 
# DATE:     2021-07-19
# UPDATED:  2023-08-25 (new 2023 UNAIDS data)
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
  NEW HIV INFECTIONS</span> AND <span style = 'color:#c43d4d;'> TOTAL PLHIV DEATHS </span> SINCE EARLY 2000s"
  
  source <- source_note
  
  # Creates a new dataframe so you can call geom_richtext() in the viz flow. This allows for markedup annotations.
  note_df <- tibble(
    label = c("**HIV epidemic control is the point at which<br> the number of </span><span style= 'color:#2057a7;'>new HIV infections</span>
    falls<br> below the number of <span style= 'color:#c43d4d;'>total deaths to PLHIV</span>**"),
    year = c(2010), 
    value = c(0.4e6))
  
  ref_id <- "c4d74f41"
  
  # To preview annotation blob
  # ggplot(note_df, aes(year, value, label = label, hjust = 0)) +
  # geom_richtext(fill = NA, label.color = NA, # remove background and outline
  #               label.padding = grid::unit(rep(0, 4), "pt"))
  
  
# IMPORT ------------------------------------------------------------------
  # Grab data from google drive
  
  
  
  epi_glbl <- pull_unaids("HIV Estimates", FALSE) %>% 
    filter(country == "Global", str_detect(indicator, "(Total deaths|New HIV Infections)"))
  
  #pull Total PLHIV death data ()
  g_id <- "1CSVOauu2gyq9Am0eCl7TgpAeB1Xd3dCtE_Oc_yk3cI4"
  
  df_deaths <-  pull_unaids(FALSE, "epicontrol", FALSE) %>% 
    filter(indicator == "Number Total Deaths HIV Pop")

  # Check that filters have not changed
  epi_glbl %>% count(country) %>% prinf()
  
  # #grab total deaths
  # total_deaths <- df_deaths %>% 
  #   #select(-c(iso2, geo_level)) %>% 
  #   filter(age == "all",
  #          sex == "all",
  #          country == "Global") %>% 
  #   select(c(country, year, indicator, estimate)) %>% 
  #   spread(indicator, estimate) %>% 
  #   janitor::clean_names() 
  
  epi_viz <- 
    epi_glbl %>% 
    filter(age == "All",
           sex == "All") %>% 
    mutate(region = ifelse(is.na(region), "Global", region)) %>% 
    select(-c(lower_bound, upper_bound, estimate_flag)) %>% 
    spread(indicator, estimate) %>%
    janitor::clean_names() %>% 
    #left_join(total_deaths, by = c("country", "year")) %>% 
    mutate(epi_gap = number_new_hiv_infections - total_deaths_to_hiv_population)
  

    
    
    
  
## FT VIZ ---------------------------------------------------------
  # continent
  epi_gap_end <- epi_viz %>% filter(year == max(year)) %>% pull(epi_gap)
  
  viz <- 
    epi_viz %>% 
    ggplot(aes(x = year)) +
    geom_area(aes(y = number_new_hiv_infections), fill = "#C6D5E9", alpha = 0.95) +
    geom_area(aes(y = -total_deaths_to_hiv_population), fill = "#F1CED2",  alpha = 0.95) +
    geom_line(aes(y = number_new_hiv_infections), color = denim, size = 1) +
    geom_line(aes(y = -total_deaths_to_hiv_population), color = old_rose, size = 1) +
    geom_line(aes(y = epi_gap), color = "white", size = 0.25) +
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = number_new_hiv_infections, fill = denim), shape = 21, color = "white", size = 3)+
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = -total_deaths_to_hiv_population, fill = old_rose), shape = 21, color = "white", size = 3) + 
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = number_new_hiv_infections, color = denim, 
                  label = paste0(round(number_new_hiv_infections/1000000, digits = 3), "M")),
              hjust = -0.3, size = 12/.pt,
              family = "Source Sans Pro SemiBold") +
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = -total_deaths_to_hiv_population, color = old_rose, 
                  label = paste0(abs(total_deaths_to_hiv_population/1000) %>% comma(1.0), "K")),
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
                  label.padding = grid::unit(rep(0, 4), "pt"), hjust = 0, vjust = 0.7,
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
    annotate(geom = "text", x = 1997, y = -1.5e6, label = c("Total Deaths to PLHIV"), hjust = 0,
             vjust = -1, family = "Source Sans Pro SemiBold", color = old_rose, size = 14/.pt) +
    labs(x = NULL, y = NULL,
         title = plot_title,
         caption = glue("Source: {source_note} [{date_pulled}]
                        SI analytics | Ref id: {ref_id}")) +
    coord_cartesian(expand = T, clip = "off") +
    theme(plot.title = element_markdown())
  
  ggsave("Images/02_epi_ann_global_epi_control_v1.png", scale = 1.2, width = 10, height = 7)
  ggsave("Graphics/02_epi_ann_global_eip_control_update2022.svg", scale = 1.2, width = 10, height = 7)
  

# EXCEL VIZ ---------------------------------------------------------------

  viz_excel <- 
    epi_viz %>% 
    ggplot(aes(x = year)) +
    geom_area(aes(y = number_new_hiv_infections), fill = "#C6D5E9") +
    geom_area(aes(y = total_deaths_to_hiv_population), fill = "#F1CED2") +
    geom_line(aes(y = number_new_hiv_infections), color = "white", size = 3) +
    geom_line(aes(y = number_new_hiv_infections), color = denim, size = 1) +
    geom_line(aes(y = total_deaths_to_hiv_population), color = "white", size = 3) +
    geom_line(aes(y = total_deaths_to_hiv_population), color = old_rose, size = 1) +
    
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = number_new_hiv_infections, fill = denim), shape = 21, color = "white", size = 3)+
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = total_deaths_to_hiv_population, fill = old_rose), shape = 21, color = "white", size = 3) + 
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = number_new_hiv_infections, color = denim, 
                  label = paste0(round(number_new_hiv_infections/1000000, digits = 3), "M")),
              hjust = -0.3, size = 12/.pt,
              family = "Source Sans Pro SemiBold") +
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = total_deaths_to_hiv_population, color = old_rose, 
                  label = paste0(abs(total_deaths_to_hiv_population/1000) %>% comma(1.0), "K")),
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
    annotate(geom = "text", x = 2000.5, y = 1.2e6, label = c("Total Deaths of PLHIV"), hjust = 0,
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
 
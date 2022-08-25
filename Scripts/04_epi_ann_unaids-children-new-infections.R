# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | T.Essam | K. Srikanth | USAID
# PURPOSE:  Global decline in new child HIV Infections
# LICENSE:  MIT
# DATE:     2021-07-19
# UPDATED:  2022-08-25 (new 2022 UNAIDS data)
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
  library(geomtextpath)


# GLOBAL VARIABLES --------------------------------------------------------

  load_secrets()
  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  source <- source_note

# Creates a new dataframe so you can call geom_richtext() in the viz flow. This allows for markedup annotations.
  note_df <- tibble(
    label = c("**HIV epidemic control is the point at which<br> the number of </span><span style= 'color:#2057a7;'>new HIV infections</span>
      falls<br> below the number of <span style= 'color:#c43d4d;'>total deaths to PLHIV</span>**"),
    year = c(2010), 
    value = c(0.4e6))

# To preview annotation blob
# ggplot(note_df, aes(year, value, label = label, hjust = 0)) +
# geom_richtext(fill = NA, label.color = NA, # remove background and outline
#               label.padding = grid::unit(rep(0, 4), "pt"))


# IMPORT ------------------------------------------------------------------
# Grab data from google drive
  epi_glbl <- pull_unaids("HIV Estimates", FALSE) %>% 
    filter(country == "Global", str_detect(indicator, "(AIDS Related|New HIV Infections)"))

  # Check that filters have not changed
  epi_glbl %>% count(country) %>% prinf()

  epi_viz <- 
    epi_glbl %>% 
    filter(age == "0-14",
           indicator == "Number New HIV Infections") %>% 
    arrange(year) %>% 
    select(year:estimate) %>% 
    mutate(delta = (estimate - lag(estimate)) / lag(estimate),
           delta_10 = (estimate - lag(estimate, n = 10))/lag(estimate, n = 10))
  
  epi_viz_deaths <- 
    epi_glbl %>% 
    filter(age == "0-14", str_detect(indicator, "AIDS Related Deaths")) 
  
  epi_viz_decade <- epi_viz %>% filter(year %in% c(2011, 2021))

# VIZ ---------------------------------------------------------------------

  # Let's try a faint time-series graph overlaid with a slope graph showing change between 2010-2020
  BAN <- epi_viz %>% filter(year == max(year)) %>% pull(delta_10)
  end_point <- epi_viz %>% filter(year == max(year)) %>% pull(estimate)
  plot_title <- glue("NEW HIV INFECTIONS AMONG CHILDREN DECLINED BY ALMOST HALF ({percent({BAN}, 1)}) FROM 2011 TO 2021")

  epi_viz %>% 
    mutate(label = "New HIV Infections\n0 - 14 year olds") %>% 
  ggplot(aes(x = year, y = estimate)) +
  geom_area(data = . %>% filter(year < 2012), fill = "#C6D5E9", alpha = 0.85) +
  geom_area(data = . %>% filter(year >= 2011), fill = "#d8e3f0", alpha = 0.85) +
  geom_line(color = "white", size = 3) +
  geom_line(color = denim, size = 1) +
  geom_line(data = epi_viz_decade, linetype = "dashed", color = grey50k, size = 0.5) +
  geom_point(data = epi_viz_decade, color = "white", size = 5) +
  geom_point(data = . %>% filter(year %in% c(2011, 2021)), aes(y = estimate), shape = 1, size = 5, color = grey90k) +
  annotate(geom = "text", x = 1998, y = 5e5, label = c("New HIV Infections\n0-14 year olds"), hjust = 0, vjust = 1,
            family = "Source Sans Pro SemiBold", color = denim, size = 14/.pt) +
  annotate(geom = "text", x = 2015, y = 2.5e5, label = paste(percent(BAN, 1), " decline"),
           family = "Source Sans Pro SemiBold", color = grey90k, size = 16/.pt, hjust = -0.2) +
    annotate("curve",
             x = 2019.75, y = 2.4e5, xend = 2020, yend = end_point + 0.12e5,
             arrow = arrow(length = unit(0.03, "inches"),
                           type = "closed"),
             curvature = -.4,
             color = suva_grey) +  
  # geom_textpath(aes(label = label), vjust = 1.25, hjust = 1, color = denim, 
  #                family = "Source Sans Pro", fontface = 2, size = 14/.pt) +
  scale_y_continuous(label = ~ label_number_si()(.), position = "right") +
  scale_x_continuous(breaks = seq(1990, 2024, 5)) +
  si_style_ygrid(text_scale = 1.15) +
  labs(x = NULL, y = NULL,
       title = plot_title,
         caption =  glue("Source: {source}
                     SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"))   

  si_save("Images/04_epi_ann_clhiv_new_infections_decline_v1.png", scale = 1.2)  
  

# TINKERING WITH FILLED STEP ----------------------------------------------

  # To get the fill, you need to create a duplicate value that straddles the two time points
  epi_viz_fill <- bind_rows(old = epi_viz,
                            new = epi_viz %>% mutate(estimate = lag(estimate)),
                            .id = "source") %>% 
    arrange(year, source)
  
  ggplot(epi_viz, aes(x = year, y = -estimate)) +
    geom_ribbon(aes(x = year, ymin = -estimate, ymax = 0), data = epi_viz_fill, fill =  "#C6D5E9", alpha = 0.75) +
    geom_step(color = denim) +
    geom_text(data = . %>%  filter(year %in% seq(1990, 2020, 5)), aes(label = year), hjust = .5, vjust = 2,
              family = "Source Sans Pro SemiBold", color = grey90k) +
    geom_point() +
    geom_hline(yintercept = 0, size = 0.5, color = grey90k) +
    si_style_xgrid() +
    scale_y_continuous(label = ~ label_number_si()(abs(.))) +
    scale_x_continuous(breaks = seq(1990, 2024, 5)) +
    si_style_ygrid(text_scale = 1.15) +
    labs(x = NULL, y = NULL,
         title = plot_title,
         caption =  glue("Source: {source}
                     SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"))  +
    theme(axis.text.x = element_blank())
  
  # Fix up in AI
  si_save("Images/04_epi_ann_clhiv_new_infections_decline_v1.svg", scale = 1.2)
    
  
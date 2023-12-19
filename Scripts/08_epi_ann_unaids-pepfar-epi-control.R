# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | T.Essam | K. Srikanth USAID
# PURPOSE:  Epi control trend graphs
# REF ID:   d565f3f7 
# LICENSE:  MIT
# DATE:     2021-12-01
# UPDATED:  2023-08-25 update for 2023
# NOTE:     derived from agitprop/24b_HIV_epi_control_country.R and catch-22/gpm_country_historic_epi-control

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
  library(gisr)
  library(googlesheets4)
  library(readxl)
  library(stringi)
  # remotes::install_github("https://github.com/USAID-OHA-SI/mindthegap.git", ref = "unaids-data")
  library(mindthegap) #remotes::install_github("USAID-OHA-SI/mindthegap")


# GLOBAL VARIABLES --------------------------------------------------------

  load_secrets()

ref_id <- "d565f3f7"

  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  
  #source info & definition of epidemic control
  source <- source_note 
  date_pulled <- "August 2023"
  
  note <- str_wrap("HIV epidemic control is the point at which the number number of new HIV infections falls below the number of total deaths to PLHIV", width = 40)
  
  # epi_control <- str_wrap("PEPFAR defines national HIV epidemic control as the point at which the total number of new HIV infections falls below the total number of deaths from all causes among HIV-infected individuals, with both new infections and deaths among HIV-infected individual slowing and declining.", width = 100)
  epi_control <- "20 PEPFAR countries have reached epidemic control, where new infections falls below total deaths and total deaths are declining. "
  
  plot_title <- "STEADY DECLINE IN THE NUMBER OF <span style= 'color:#2057a7;'> NEW HIV INFECTIONS</span> AND <span style = 'color:#c43d4d;'> TOTAL DEATHS TO PLHIV </span> SINCE THE EARLY 2000s"
  
  #focal countries
  sel_cntry <- c("Uganda", "Kenya", "Namibia", "Eswatini")

# IMPORT ------------------------------------------------------------------

  df_epi <- pull_unaids("HIV Estimates", TRUE)


# MUNGE -------------------------------------------------------------------

  df_epi_pepfar <- df_epi %>% 
    filter(
      #stat == "est",
           age == "All",
           sex == "All",
           indicator %in% c("Number New HIV Infections", "Total deaths to HIV Population")) %>%
    # semi_join(pepfar_country_list, by = c("iso" = "countryname_iso")) %>%
    select(year, country, region, indicator, estimate) %>%
    arrange(country, indicator, year)  

  df_epi_pepfar <- df_epi_pepfar %>% 
        pivot_wider(names_from = "indicator", values_from = "estimate") %>%
    rename(infections = `Number New HIV Infections`,
           total_deaths = `Total deaths to HIV Population`) %>% 
   # left_join(total_deaths, by = c("year", "country")) %>% 
    group_by(country) %>% 
    mutate(declining_inf = infections - lag(infections, order_by = year) <= 0,
      declining_deaths = total_deaths - lag(total_deaths, order_by = year) <= 0) %>% 
    ungroup() %>% 
    mutate(infections_below_deaths = infections < total_deaths,
           ratio = infections / total_deaths,
           direction_streak = sequence(rle(declining_deaths)$lengths),
           epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE) 
  
  
  df_epi_pepfar <- df_epi_pepfar %>% 
    pivot_longer(c(infections, total_deaths), names_to = "indicator") %>% 
    arrange(country, indicator, year) %>% 
    mutate(value_mod = ifelse(indicator == "total_deaths", -value, value),
           fill_color = ifelse(indicator == "total_deaths", old_rose, denim))
  
  epi_cntry <- df_epi_pepfar %>% 
    filter(year == max(year),
           indicator == "infections",
           epi_control == TRUE) %>%
           # country %in% sel_cntry) %>% 
    arrange(desc(value)) %>%
    pull(country)
  
  
  df_viz_pepfar <- df_epi_pepfar %>% 
    mutate(country = "All PEPFAR") %>% 
    group_by(country, year, indicator, fill_color) %>% 
    summarise(across(c(value, value_mod), sum, na.rm = TRUE), .groups = "drop") %>% 
    mutate(val_lab = case_when(year == max(year) ~ number(value, 1, scale = 1e-3, suffix = "k")),
           max_plot_pt = max(value),
           lab_pt = case_when(year == max(year) ~ value_mod))
  
  df_viz_cntry <- df_epi_pepfar %>% 
    filter(country %in% epi_cntry) %>% 
    mutate(val_lab = case_when(year == max(year) ~ number(value, 1, scale = 1e-3, suffix = "k")),
           max_plot_pt = max(value),
           lab_pt = case_when(year == max(year) ~ value_mod),
           country = factor(country, epi_cntry)) 
  
  #calc for countries newly at epi control
  df_viz_cntry %>% 
    filter(year %in% c(2021,2022)) %>% 
    count(year, country, epi_control) %>% 
    pivot_wider(names_from = "year", values_from = "epi_control") %>% 
    rename(epicontrol_2021 = `2021`,
           epicontrol_2022 = `2022`) %>% 
    mutate(flag_new_epi = ifelse(epicontrol_2021 == FALSE & epicontrol_2022 == TRUE, TRUE, FALSE)) %>% View()

#calc for approaching epi control    
df_epi_pepfar %>% 
    mutate(val_lab = case_when(year == max(year) ~ number(value, 1, scale = 1e-3, suffix = "k")),
           max_plot_pt = max(value),
           lab_pt = case_when(year == max(year) ~ value_mod)) %>% 
    filter(year %in% c(2022),
           str_detect(region, "Africa")) %>% 
    count(year, country, declining_deaths, infections_below_deaths) %>% View()
  
    
  

# VIZ ---------------------------------------------------------------------

  
  v_p <- df_viz_pepfar %>% 
    ggplot(aes(year, value_mod, group = indicator, fill = fill_color, color = fill_color)) +
    geom_blank(aes(y = max_plot_pt)) +
    geom_blank(aes(y = -max_plot_pt)) +
    geom_area(alpha = .25) +
    geom_hline(yintercept = 0, color = grey80k) +
    geom_line() +
    geom_point(aes(y = lab_pt), na.rm = TRUE,
               shape = 21, color = "white", size = 3) +
    geom_text(aes(label = val_lab), na.rm = TRUE,
              hjust = -0.3,
              family = "Source Sans Pro Light") +
    facet_wrap(~country) +
    scale_y_continuous(labels = ~ label_number_si()(abs(.))) +
    scale_x_continuous(breaks = seq(1990, 2025, 5)) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL) +
    coord_cartesian(expand = T, clip = "off") +
    si_style_ygrid() +
    theme(axis.text.y = element_markdown())
  
  
  high_vol_cntry <- df_viz_cntry %>% 
    filter(year == max(year),
           indicator == "infections",
           value > 5000) %>% 
    count(country) %>% 
    pull(country)
  
  
  low_vol_cntry <- df_viz_cntry %>% 
    filter(year == max(year),
           indicator == "infections",
           value <= 5000) %>% 
    count(country) %>% 
    pull(country)
  
  v_c_high <- df_viz_cntry %>% 
    #filter(country %in% c(high_vol_cntry)) %>% 
    ggplot(aes(year, value_mod, group = indicator, fill = fill_color, color = fill_color)) +
    geom_blank(aes(y = max_plot_pt)) +
    geom_blank(aes(y = -max_plot_pt)) +
    geom_area(alpha = .25) +
    geom_hline(yintercept = 0, color = grey80k) +
    geom_line() +
    geom_point(aes(y = lab_pt), na.rm = TRUE,
               shape = 21, color = "white", size = 3) +
    geom_text(aes(label = val_lab), na.rm = TRUE,
              hjust = -0.3,
              family = "Source Sans Pro Light") +
    facet_wrap(~country, nrow = 2, scales = "free_y") +
    scale_y_continuous(labels = ~ label_number_si()(abs(.))) +
    scale_x_continuous(breaks = seq(1990, 2025, 10)) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL) +
    coord_cartesian(expand = T, clip = "off") +
    si_style_ygrid() +
    theme(axis.text.y = element_markdown(),
          axis.text.x = element_blank(),
          panel.spacing.x = unit(20, "pt"),
          panel.spacing.y = unit(0, "pt"))
  
  v_c_low <- df_viz_cntry %>% 
    filter(country %in% c(low_vol_cntry)) %>% 
    ggplot(aes(year, value_mod, group = indicator, fill = fill_color, color = fill_color)) +
    geom_blank(aes(y = 50000)) +
    geom_blank(aes(y = -50000)) +
    geom_area(alpha = .25) +
    geom_hline(yintercept = 0, color = grey80k) +
    geom_line() +
    geom_point(aes(y = lab_pt), na.rm = TRUE,
               shape = 21, color = "white", size = 3) +
    geom_text(aes(label = val_lab), na.rm = TRUE,
              hjust = -0.3,
              family = "Source Sans Pro Light") +
    facet_wrap(~country, nrow = 2, scales = "free_y") +
    scale_y_continuous(labels = ~ label_number_si()(abs(.))) +
    scale_x_continuous(breaks = seq(1990, 2025, 10)) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL) +
    coord_cartesian(expand = T, clip = "off") +
    si_style_ygrid() +
    theme(axis.text.y = element_markdown(),
          panel.spacing.x = unit(20, "pt"),
          panel.spacing.y = unit(0, "pt"))
  
  #patchwork
  v_p + (v_c_high/v_c_low) +  
    plot_layout(widths = c(0.75, 1)) +
    plot_annotation(title = plot_title,
                    subtitle = epi_control,
                    caption = glue("Source: {source_note} [{date_pulled}]
                        SI analytics | Ref id: {ref_id}"),
                    theme = si_style_ygrid()) &
    theme(axis.text.y = element_markdown(),
          panel.spacing.x = unit(20, "pt"),
          panel.spacing.y = unit(0, "pt"),
          plot.title = element_markdown())

  
  si_save("Graphics/08_epi_ann_unaids-pepfar-epi-control_update2022.svg")
  si_save("Graphics/08_epi_ann_unaids-pepfar-epi-control_update2022.png")
  
  
 

  
    
    

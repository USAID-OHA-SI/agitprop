# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | T.Essam | K. Srikanth USAID
# PURPOSE:  Epi control trend graphs
# LICENSE:  MIT
# DATE:     2021-07-19
# UPDATED:  2021-09-30
# NOTE:     Created based on request from FO "2 visuals for AHOP deck" -- 2021-07-16
# DATA NOTE: Data id --> "1p-fN3qDJ138uSQ4e63EYgEXrop8lhbO1"
# TODO: Rewrite to use the miindthegap pacakge to pull the data

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
  
  source("Scripts/99_utilities.R")


# GLOBAL VARIABLES --------------------------------------------------------

  authors <- c("Aaron Chafetz", "Tim Essam")
  
  # Strip excel stub and grab last part of string. 
  remove_stub <- "Data/Epidemic transition metrics_National_"
  
  extract_country <- function(x) {
    gsub('.{5}$', '', x) %>% 
    gsub(remove_stub, "", .)
    
    # Return everything up to "National_", then replace that with ""
    # gsub("^.*National_", "", .)
    # gsub("^.*_", "", .)
    }
  
  # High burden
  top3 <- c("Kenya", "Zimbabwe", "Ethiopia")
  
  
  #source info & definition of epidemic control
  source <- "UNAIDS, https://aidsinfo.unaids.org/" 
  date_pulled <- "2021-07-19"
  
  note <- str_wrap("HIV epidemic control is the point at which the number number of new HIV infections falls below the number of AIDS-related deaths", width = 40)
  
  epi_control <- str_wrap("PEPFAR defines national HIV epidemic control as the point at which the total number of new HIV infections falls below the total number of deaths from all causes among HIV-infected individuals, with both new infections and deaths among HIV-infected individual slowing and declining.", width = 100)
  
  plot_title <- "STEADY DECLINE IN THE NUMBER OF <span style= 'color:#2057a7;'> NEW HIV INFECTIONS</span> AND <span style = 'color:#c43d4d;'> AIDS-RELATED DEATHS </span> SINCE THE EARLY 2000s"
  
  
# IMPORT ------------------------------------------------------------------
  # Steps: 
  # 1) Loop over names pattern in the data folder
  # 2) Store the list of files matching the pattern we provide
  # 3) Determine the range of tabs and cells we need to read in from each file (A2:D33)
  # 4) Write a function to read in the cells from each tab
  # 5) Use purrr::map_dfr() to loop over everything and return a single data frame
  
  # Check for the pattern in the file name
  file_list <- list.files("Data", pattern = "transition metrics_National", full.names = T)
  
  # Check for the name of each tab
  sheet_list <- excel_sheets(file_list[1])
  
  # Check function for removing text from file name 
  file_list[5] %>% gsub('.{5}$', '', .) %>% gsub(remove_stub, "", .)
  
  # Read in data for 1 country, combine the two sheets into a single file
  combine_data <- function(file_list) {
    file_list %>% 
    excel_sheets() %>%
    map_dfr(~read_excel(file_list, range = "A2:D33", sheet = .x) %>% 
              mutate(operatingunit = extract_country(file_list),
                     metric = set_names(.x) %>% gsub("^.{3}", "", .)))
  }

  
  # Now we need to loop over the file_list object and run the routine
  epi_df <- 
    map_dfr(file_list, ~combine_data(.x))
  
  
  # Google drive check
  epi_gd_df <- range_read("15pUjqV1STbE2XPfqX2ZIMu8KKBsSXSNu_GOVthUuMBA", 
                          sheet = "epi_transition_metrics_ou")

  epi_gd_clean <- 
    epi_gd_df %>% 
    mutate(across(c(value:upper), ~ifelse(str_detect(., "\\&lt;"), 1, 0), .names = "{.col}_flag")) %>% 
    mutate(across(c(year, value:upper), ~str_replace_all(., ("(\\&lt;| )"), "") %>% as.numeric)) %>% 
    mutate(line_color = ifelse(str_detect(metric, "HIV"), denim, golden_sand),
           fill_color = ifelse(str_detect(metric, "HIV"), denim, old_rose)) %>% 
    group_by(operatingunit, metric) %>% 
    mutate(max_val = max(value)) %>% 
    ungroup() %>% 
    mutate(ou_order = fct_reorder(operatingunit, max_val, .desc = T))
  
# MUNGE -------------------------------------------------------------------

  
  # What does this data look like? is not numbers! Lots of characters in there, let's fix that
  str(epi_df)
  glimpse(epi_df)
  epi_df %>% count(operatingunit, metric)
  
  # Steps:
  # 1) flag any metric that has a less than sign in the number (&lt;) - want to mark these for use later
  # 2) remove the spaces in the number 10 000 --> 10000
  # 3) rename variables
  
  epi_df_clean <- 
    epi_df %>% 
    rename(year = Year,
           value = `All ages estimate`,
           lower= `Lower Estimate`,
           upper = `Upper Estimate`) %>% 
    mutate(across(c(value:upper), ~ifelse(str_detect(., "\\&lt;"), 1, 0), .names = "{.col}_flag")) %>% 
    mutate(across(c(year, value:upper), ~str_replace_all(., ("(\\&lt;| )"), "") %>% as.numeric)) %>% 
    mutate(line_color = ifelse(str_detect(metric, "HIV"), denim, golden_sand),
           fill_color = ifelse(str_detect(metric, "HIV"), denim, old_rose)) %>% 
    group_by(operatingunit, metric) %>% 
    mutate(max_val = max(value)) %>% 
    ungroup() %>% 
    mutate(ou_order = fct_reorder(operatingunit, max_val, .desc = T)) %>% 
    mutate(value_mod = ifelse(str_detect(metric, "death"), -value, value),
           ymax = ifelse(operatingunit %in% top3, 2.5e5, 4e4),
           ymin = ifelse(operatingunit %in% top3, -1e5, -2e4)) 
  
  # Wide dataset in case we need it
  epi_df_wide <- 
    epi_df_clean %>% 
    pivot_wider(names_from = metric,
                names_glue = "{metric}_{.value}",
                values_from = c(value:upper, value_flag:upper_flag))
  

# VIZ ---------------------------------------------------------------------

  str(epi_df_clean)
  
  # Reproduce the standard plots that are used to show progress
  epi_df_clean %>% 
    ggplot(aes(x = year, group = metric)) +
    geom_blank(aes(y = 0)) +
    geom_blank(aes(y = ymax)) +
    geom_line(aes(y = value, color = line_color)) +
    geom_point(data = . %>%  filter(value_flag == 1), aes(y = value, color = "white")) +
    facet_wrap(~ou_order, scales = "free_y") +
    scale_y_continuous(labels = label_number_si()) +
    scale_color_identity()+
    labs(x = NULL, y = NULL,
         title = ) +
    si_style_ygrid() +
    scale_x_continuous(breaks = seq(1990, 2020, 5)) +
    labs(x = NULL, y = NULL,
         title = plot_title, 
         caption =  glue("{source} [{date_pulled}]
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    theme(plot.title = element_markdown())
  
# New graph in FT style with geom_area reflected below
 epi_df_clean %>% 
    ggplot(aes(x = year, group = metric)) +
    geom_area(aes(y = value_mod, fill = fill_color), alpha = .25) +
    geom_line(aes(y = value_mod, color = fill_color)) +
    geom_point(data = . %>% filter(value_flag == 1), 
               aes(y = value_mod, color = "white")) +
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = value_mod, fill = fill_color), 
               shape = 21, color = "white", size = 3) +
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = value_mod, 
                  color = fill_color, 
                  label = paste0(abs(value_mod/1000) %>% comma(1.0), "K")),
              hjust = -0.3,
              family = "Source Sans Pro Light") +
    geom_hline(yintercept = 0, color = grey80k) +
    facet_wrap(~ou_order, scales = "free_y") +
    scale_y_continuous(labels = label_number_si()) +
    geom_blank(aes(y = ymin)) +
    geom_blank(aes(y = ymax)) +
    scale_color_identity() +
    scale_fill_identity()+
    labs(x = NULL, y = NULL,
         title = plot_title,
         subtitle = epi_control,  
         caption =  glue("{source} [{date_pulled}]
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid(facet_space = 0.5) +
    scale_x_continuous(breaks = seq(1990, 2025, 5)) +
    coord_cartesian(expand = T, clip = "off") +
    theme(axis.text.y = element_markdown(), 
          plot.title = element_textbox_simple(margin = margin(5.5, 0, 0, 5.5)),
          plot.subtitle = element_textbox_simple(family = "Source Sans Pro Light",
                                                 margin = margin(5.5, 5.5, 5.5, 5.5))) 
  
  ggsave("Graphics/24b_HIV_epi_control_select_ous.pdf", 
         width = 10, height = 5.625, 
         dpi = "retina", useDingbats = F, scale = 1.35)  

  
    
    

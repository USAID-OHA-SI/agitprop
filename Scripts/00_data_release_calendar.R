# PROJECT:  agiprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  data release calendar
# LICENSE:  MIT
# DATE:     2021-10-14
# UPDATED:  

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glamr)
library(lubridate)
library(glue)
library(glitr)
library(extrafont)



# GLOBAL VARIABLES --------------------------------------------------------

  curr_fy <- source_info(return = "fiscal_year")

  d_breaks <- seq.Date(as.Date(glue("{curr_fy-1}-10-01")), length.out = 6, by = "3 months")
  
# MUNGE -------------------------------------------------------------------


  df_cal <- pepfar_data_calendar %>% 
    filter(fiscal_year == curr_fy) %>% 
    mutate(quarter_end = yq(glue("{fiscal_year}_{quarter}")),
           quarter_start = quarter_end - months(3),
           data_release = as.Date(entry_close) + weeks(1),
           qtr_lab = glue("Q{quarter}")) %>% 
    select(-starts_with("entry")) %>%
    pivot_longer(starts_with("quarter_"),
                 names_prefix = "quarter_",
                 names_to = "pd_type",
                 values_to = "date")
  
  df_viz <- df_cal %>% 
    mutate(fill_color = ifelse(type == "initial", old_rose, scooter),
           lab_pt_initital = case_when(type == "initial" & pd_type == "end" ~ as.Date(glue("{fiscal_year+1}-03-15"))),
           lab_pt_clean = case_when(type == "clean" & pd_type == "end" ~ as.Date(glue("{fiscal_year+1}-07-15"))),
           lab_qtr = case_when(type == "clean" & pd_type == "start" ~ qtr_lab),
           lab_qtr_pos = case_when(!is.na(lab_qtr) ~ date + days(45)))


# VIZ ---------------------------------------------------------------------


  df_viz %>% 
    ggplot(aes(date, fct_rev(qtr_lab)), group = qtr_lab) +
    geom_line(size = 10, color = trolley_grey) +
    geom_point(aes(x = data_release, color = fill_color), size = 3) +
    geom_text(aes(x = lab_qtr_pos, label = lab_qtr), na.rm = TRUE,
              color = "white", family = "Source Sans Pro", fontface = "bold", size = 13/.pt) +
    geom_label(aes(x = lab_pt_initital, label = format(data_release, "%b %d"), fill = fill_color), na.rm = TRUE,
               color = "white", family = "Source Sans Pro SemiBold", size = 11/.pt) +
    geom_label(aes(x = lab_pt_clean, label = format(data_release, "%b %d"), fill = fill_color), na.rm = TRUE,
               color = "white", family = "Source Sans Pro SemiBold", size = 11/.pt) +
    annotate(geom = "text",
             x = as.Date(glue("{curr_fy+1}-03-15")),
             y = 4.4, label = "initial", family = "Source Sans Pro",
             color = old_rose) +
    annotate(geom = "text",
             x = as.Date(glue("{curr_fy+1}-07-15")),
             y = 4.4, label = "clean", family = "Source Sans Pro",
             color = scooter) +
    scale_x_date(date_labels = "%b", position = "top", breaks = d_breaks) +
    scale_fill_identity() +
    scale_color_identity() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = glue("FY{str_sub(curr_fy, -2)} PEPFAR MER DATA RELEASE CALENDAR")) +
    si_style_xgrid() +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size = 12, family = "Source Sans Pro SemiBold"))
         

  si_save("Images/data_calendar.png", height = 3.51, width = 4.35)
  

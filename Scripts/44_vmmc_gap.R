# PROJECT:  agitprop
# AUTHOR:   B.Kagniniwa | USAID
# PURPOSE:  VMMC Gap
# LICENSE:  MIT
# DATE:     2021-12-09

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
  
  file_nat_subnat_curr <- dir_merdata %>% 
    return_latest(pattern = "NAT_SUBNAT_.*_\\d{8}.*")
  
  file_ou_im_curr <- dir_merdata %>% 
    return_latest(pattern = "OU_IM_FY19-22_\\d{8}.*")
  
  file_psnu_im_curr <- dir_merdata %>% 
    return_latest(pattern = "PSNU_IM_FY19-22_\\d{8}.*")
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Baboyma Kagniniwa")

# IMPORT ----

  # NAT SubNat
  df_nat <- file_nat_subnat_curr %>% read_msd()
  
  # MER data
  df_msd <- file_ou_im_curr %>% read_msd()
  
  curr_fy <- df_msd %>% identifypd(pd_type = "year")
  curr_pd <- df_msd %>% identifypd(pd_type = "full")

# MUNGING ---- 

  df_nat %>% 
    filter(indicator %in% c("POP_EST", "VMMC_TOTALCIRC_NAT")) %>% 
    distinct(indicator, standardizeddisaggregate)
  
  df_pop <- df_nat %>% 
    filter(fiscal_year == curr_fy +1,
           countryname %in% c("Botswana", "Eswatini", "South Africa"),
           indicator %in% c("POP_EST"),
           standardizeddisaggregate %in% c("Age/Sex"),
           sex == "Male",
           trendscoarse == "15+") %>% 
    group_by(countryname, indicator) %>% 
    summarise(value = sum(targets, na.rm = T), .groups = "drop")
  
  df_cir <- df_nat %>% 
    filter(fiscal_year == curr_fy,
           countryname %in% c("Botswana", "Eswatini", "South Africa"),
           indicator %in% c("VMMC_TOTALCIRC_NAT"),
           standardizeddisaggregate %in% c("Age Aggregated/Sex"),
           ageasentered != "<15") %>% 
    group_by(countryname, indicator) %>% 
    summarise(value = sum(qtr4, na.rm = T), .groups = "drop")
  
  df_vmmc <- df_pop %>% bind_rows(df_cir) 
  
  df_vmmc <- df_vmmc %>% 
    group_by(countryname) %>% 
    summarise(value = value[indicator == "VMMC_TOTALCIRC_NAT"] / 
                value[indicator == "POP_EST"], .groups = "drop") %>% 
    mutate(indicator = "VMMC_COV") %>% 
    bind_rows(df_vmmc, .) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    mutate(
      cntry_label = paste0(
        "<span>**",
        countryname, "**</span><br/>",
        "<span style='display:table; padding:5px; background-color:#e07653'>",
        comma(POP_EST), "</span> Males 15+<br/>",
        "<span style='color:#e07653'>**",
        percent(1 - VMMC_COV, 1),
        "**</span> VMMC Gap"
      ))
  
  cntry_labels <- df_vmmc %>% 
    arrange(VMMC_COV) %>% 
    pull(cntry_label)
    
  
# VIZ ----

  df_vmmc %>% 
    ggplot(aes(x = reorder(countryname, VMMC_COV), y = VMMC_COV)) +
    geom_col(fill = burnt_sienna) +
    geom_hline(yintercept = 0, size = 1, color = grey90k) +
    geom_text(aes(label = percent(VMMC_COV, 1)),
              hjust = 1.2,
              size = 20, color = "white") +
    annotate(geom = "curve", 
             x = 1.3, y = .4,
             xend = 2, yend = .328,
             curvature = .3, size = 1, color = grey90k,
             arrow = arrow(length = unit(5, "pt"))) +
    annotate(geom = "text",
             x = 1.0, y = .42,
             label = "% of males 15+\ncircumcised to date",
             size = 8, color = grey90k, hjust = 1) +
    scale_x_discrete(labels = cntry_labels) +
    coord_flip() +
    labs(x = "", y = "", 
         #title = "Zeroing in on VMMC",
         caption = glue("Source: PEPFAR {source_info()} NAT-SUBNAT\n% Gap = (POP_EST - VMMC_TOTALCIRC_NAT) / POP_EST\nNote: National and historical VMMC data is only available for these 3 countries\nProduced by USAID's Office of HIV-AIDS/SIEI/SI Core Analytics, {format(Sys.Date(), '%Y-%m-%d')}")) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_markdown(size = 20))
  
  si_save(file.path(dir_graphics, "USAID - OU VMMC Gap.png"))
  
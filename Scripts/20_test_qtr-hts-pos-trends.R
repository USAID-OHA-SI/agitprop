# PROJECT:  agitprop
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  HTS_POS scale up since PEPFAR start
# LICENSE:  MIT
# DATE:     2021-11-30
# UPDATED: 

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
library(janitor)
library(lubridate)

# GLOBAL VARIABLES --------------------------------------------------------

authors <- c("Karishma Srikanth", "Aaron Chafetz", "Tim Essam")

#clean number
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}


# IMPORT ------------------------------------------------------------------
  
load_secrets()

#Source: PEPFAR Spotlight (public)
df_hist <- read_csv("Data/Country and Regional Targets_Results 2004-2016.csv",
                    na = c("", "NA", "null"),
                    col_types = c(Year = "i",
                                  `Measure Value` = "d",
                                  .default = "c")) %>% 
  clean_names()

#Current MSD
df <- si_path() %>% 
  return_latest("OU_IM_FY19") %>% 
  read_msd() %>% 
  resolve_knownissues()

#Archived MSD
df_arch <- si_path() %>% 
  return_latest("OU_IM_FY15") %>% 
  read_msd()

# MUNGE -------------------------------------------------------------------

#source info
msd_source <- source_info()

#current period
curr_fy <- identifypd(df, "year")
curr_qtr <- identifypd(df, "quarter")
curr_pd <- source_info(return = "period")


#HTS_TST_POS dataset
df_hts <- df %>% 
  bind_rows(df_arch) %>% 
  filter(indicator == "HTS_TST_POS",
         standardizeddisaggregate == "Total Numerator") 

df_hts <- df_hts %>%
  bind_rows(df_hts %>% mutate(fundingagency = "PEPFAR")) %>% 
  group_by(fiscal_year, fundingagency) %>% 
  summarise(across(c(cumulative), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(fundingagency %in% c("PEPFAR", "USAID")) %>% 
  mutate(source = "MSD") %>% 
  rename(value = cumulative) %>% 
  pivot_wider(names_from = fundingagency, values_from = value) %>%
  group_by(fiscal_year) %>%
  mutate(share = USAID / PEPFAR)  %>%
  pivot_longer(cols = PEPFAR:USAID, names_to = "fundingagency")

#historical data
df_hist_clean <- df_hist %>% 
  filter(indicator_short_name %in% c("HIV Tested Positive"),
         measure_name == "Results",
         country_region != "Global",
         dsd_ta == "DSD+TA") %>% 
  mutate(fundingagency = "PEPFAR") %>% 
  group_by(fiscal_year = year, fundingagency) %>% 
  summarise(value = sum(measure_value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(type = "results",
         source = "Spotlight") %>% 
  filter(!fiscal_year %in% unique(df_hts$fiscal_year))

df_hts <- bind_rows(df_hist_clean, df_hts) %>% 
  rename(hts_pos = value)

#prep viz dataset
df_hts_viz <- df_hts %>% 
  select(-c(type)) %>% 
  filter(fiscal_year <= 2021) %>% 
  arrange(fundingagency, fiscal_year) %>% 
  mutate(agency_alpha = ifelse(fundingagency == "USAID", 1, 0.6),
         label_hts = glue("patients receiving\n positive HIV results"))

#get numbers for title
title_info_usaid <- df_hts_viz %>% 
  filter(fiscal_year == 2021,
         fundingagency == "USAID") %>% 
  select(fiscal_year, hts_pos, share) %>% 
  mutate(share = percent(round(share, 2)),
         hts_pos = round(hts_pos, 2),
         hts_pos = hts_pos %>%  clean_number()) 

title_info_pepfar <- df_hts_viz %>% 
  filter(fiscal_year == 2021,
         fundingagency == "PEPFAR") %>% 
  select(fiscal_year, hts_pos, share) %>% 
  mutate(share = percent(round(share, 2)),
      #   hts_pos = round(hts_pos, 2),
         hts_pos = hts_pos %>%  clean_number()) 

#VIZ -----------------------------------------------------------

df_hts_viz %>% 
  mutate(value_label = ifelse(fundingagency == "USAID", clean_number(hts_pos, 1), NA)) %>% 
  ggplot(aes(fiscal_year, hts_pos)) +
  geom_col(aes(alpha = agency_alpha), fill = old_rose, position = "identity", na.rm = TRUE) +
  geom_hline(yintercept = 0, color = "gray40") +
  #geom_hline(yintercept = seq(1e6, 3e6, 1e6), color = "white") +
  #facet_grid(label_hts~., switch = "y") +
  #geom_text(aes(label = value_label), na.rm = TRUE, vjust = -0.5, 
    #        family = "Source Sans Pro") +
  geom_text(data = . %>% filter(fiscal_year == curr_fy, fundingagency == "USAID"), 
            aes(label = percent(share, 1)), 
            vjust = 1.2, family = "Source Sans Pro", color = "white")  +
  scale_y_continuous(labels = label_number_si(),
                     position = "right") +
  scale_x_continuous(expand = c(.005, .005),
                     n.breaks = unique(df_hts_viz$fiscal_year) %>% length())+
  scale_alpha_identity() +
  labs(x = NULL, y = NULL, fill = NULL,
       title = glue("In {str_sub(curr_pd, 1, 4)}, {title_info_usaid$share} of the {title_info_pepfar$hts_pos}+ newly \\
                     identified patients in PEPFAR were identified by USAID testing services") %>%  toupper(),
       caption = glue("Source: {msd_source} (including FY15-18) + Spotlight FY04-14
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_ygrid() +
  theme(strip.placement = "outside",
        strip.text.y = element_markdown(family = "Source Sans Pro SemiBold", hjust = .5, 
                                        color = old_rose)) +
  coord_cartesian(expand = F)

si_save("Graphics/20_test_qtr.svg")
si_save("Images/20_test_qtr.png")

# PROJECT:  agitprop
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  HTS scale up since PEPFAR start
# LICENSE:  MIT
# REF ID:   08e5947b 
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


ref_id <- "08e5947b"

#clean number function
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

get_metadata()


# IMPORT ------------------------------------------------------------------

#Source: PEPFAR Spotlight (public)
df_hist <- read_csv("Data/Country and Regional Targets_Results 2004-2016.csv",
                    na = c("", "NA", "null"),
                    col_types = c(Year = "i",
                                  `Measure Value` = "d",
                                  .default = "c")) %>% 
  clean_names()

#Current MSD
df <- si_path() %>% 
  return_latest("OU_IM_FY21") %>% 
  read_msd()

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
pd_c_or_i <- msd_source %>% str_sub(1,7) # period w/ clean or initial

#HTS_TST dataset
df_hts <- df %>% 
  bind_rows(df_arch) %>% 
  filter(indicator == "HTS_TST",
         standardizeddisaggregate == "Total Numerator") 

df_hts <- df_hts %>%
  bind_rows(df_hts %>% mutate(funding_agency = "PEPFAR")) %>% 
  group_by(fiscal_year, funding_agency) %>% 
  summarise(across(c(cumulative), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(funding_agency %in% c("PEPFAR", "USAID")) %>% 
  mutate(source = "MSD") %>% 
  rename(value = cumulative) %>% 
  pivot_wider(names_from = funding_agency, values_from = value) %>%
  group_by(fiscal_year) %>%
  mutate(share = USAID / PEPFAR)  %>%
  pivot_longer(cols = PEPFAR:USAID, names_to = "funding_agency")

df_hist_clean <- df_hist %>% 
  filter(indicator_short_name %in% c("HIV Testing and Counseling Services"),
         measure_name == "Results",
         country_region != "Global",
         dsd_ta == "DSD+TA") %>% 
  mutate(funding_agency = "PEPFAR") %>% 
  group_by(fiscal_year = year, funding_agency) %>% 
  summarise(value = sum(measure_value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(type = "results",
         source = "Spotlight") %>% 
  filter(!fiscal_year %in% unique(df_hts$fiscal_year))

df_hts <- bind_rows(df_hist_clean, df_hts) %>% 
  rename(hts_tst = value)

### FIGURE OUT WHAT THIS MEANS AND WHETHER TO REMOVE 2021

#prep viz dataset
df_hts_viz <- df_hts %>% 
  select(-c(type)) %>% 
 # filter(fiscal_year <= 2021) %>% 
  arrange(funding_agency, fiscal_year) %>% 
  mutate(agency_alpha = ifelse(funding_agency == "USAID", 1, 0.6),
         agency_alpha = ifelse(fiscal_year == 2023 & funding_agency == "PEPFAR", 0.3, agency_alpha),
         agency_alpha = ifelse(fiscal_year == 2023 & funding_agency == "USAID", 0.5, agency_alpha),
         label_hts = glue("patients receiving\n HIV Testing Services"))

#get numbers for title
title_info_usaid <- df_hts_viz %>% 
  filter(fiscal_year == 2021,
         funding_agency == "USAID") %>% 
  select(fiscal_year, hts_tst, share) %>% 
  mutate(share = percent(round(share, 2)),
    hts_tst = round(hts_tst, 2),
    hts_tst = hts_tst %>%  clean_number()) 

title_info_pepfar <- df_hts_viz %>% 
  filter(fiscal_year == 2021,
         funding_agency == "PEPFAR") %>% 
  select(fiscal_year, hts_tst, share) %>% 
  mutate(share = percent(round(share, 2)),
         hts_tst = round(hts_tst, 2),
         hts_tst = hts_tst %>%  clean_number()) 

#VIZ ---------------------------------------------------------------------------

df_hts_viz %>% 
  mutate(value_label = ifelse(funding_agency == "USAID", clean_number(hts_tst), NA)) %>% 
  ggplot(aes(fiscal_year, hts_tst)) +
  geom_col(aes(alpha = agency_alpha), fill = burnt_sienna, position = "identity", na.rm = TRUE) +
  geom_hline(yintercept = 0, color = "gray40") +
  #geom_hline(yintercept = seq(2.5e7, 1e8, 2.5e7), color = "white") +
  #facet_grid(label_hts~., switch = "y") +
 geom_text(aes(label = value_label), na.rm = TRUE, vjust = -0.5, color = grey90k,
  family = "Source Sans Pro") +
  scale_y_continuous(labels = label_number_si(),
                     position = "right") +
  scale_x_continuous(expand = c(.005, .005),
                     n.breaks = unique(df_hts_viz$fiscal_year) %>% length())+
  scale_alpha_identity() +
  labs(x = NULL, y = NULL, fill = NULL,
       title = glue("As of {curr_pd}, USAID provided HIV testing services to \\
                       {title_info_usaid$share} \\
                       of PEPFAR's {title_info_pepfar$hts_tst} patients receiving HIV testing services") %>%  toupper(),
       caption = glue("Source: {metadata$source} (including FY15-18) + Spotlight FY04-14 | ref_id: {ref_id}")) +
  si_style_ygrid()

si_save("Graphics/19_test_qtr.svg")

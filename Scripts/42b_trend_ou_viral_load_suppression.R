# PROJECT:  agitprop
# AUTHOR:   K.Srikanth | USAID
# PURPOSE:  VLS Trend by OU - revsied for GH Portfolio Review
# REF ID:   826cc3f4
# LICENSE:  MIT
# DATE:     2021-12-07
# NOTE: adapted from Scripts/42_trend_ou_viral_load_coverage

# DEPENDENCIES ----

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
library(lubridate)
library(gt)


# GLOBAL VARS ----

ref_id <- "826cc3f4"

dir_merdata <- si_path()
dir_graphics <- "./Graphics"

file_ou_im_curr <- dir_merdata %>% 
  return_latest(pattern = "OU_IM_FY21-23_\\d{8}.*")

file_ou_im_prev <- dir_merdata %>% 
  return_latest(pattern = "OU_IM_FY15-20_\\d{8}.*")

epi_cntries <- c("Kenya", "Uganda", "Botswana", 
                 "Lesotho", "Eswatini", "Namibia")

msd_source <- source_info(file_ou_im_curr)
today <-  today()

get_metadata()r

# IMPORT ----

#MER data
df_msd1 <- file_ou_im_prev %>% read_msd()
df_msd2 <- file_ou_im_curr %>% read_msd()

df_msd <- df_msd1 %>% 
  bind_rows(df_msd2)

# df_msd <- file_ou_im %>% 
#   c(file_ou_im_prev) %>% 
#   map_dfr(read_msd)

df_msd <- df_msd %>% 
  clean_indicator() %>% 
  clean_agency() %>% 
  clean_countries(colname = "operatingunit")

df_msd %>% glimpse()

df_msd %>% 
  distinct(fiscal_year) %>% 
  arrange(fiscal_year) %>% 
  pull()

curr_pd <- df_msd %>% identifypd()
curr_fy <- df_msd %>% identifypd(pd_type = "year")

# MUNGE MER ----

df_tx_ou <- df_msd %>% 
  filter(
    fiscal_year != curr_fy + 1,
    funding_agency == "USAID",
    indicator %in% c("TX_CURR", "TX_PVLS_D", "TX_PVLS"),
    standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")
  ) %>% 
  group_by(fiscal_year,
           funding_agency, 
          # operatingunit,
           indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  dplyr::select(-period_type) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  group_by(funding_agency
           #,operatingunit
           ) %>% 
  mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period),
         VLC = TX_PVLS_D / TX_CURR_LAG2,
         VLS = TX_PVLS/TX_PVLS_D) %>% 
  ungroup() %>% 
  relocate(TX_CURR_LAG2, .before = TX_CURR) %>% 
  filter(!is.na(TX_PVLS_D), TX_PVLS_D > 0)

df_vls <-df_tx_ou %>% 
  filter(period == metadata$curr_pd) %>% 
  arrange(desc(VLS)) %>% 
  mutate(bar_alpha = case_when(VLS >=.95 ~ 0.9,
                               VLS >=0.9 & VLS < .95 ~ 0.5,
                               TRUE ~ 0.2))

viz_vls <- df_vls %>% 
  ggplot(aes(x = VLS, y = fct_reorder(operatingunit, VLS), alpha = bar_alpha)) +
  geom_col(fill = scooter) +
  si_style_xgrid() +
  geom_vline(xintercept = 0.95, size = 1, linetype = "dotted", color = trolley_grey) +
  geom_vline(xintercept = 0.9, size = 1, linetype = "dotted", color = glitr::trolley_grey) +
  scale_x_continuous(label = percent_format(1)) +
  scale_alpha_identity() +
  labs(x = NULL,
       y = NULL,
       caption = metadata$caption,
       title = "USAID PROGRAMS IN 14 OPERATING UNITS HAVE REACHED 95% VIRAL LOAD SUPPRESSION, WITH MORE WORK NEEDED TO REACH THE VIRAL LOAD COVERAGE RATE OF 90%")

si_save("Graphics/gh_review_vls.svg")

df_vlc <-df_tx_ou %>% 
  filter(period == metadata$curr_pd) %>% 
  arrange(desc(VLC)) %>% 
  mutate(bar_alpha = case_when(VLC >=.95 ~ 0.9,
                               VLC >=0.9 & VLC < .95 ~ 0.5,
                               TRUE ~ 0.2))

viz_vlc <-df_vlc %>% 
 # filter(operatingunit != "Cote d'Ivoire") %>% 
  ggplot(aes(x = VLC, y = fct_reorder(operatingunit, VLS), alpha = bar_alpha)) +
  geom_col(fill = moody_blue) +
  si_style_xgrid() +
  geom_vline(xintercept = 0.95, size = 1, linetype = "dotted", color = trolley_grey) +
  geom_vline(xintercept = 0.9, size = 1, linetype = "dotted", color = glitr::trolley_grey) +
  scale_x_continuous(label = percent_format(1), limits = c(0,1.3)) +
  scale_alpha_identity() +
  labs(x = NULL, y = NULL,
       caption = metadata$caption)

viz_vls + viz_vlc

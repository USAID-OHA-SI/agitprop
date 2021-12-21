# Project: agitprop
# Author: T Essam | K Srikanth
# Purpose: Show LP contributions to HTS_TST_POS
# License: MIT
# Date: 2021-11-24

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
library(googlesheets4)


# GLOBALS -----------------------------------------------------------------

load_secrets()
authors <- c("Karishma Srikanth", "Aaron Chafetz", "Tim Essam")

msd_path <- si_path() %>% return_latest("OU_IM_FY19")
msd_path2 <- si_path() %>% return_latest("OU_IM_FY15")

#source info
curr_pd <- source_info(msd_path, return = "period")
curr_fy <- source_info(msd_path, return = "fiscal_year")
msd_source <- source_info(msd_path)

pull_figures <- function(df, indic, partner, metric) {
  df %>% 
    filter(indicator == indic, 
           partner_type == partner, 
           fiscal_year == curr_fy) %>% 
    select({{metric}}) %>% 
    pull()
}


# IMPORT ------------------------------------------------------------------

#Current MSD
df <- read_msd(msd_path)
df_arch <- read_msd(msd_path2)


#Read in the google sheet hyperfile with local partner
sheet_id <- "1MQviknJkJDttGdNEJeNaYPKmHCw6BqPuJ0C5cslV5IE"

df_partner <- read_sheet(sheet_id, sheet = "MechID-PartnerType", range = "A:B") %>% 
  clean_names() %>% 
  rename(mech_code = mechanism_id) %>% 
  mutate(mech_code = as.character(mech_code),
         partner_type = case_when(partner_type == "Regional" ~ "Local",
                                  partner_type == "TBD Local" ~ "Local", TRUE ~ partner_type))


# MUNGE -------------------------------------------------------------------

df_hts <- df %>% 
  bind_rows(df_arch) %>% 
  filter(fundingagency == "USAID",
         indicator %in% c("HTS_TST_POS"),
         standardizeddisaggregate == "Total Numerator",
         fiscal_year <= curr_fy) %>% 
  group_by(mech_code) %>% 
  left_join(df_partner, by = c("mech_code")) %>% 
  group_by(fiscal_year, fundingagency, indicator, partner_type) %>% 
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(partner_type != "TBD") %>% 
  group_by(fiscal_year, indicator) %>% 
  mutate(total = sum(cumulative),
         share = cumulative / total) %>% 
  ungroup()


# PLOT --------------------------------------------------------------------

min_yr <- df_hts %>% 
  select(fiscal_year) %>% 
  filter(fiscal_year == min(fiscal_year)) %>% 
  distinct(fiscal_year) %>% pull()  

tst_latest <- pull_figures(df_hts, "HTS_TST_POS", "Local", cumulative)
tst_max <- pull_figures(df_hts, "HTS_TST_POS", "Local", total)
tst_share <- pull_figures(df_hts, "HTS_TST_POS", "Local", share)

#Testing
df_hts %>% 
  filter(indicator == "HTS_TST_POS", partner_type == "Local") %>% 
  ggplot(aes(x = fiscal_year)) +
  geom_col(aes(y = total), fill = scooter_med, alpha = 0.85) +
  geom_col(aes(y = cumulative), fill = scooter) +
  geom_hline(yintercept = seq(0.5e6, 1.5e6, 0.5e6), color = "white", size = 0.5) +
  geom_label(aes(y = cumulative, label = percent(share, 1)), 
             size = 12/.pt, family = "Source Sans Pro", vjust = 1.15) +
  geom_label(aes(y = tst_max, x = curr_fy, label = "Total HTS_TST_POS"),
             size = 12/.pt, family = "Source Sans Pro", color = grey90k, vjust = 1, alpha = 0.85) +
  geom_text(aes(y = tst_latest, x = curr_fy, label = "Local Partner\n Contribution"), 
            color = grey10k,
            size = 12/.pt, family = "Source Sans Pro", color = grey90k, vjust = 2) +
  scale_y_continuous(labels = label_number_si(accuracy = 0.1), position = "right", lim = c(0, 1.75e6)) +
  scale_x_continuous(breaks = seq(min_yr, curr_fy, 1)) +
  si_style_xline(text_scale = 1.25) +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = NULL, 
       title = glue("LOCAL PARTNERS CONTINUE TO SUPPORT TESTING EFFORTS, ACCOUNTING FOR {label_number_si()(tst_latest)} POSITIVE TEST RESULTS
                    NEARLY 1/2 THE TOTAL SHARE OF POSITIVE TEST RESULTS IN {curr_fy}"),
       caption = glue("Source: {msd_source}
                      SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"))

si_save("Images/22_treat_qtr_LP_contribution_HTS_TST_POS.png", scale = 1.25)






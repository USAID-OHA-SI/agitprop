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

df_ovc <- df %>% 
  bind_rows(df_arch) %>% 
  filter(fundingagency == "USAID",
         indicator %in% c("OVC_SERV"),
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

min_yr <- df_ovc %>% 
  select(fiscal_year) %>% 
  filter(fiscal_year == min(fiscal_year)) %>% 
  distinct(fiscal_year) %>% pull()  

ovc_latest <- pull_figures(df_ovc, "OVC_SERV", "Local", cumulative)
ovc_max <- pull_figures(df_ovc, "OVC_SERV", "Local", total)
ovc_share <- pull_figures(df_ovc, "OVC_SERV", "Local", share)


#Testing
df_ovc %>% 
  filter(indicator == "OVC_SERV", partner_type == "Local") %>% 
  ggplot(aes(x = fiscal_year)) +
  geom_col(aes(y = total), fill = burnt_sienna_light, alpha = 0.85) +
  geom_col(aes(y = cumulative), fill = burnt_sienna) +
  geom_hline(yintercept = seq(2e6, 6e6, 2e6), color = "white", size = 0.5) +
  geom_label(aes(y = cumulative, label = percent(share, 1)), 
             size = 12/.pt, family = "Source Sans Pro", vjust = 1.15) +
  geom_label(aes(y = ovc_max, x = curr_fy, label = "Total OVC_SERV"),
             size = 12/.pt, family = "Source Sans Pro", color = grey90k, vjust = 1, alpha = 0.85) +
  geom_text(aes(y = ovc_latest, x = curr_fy, label = "Local Partner\n Contribution"), 
            color = grey10k,
            size = 12/.pt, family = "Source Sans Pro", color = grey90k, vjust = 2) +
  scale_y_continuous(labels = label_number_si(), position = "right", lim = c(0, 6.5e6)) +
  scale_x_continuous(breaks = seq(min_yr, curr_fy, 1)) +
  si_style_xline(text_scale = 1.25) +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = NULL,
       title = glue("LOCAL PARTNERS CONTINUE TO EXPAND SERVICES TO ORPHANS AND VULNERABLE CHILDREN, ACCOUNTING FOR {label_number_si(0.01)(ovc_latest)} OVC_SERV
                    MORE THAN 1/2 OF THE USAID TOTAL IN {curr_fy}"),
       caption = glue("Source: {msd_source}
                      SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"))

si_save("Images/12_prev_semi_LP-contribution-OVC_SERV.png", scale = 1.25)






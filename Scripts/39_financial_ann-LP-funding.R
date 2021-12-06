# PROJECT: agitprop
# AUTHOR: K Srikanth | SI
# PURPOSE: LP Funding trends
# LICENSE: MIT
# DATE: 2021-12-02
# NOTES: adapted from catch-22/Scripts/2021115_Global Planning Meeting/gpm_localpartner_funding.R

# LOCALS & SETUP ============================================================================

# Libraries
library(glitr)
library(glamr)
library(tidyverse)
library(gophr)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(here)
library(patchwork)
library(googlesheets4)
library(janitor)
library(ggnewscale)

# Set paths  
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"

# GLOBAL VARIABLES --------------------------------------------------------

authors <- c("Karishma Srikanth", "Aaron Chafetz", "Tim Essam")

#clean number
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}


# LOAD DATA ============================================================================  

#COP Matrix LP funding googlesheet
sheet_id <- "14m5Z7WzJyRpE_EGOETbYaE1RglnRaBtqb1CdDTqDA-c"

df <- read_sheet(sheet_id, sheet = "LP% Projections (SBU)") %>%
  clean_names()

#source info
curr_pd <- identifypd(df)
curr_fy <- identifypd(df, "year")
msd_source <- source_info()


# MUNGE ===============================================================================

df_funding <- df %>% 
  filter(cop != "COP24",
         cop != "COP25",
         adjusted_category != "%LP",
         ou_type == "LTS") %>% 
  select(-c(date_updated)) %>% 
  group_by(cop, adjusted_category) %>% 
  summarise(across(budget, sum, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = adjusted_category, values_from = budget) %>% 
  mutate(share = `LPBudget` / `Budget`) %>% 
  group_by(cop) %>% 
  mutate(target = Budget *0.7) %>% 
  pivot_longer(cols = Budget:LPBudget, names_to = "category", values_to = "value") %>% 
  mutate(fill_color = ifelse(category == "LPBudget", old_rose, trolley_grey_light),
         fill_color = ifelse(cop %in% c("COP22", "COP23") & category == "LPBudget", old_rose_light, fill_color)) 

#VIZ ===============================================================================

#Title info
title_info <- df_funding %>% 
  filter(cop == "COP21",
         category == "LPBudget") %>% 
  select(cop, share, category, value) %>% 
  mutate(share = percent(round(share, 2)),
         #   hts_pos = round(hts_pos, 2),
         value = value %>%  clean_number()) 


df_funding %>% 
  mutate(share = ifelse(category == "LPBudget", share, NA)) %>% 
  #mutate(share = ifelse(partner_type == "Local", share, NA)) %>% 
  ggplot(aes(cop, value)) +
  geom_col(aes(fill = fill_color),
           position = "identity") +
  geom_errorbar(aes(y = target, ymax = target, ymin = target, color = trolley_grey), linetype = "dashed", size = 0.5) +
  #geom_hline(yintercept = seq(2e6, 6e6, 2e6), color = "white") +
  scale_y_continuous(labels = label_number_si(), #change 2B to 1.5B
                     position = "right") +
  # scale_x_continuous(expand = c(.005, .005),
  #   n.breaks = unique(df_funding$cop) %>% length())+
  geom_text(aes(label = percent(share, 1), vjust = -1, 
                size = 12/.pt, family = "Source Sans Pro")) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_alpha_identity() +
  si_style_nolines() +
  new_scale_fill() +
  scale_fill_si(palette = "old_roses", lim = c(0.4, 1), alpha = 0.85, labels = percent,
                oob = squish) +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL,
       title = "USAID/PEPFAR has prioritized shifting programs and funding to local partners" %>% toupper(),
       subtitle = glue("{title_info$share} of all USAID/PEPFAR COP21 program funding is through Local Partners"),
       caption = glue("Note: LTS OUs only. Percentages exclude GHSC-PSM/RTK and M&O,
       Source: SBU Local Partner Strategy Dashboard
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"))

si_save("Graphics/39_financial_ann.svg")

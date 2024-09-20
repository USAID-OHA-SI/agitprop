## PROJECT: groundhog_day
## AUTHOR:  K. Srikanth | USAID
## PURPOSE: 95's Achievement
# REF ID:   5ba96db8 
## LICENSE: MIT
## DATE:    2022-06-08
# UPDATED: 2023-08-25
## NOTE: agitprop/Scripts/06_epi_ann-90s.R


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glamr)
library(glitr)
library(googlesheets4)
library(extrafont)
library(scales)
library(tidytext)
library(glue)
library(mindthegap)

# GLOBAL VARIABLES --------------------------------------------------------

#creds  
load_secrets()

authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")

#goal
goal <- 95

#indicators
ind_sel <- c("Percent Known Status of PLHIV","Percent on ART with Known Status", "Percent VLS on ART")

ref_id <- "5ba96db8"


# IMPORT ------------------------------------------------------------------

#Cascade %
df_unaids <- pull_testtreat(pepfar_only = TRUE)

#PLHIV number
df_est <- pull_estimates(pepfar_only = TRUE)

#PEPFAR select list
# pepfar_cntry <- pepfar_country_list %>% 
#   filter(str_detect(operatingunit, "Region", negate = TRUE)) %>% 
#   pull(country)
# 
# pepfar_cntry <- pepfar_country_list %>% 
#   distinct(country) %>% pull()

#select only philippines from Asia region + WAR

asia_ou_filter_out <- pepfar_country_list %>% 
  filter(operatingunit == "Asia Region" & country != "Philippines", negate = TRUE) %>% 
  pull(country)

#PEPFAR select list
pepfar_cntry <- pepfar_country_list %>% 
  filter(str_detect(operatingunit, "Western Hemisphere Region", negate = TRUE)) %>% 
  filter(country %ni% asia_ou_filter_out) %>%
  pull(country)


# MUNGE -------------------------------------------------------------------

#num PLHIV
df_est <- df_est %>% 
  filter(country %in% pepfar_cntry,
         indicator == "Number PLHIV",
         #  stat == "est",
         age == "All",
         sex == "All") %>% 
  group_by(country) %>% 
  summarise(estimate = sum(estimate, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename(PLHIV = estimate)

#Cascade
df_unaids <- df_unaids %>% 
  filter(year == max(year),
         sex == "All",
         # stat == "est",
         age == "All",
         country %in% pepfar_cntry,
         indicator %in% ind_sel)

df_viz <- df_unaids %>% 
  select(-c(lower_bound:upper_bound)) %>% 
  left_join(df_est, by = c("country")) %>% 
  filter(country != "Ukraine") %>%
  mutate(country = case_when(country == "Democratic Republic of the Congo" ~ "DRC",
                             country == "Dominican Republic" ~ "DR", 
                             TRUE ~ country),
         indicator = case_when(indicator == "Percent on ART with Known Status" ~ "On Treatment",
                               indicator == "Percent Known Status of PLHIV" ~ "Known Status",
                               indicator == "Percent VLS on ART" ~ "Virally Suppressed",
                               TRUE ~ indicator),
         PLHIV = ifelse(is.na(PLHIV), 0, PLHIV))

#BURUNDI AND MALAWI - issues because 1st 95 and 3 95 are the same
df_viz <- df_viz %>%
  rename(value = estimate) %>% 
  group_by(country) %>% 
  mutate(value = round(value, 2),
         grouping = case_when(value == min(value, na.rm = TRUE) ~ indicator),
         grouping = case_when(min(value, na.rm = TRUE) >= goal ~ "Achieved", #"Z_Achieved",
                              #country == "Eswatini" ~ "Z_Achieved",
                              #country == "Zambia" & indicator == "Virally Suppressed" ~ NA_character_,
                              TRUE ~ grouping),
         gap = case_when(value == min(value, na.rm = TRUE) & value < goal ~ goal-value,
                         value == min(value, na.rm = TRUE) & grouping == "Achieved" ~ 1-value,
                         TRUE ~ 0),
         achv = case_when(value == min(value, na.rm = TRUE) & value < goal ~ value),
         # gap = ifelse(iso %in% c("BDI", "MWI") & grouping == "Virally Suppressed", 0, gap),
         # achv = ifelse(iso %in% c("BDI", "MWI") & grouping == "Virally Suppressed", NA, achv),
         # grouping = ifelse(iso %in% c("BDI", "MWI"), "Known Status", grouping),
         dot_color = case_when(grouping == "Known Status" ~ "#009ee3",
                               grouping == "On Treatment" ~ "#009ee3",
                               grouping == "Virally Suppressed" ~ "#009ee3",
                               grouping == "Achieved" ~ "#dd052a",
                               # grouping == "Z_Achieved" ~ genoa,
                               TRUE ~ "#a8e5ff")) %>% 
  fill(grouping, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(gap_bar = case_when(value < goal ~ value),
         country = reorder_within(country, gap, grouping, max, na.rm = TRUE))

# PLOT --------------------------------------------------------------------

epi_ctrl_cnt <- df_viz %>% 
  filter(grouping == "Achieved",
         !is.na(value)) %>% 
  distinct(country) %>% 
  nrow()

df_viz %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(value, country, color = dot_color)) +
  geom_vline(xintercept = goal, linetype = "dashed") + 
  geom_linerange(aes(xmin = gap_bar, xmax = goal), color = "gray90",
                 size = 2, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  scale_y_reordered(limits = rev) +
  scale_x_continuous(labels=function(x) paste0(x,"%")) +
  scale_color_identity() +
  facet_grid(grouping~indicator, scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL, color = NULL,
       title = glue("AS OF {df_viz$year %>% unique()}, {epi_ctrl_cnt} PEPFAR COUNTRIES HAVE ACHIEVED THE UNAIDS' 2030 FAST TRACK TARGETS"),
       caption = glue("{source_note}
                       Ref id: {ref_id}")) +
  si_style_xgrid() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(.5, "lines"))

si_save("Graphics/06_epi_ann-95s_2023_update.svg")

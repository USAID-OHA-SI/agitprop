# PROJECT:  agitprop
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  USAID global OVC support
# REF ID:   daf92875 
# LICENSE:  MIT
# DATE:     2021-12-02
# NOTE:     adapted from "catch-22/Scripts/2021_08_13_ovcwork.R"

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
library(packcircles)

#clean number
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

# IMPORT ------------------------------------------------------------------

df <- si_path() %>% 
  return_latest("OU_IM_FY20") %>% 
  read_msd() 

df_arch <- si_path() %>% 
  return_latest("OU_IM_FY15") %>% 
  read_msd()


# GLOBAL VARIABLES --------------------------------------------------------

authors <- c("Karishma Srikanth", "Aaron Chafetz", "Tim Essam")

#source info
curr_pd <- identifypd(df)
curr_fy <- identifypd(df, "year")
msd_source <- source_info()

ref_id <- "daf92875"

# Function to group and sum
group_sum <- function(.data, ...){
  .data %>% 
    group_by(indicator, fiscal_year, ...) %>% 
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop")
}


# TRENDS IN KNOWN STATUS PROXY (didnt include this) --------------------------

# df_knowstatus <- df %>% 
#   bind_rows(df_arch) %>% 
#   filter((indicator == "OVC_SERV" & standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex") & trendscoarse == "<18") |
#            (indicator == "OVC_HIVSTAT"& standardizeddisaggregate == "ReportedStatus"),
#           funding_agency == "USAID",
#          fiscal_year >=2018) %>%
#   mutate(otherdisaggregate = ifelse(is.na(otherdisaggregate), "NA", otherdisaggregate)) %>% 
#   filter(otherdisaggregate != "No HIV Status") %>% 
#   group_by(fiscal_year, indicator) %>% 
#   summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   reshape_msd() %>% 
#   arrange(period) %>% 
#   pivot_wider(names_from = indicator) %>% 
#   mutate(knownstat = OVC_HIVSTAT/OVC_SERV)
# 
# #grab the % know status for max period
# latest_stat <- df_knowstatus %>% 
#   filter(period == max(period)) %>% 
#   pull()
# 
# #grab the max period
# latest_pd <- df_knowstatus %>% 
#   slice_max(order_by = period, n = 1) %>% 
#   pull(period)
# 
# #visual 1: OVC known status
# v1 <- df_knowstatus %>% 
#   filter(!is.na(knownstat)) %>% 
#   ggplot(aes(period, knownstat, group = period_type)) +
#   geom_blank(aes(y = 1.1 * knownstat)) +
#   geom_line(size = 1.5, color = scooter_light) +
#   geom_point(shape = 21, color = scooter_light, fill = scooter_light, size = 12, stroke = 2) +
#   geom_text(aes(label = percent(knownstat, 1)),
#             family = "Source Sans Pro", size = 12/.pt) +
#   expand_limits(y = .2) +
#   labs(
#        x = NULL, y = NULL) +
#   si_style_nolines() +
#   theme(axis.text.y = element_blank(),
#         axis.text.x = element_blank()) +
#   coord_cartesian(expand = F, clip = "off")


# HIV+ OVC (BAR CHART) --------------------------------------------

df_hiv_ovc <- df %>% 
  bind_rows(df_arch) %>% 
  filter((indicator == "OVC_SERV" & standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex") & trendscoarse == "<18") |
           (indicator == "OVC_HIVSTAT"& (standardizeddisaggregate == "ReportedStatus" | standardizeddisaggregate == "StatusPosART")),
          funding_agency == "USAID",
         fiscal_year >=2018) 

#2018 - munge for 2018 because disaggs different
df_hiv_ovc18 <- df_hiv_ovc %>% 
  filter(fiscal_year == 2018,
         standardizeddisaggregate == "StatusPosART") %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  arrange(period) %>% 
  pivot_wider(names_from = indicator)

#munge for 2019-2021  
df_hiv_ovc19_21 <- df_hiv_ovc %>% 
  filter(fiscal_year > 2018,
         standardizeddisaggregate == "ReportedStatus",
         statushiv == "Positive") %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  arrange(period) %>% 
  pivot_wider(names_from = indicator)

#munge for 2022
df_hiv_ovc_fy22 <-  df %>% 
  filter(indicator %in% c("OVC_HIVSTAT"), 
         standardizeddisaggregate  %in% c("Age/Sex/ReportedStatus"),
         funding_agency == "USAID",
         fiscal_year >= 2022) %>% 
  separate(categoryoptioncomboname, sep = ", ", into = c("age", "sex", "art_status")) %>% 
  #count(art_status)
  group_sum(type = art_status) %>% 
  filter(str_detect(type, "Positive")) %>%
  group_by(indicator, fiscal_year) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  reshape_msd() %>% 
  filter(period_type == "results") %>% 
  arrange(period) %>% 
  pivot_wider(names_from = indicator)

df_hiv_ovc <- bind_rows(df_hiv_ovc18, df_hiv_ovc19_21, df_hiv_ovc_fy22)

#latest stat
latest_stat_ovc <- df_hiv_ovc %>% 
  filter(period == max(period)) %>% 
  pull() %>% 
  clean_number()

# add color and opacity
df_hiv_ovc <- df_hiv_ovc %>% 
  mutate(bar_color = denim,
         bar_alpha = case_when(period == max(period) & str_detect(curr_pd, "Q4", negate = TRUE) ~ .6,
                               TRUE ~ 1)) 


#visual 2 - bar chart of # HIV+ OVC over time
v2 <- df_hiv_ovc %>% 
  filter(OVC_HIVSTAT > 0) %>% 
  ggplot(aes(period, OVC_HIVSTAT)) + 
  geom_blank(aes(y = 3.5e5)) + 
  geom_col(aes(fill = bar_color, alpha = bar_alpha), na.rm = TRUE) +
  scale_fill_identity() + 
  scale_alpha_identity() +
  scale_y_continuous(label = scales::comma,
                     expand = c(.005, .005)) +
  geom_text(aes(label = clean_number(OVC_HIVSTAT), vjust = -1, na.rm = TRUE,
                family = "Source Sans Pro")) +
  labs(x = NULL, y = NULL) +
  si_style_ygrid() +
  theme(legend.position = "none",
        strip.text.x = element_text(family = "Source Sans Pro SemiBold", size = 13),
        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(.5, "lines"),
        axis.text.y = element_blank()) + 
  coord_cartesian(expand = F, clip = "off")

# ART COVERAGE % -------------------------------------------------------------------

df_art <- df %>% 
  bind_rows(df_arch) %>% 
  filter((indicator == "OVC_SERV" & standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex") & trendscoarse == "<18") |
           (indicator == "OVC_HIVSTAT"),
          funding_agency == "USAID",
         fiscal_year >=2018)

#create shares 
df_art <- df_art %>%
  filter(indicator == "OVC_HIVSTAT",
         otherdisaggregate == "Receiving ART") %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  arrange(period) %>% 
  pivot_wider(names_from = indicator) %>%
  rename(ovc_art = OVC_HIVSTAT) %>% 
  left_join(df_hiv_ovc, by = c("period","period_type")) %>% 
  filter(ovc_art > 0 & OVC_HIVSTAT) %>% 
  mutate(art_share = ovc_art / OVC_HIVSTAT)

#grab the latest stat from max period
latest_stat_art <- df_art %>% 
  filter(period == max(period)) %>% 
  pull()

#grab the max period
latest_pd_art <- df_art %>% 
  slice_max(order_by = period, n = 1) %>% 
  pull(period)

#visual 3: OVC < 18 on ART
v3 <- df_art %>% 
  ggplot(aes(period, art_share, group = period_type)) +
  geom_blank(aes(y = 1.1 * art_share)) +
  geom_line(size = 1.5, color = scooter) +
  geom_point(shape = 21, color = scooter, fill = scooter, size = 12, stroke = 2) +
  geom_text(aes(label = percent(art_share, 1)),
            family = "Source Sans Pro", size = 12/.pt, color = "white") +
  expand_limits(y = .2) +
  labs(
       x = NULL, y = NULL) +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank()) + coord_cartesian(expand = F, clip = "off")


# COMBINE PLOTS -----------------------------------------------------------

v_right <- (v3/v2) +
  plot_layout(heights = c(2, 8))

v_right  +  plot_annotation(
  title = glue("This year, USAID has ensured that \\
               almost {percent(latest_stat_art, 1)} of the {latest_stat_ovc} OVC with HIV are on treatment") %>% toupper(),
 # subtitle = glue("{percent(latest_stat) of OVC <18 known their HIV status, \\ ")
  caption = glue("Source: {msd_source}
                      SI analytics: {paste(authors, collapse = '/')} | Ref ID: {ref_id}")) & 
  theme(plot.title = element_text(family = "Source Sans Pro",
                                  size = 14,
                                  face = "bold",
                                  color =  "#202020",
                                  hjust = 0),
        plot.caption = element_text(family = "Source Sans Pro",
                                    size = 9,
                                    color = "#909090",
                                    hjust = 1, vjust = 1))

si_save("Graphics/10_prev_semi-ovc-growth.svg")
si_save("Images/10_prev_semi-ovc-growth.png")

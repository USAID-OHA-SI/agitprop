# PROJECT: agitprop
# AUTHOR: K Srikanth | SI
# PURPOSE: global HIVST scale up results 
# LICENSE: MIT
# DATE: 2021-12-01
# NOTES: adapted from catch-22/Scripts/20211115_Global Planning Meeting/gpm_selftesting.R

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

# Set paths  
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"

#Authors
authors <- c("Karishma Srikanth", "Aaron Chafetz", "Tim Essam")

#clean number function
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

#source info
msd_source <- source_info()

#current period
curr_fy <- identifypd(df, "year")
curr_qtr <- identifypd(df, "quarter")
curr_pd <- source_info(return = "period")


# LOAD DATA ============================================================================  

msd <- si_path() %>% 
  return_latest("OU_IM_FY19-22") %>% 
  read_msd()

# MUNGE =================================================================================

#DF for HTS_TST / HTS_SELF Trends
df_hts <- msd %>% 
  filter(fundingagency == "USAID",
         fiscal_year <= 2021,
         indicator %in% c("HTS_TST", "HTS_SELF"),
         standardizeddisaggregate == "Total Numerator") %>%
  group_by(indicator, fiscal_year, fundingagency) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd() %>%
  mutate(fill_color = ifelse(indicator == "HTS_SELF", scooter, denim))

#DF for trends across just KP and Men
df_kp_men <- msd %>% 
  filter(indicator %in% c("HTS_SELF"),
         fiscal_year <= 2021,
         fundingagency == "USAID",
         (standardizeddisaggregate == "KeyPop/HIVSelfTest" | 
            (standardizeddisaggregate == "Age/Sex/HIVSelfTest" & sex == "Male"))) %>% 
  mutate(sex = ifelse(standardizeddisaggregate == "KeyPop/HIVSelfTest", "KeyPop", sex)) %>% 
  group_by(indicator, fiscal_year, sex, fundingagency) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd() %>% 
  mutate(fill_color = ifelse(sex == "KeyPop", "#5BB5D5", scooter_light))

# VIZ  ========================================================================
# Adjust sizing for each

#HTS_SELF Quarterly Trends Bar Chart
df_hts <- df_hts %>% 
  filter(indicator == "HTS_SELF") %>%
  mutate(value_label = round(value),
         value_label = value_label %>%  clean_number(),
         value_label = ifelse(period == curr_pd, value_label, NA))

viz_trend <- df_hts %>%  
  ggplot(aes(period, value, fill = fill_color)) +
  geom_col(position = "identity") +
  geom_text(aes(label = value_label), vjust = -1, na.rm = TRUE,
            family = "Source Sans Pro") +
  scale_y_continuous(labels = label_number_si()) +
  scale_fill_identity() +
  scale_color_identity() +
  si_style_ygrid() +
  labs(x = NULL, y = NULL,
       title = "Global") +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))

#HTS_SELF quarterly trends: MEN
df_men_viz <- df_kp_men %>% 
  filter(sex == "Male") %>%
  mutate(value_label = round(value),
         value_label = value_label %>%  clean_number(),
         value_label = ifelse(period == curr_pd, value_label, NA))

viz_men <- df_men_viz %>% 
  ggplot(aes(period, value, fill = fill_color)) +
  geom_col(position = "identity") +
  geom_text(aes(label = value_label), vjust = -0.5, na.rm = TRUE,
            family = "Source Sans Pro") +
  scale_y_continuous(labels = label_number_si()) +
  scale_fill_identity() +
  scale_color_identity() +
  si_style_ygrid() +
  labs(x = NULL, y = NULL, title = "Men") +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))


#HTS_SELF quarterly trends: KP
df_kp_viz <- df_kp_men %>% 
  filter(sex == "KeyPop") %>%
  mutate(value_label = round(value),
         value_label = value_label %>%  clean_number(),
         value_label = ifelse(period == curr_pd, value_label, NA))

viz_kp <- df_kp_viz %>% 
#  filter(sex == "KeyPop") %>%
  ggplot(aes(period, value, fill = fill_color)) +
  geom_col(position = "identity") +
  geom_text(aes(label = value_label), vjust = -0.5, na.rm = TRUE,
            family = "Source Sans Pro") +
  scale_y_continuous(labels = label_number_si()) +
  scale_fill_identity() +
  scale_color_identity() +
  si_style_ygrid() +
  labs(x = NULL, y = NULL, title = "Key Populations") +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))


#PATCHWORK --------------------------------------------------------------

v_right <- (viz_kp/viz_men)

viz_trend + v_right + plot_annotation(
  title = glue("USAID self testing has continued to scale up, especially for priority \\
               populations during COVID" %>%  toupper),
  caption = glue("Source: {msd_source}
                      SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) & 
  theme(plot.title = element_text(family = "Source Sans Pro",
                                  size = 14,
                                  face = "bold",
                                  color =  "#202020",
                                  hjust = 0),
        plot.caption = element_text(family = "Source Sans Pro",
                                    size = 9,
                                    color = "#909090",
                                    hjust = 1, vjust = 1))

si_save("Graphics/23_test_qtr.svg")


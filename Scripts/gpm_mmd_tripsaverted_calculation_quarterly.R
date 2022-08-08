# PROJECT:  catch-22
# AUTHOR:   K. Srikanth & N. Petrovic | USAID
# PURPOSE:  MMD Trips Averted Calculation
# LICENSE:  MIT
# DATE:     2021-11-05
# UPDATED:  2022-08-08 
# Note: MMD numbers adapted from "agitprop/Scripts/11_MMD.R"

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
library(waffle)

# IMPORT ------------------------------------------------------------------

df <- si_path() %>% 
  return_latest("OU_IM_FY20") %>% 
  read_msd()   


# MUNGE MMD ---------------------------------------------------------------

df %>% 
  filter(funding_agency == "USAID") %>%
  count(country) %>% 
  unique() %>% 
  view()
  
        
#keep just TX_CURR/MMD and reshape
df_mmd <- df %>% 
  filter(funding_agency == "USAID",
         indicator == "TX_CURR",
        # operatingunit != "South Africa",
         fiscal_year >= 2020,
         standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>% 
  mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                       TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
  )) %>%
  group_by(fiscal_year, country, indicator, otherdisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  filter(value > 0)

#create group for mmd3mo and mmd6mo 
df_mmd_agency <- df_mmd %>% 
  mutate(country = recode(country,
                              "Democratic Republic of the Congo" = "DRC",
                              "Dominican Republic" = "DR")) %>% 
  select(-period_type) %>% 
  pivot_wider(names_from = otherdisaggregate) %>% 
  rename(mmd3mo = `3 to 5 months`) %>%
  rename(mmd6mo = `6 or more months`) %>% 
  #rename(nommd= `Less than 3 months`) %>%
  #rowwise() %>%
  #mutate(total_sum=sum(mmd3mo,mmd6mo,nommd, na.rm=TRUE))
  #used to check whether the categories add up to the total -- doesnt seem to
  #be the case -- they are close but there is some missing data
  select(-`Less than 3 months`) %>%
  group_by(period) %>%
  summarise(across(c(mmd3mo, mmd6mo), sum, na.rm = TRUE)) %>%
  ungroup %>%
  #Calculation starting at COVID ramp-up FY20Q3
  filter(!(period %in% c("FY20Q1","FY20Q2")))

#CALCULATION ---------------------------------------------------------------

## Calculates trips averted using quarter by quarter estimates

trips_averted_mmd=0

for (i in 1:nrow(df_mmd_agency)){
  # For all those on 3 month MMD 2 trips per quarter are averted
  # For those on 6 month MMD either 2 or 3 trips per month are averted so 2.5 on average
  # Note that we are not modeling possibility that patient starts on MMD at the end of the quarter so this 
  # is an upper bound. However, 3 month mmd refers to "3-5mo" so there is an underestimate of 
  # trips averted in this case. 
  trips_averted_mmd<- trips_averted_mmd+2*df_mmd_agency$mmd3mo[i] + 2.5*df_mmd_agency$mmd6mo[i]
}

print(trips_averted_mmd)



#test dataframe for a waffle plot
  
# fy21_trips <- df_mmd %>% 
#   mutate(otherdisaggregate = recode(otherdisaggregate,
#                                     "o3mmd" = "mmd3",
#                                     "o6mmd" = "mmd6")) %>% 
#   group_by(period, otherdisaggregate) %>% 
#   summarise(across(c(tx_curr, tx_mmd), sum,na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   filter(period == "FY21Q3") %>% 
#   mutate(total_trip = sum(tx_mmd)*12) %>% 
#   pivot_wider(names_from = otherdisaggregate, values_from = tx_mmd) %>% 
#   mutate(mmd3_trips = mmd3*4,
#          mmd6_trips = mmd6*2) %>% 
#   select(-c(tx_curr, mmd3, mmd6)) %>% 
#   pivot_longer(total_trip:mmd6_trips, names_to = "type", values_to = "value") %>% 
#   mutate(share = (value /56296116)*100)
  

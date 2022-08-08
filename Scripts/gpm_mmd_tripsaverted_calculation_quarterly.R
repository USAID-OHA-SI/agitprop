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

#create group for o3mo and o6mo via reshaping for plotting
df_mmd <- df_mmd %>% 
  mutate(country = recode(country,
                              "Democratic Republic of the Congo" = "DRC",
                              "Dominican Republic" = "DR")) %>% 
  select(-period_type) %>% 
  pivot_wider(names_from = otherdisaggregate) %>% 
  rowwise() %>% 
  mutate(#unknown = total - sum(`Less than 3 months`, `3 to 5 months`, `6 or more months`, na.rm = TRUE),
    #unknown = ifelse(unknown < 0, 0, unknown),
    o3mmd = sum(`3 to 5 months`, `6 or more months`, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  rename(o6mmd = `6 or more months`) %>% 
  select(-`Less than 3 months`, -`3 to 5 months`) %>% 
  pivot_longer(-c(period, country, indicator, total), 
               names_to = "otherdisaggregate",
               values_to = "tx_mmd") %>% 
  rename(tx_curr = total) 

#aggregate up to USAID-wide level
df_mmd_agency <- df_mmd %>% 
  #mutate(otherdisaggregate = recode(otherdisaggregate,
  #                                  "o3mmd" = "MMD - 3 months or more",
  #                                  "o6mmd" = "MMD - 6 months or more")) %>% 
  group_by(period, otherdisaggregate) %>% 
  summarise(across(c(tx_curr, tx_mmd), sum,na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(share = tx_mmd / tx_curr)

#CALCULATION ----------------------------------------------------------------

df_mmd_agency_wide<-df_mmd_agency %>%
  select(-share) %>%
  pivot_wider(values_from=tx_mmd, names_from=otherdisaggregate) %>%
  filter(!(period %in% c("FY20Q1","FY20Q2")))

## This code duplicates the calculation from before using a quarter by quarter estimate
## Matches KS estimate
    ##Since the COVID-lockdowns (FY20Q3), we have helped to save beneficiaries up to 
    ##79 million clinic trips to pick up their treatment due to multi-month dispensing 
    ##(as of FY22Q2).

trips_averted_mmd=0

for (i in 1:nrow(df_mmd_agency_wide)){
  # For all those on 3 month MMD 2 trips per quarter are averted
  # For those on 6 month MMD either 2 or 3 trips per month are averted so 2.5 on average
  # Note that we are not modeling possibility that patient starts on MMD at the end of the quarter so this 
  # is an upper bound
  trips_averted_mmd<- trips_averted_mmd+2*df_mmd_agency_wide$o3mmd[i] + 2.5*df_mmd_agency_wide$o6mmd[i]
}

print(trips_averted_mmd)

## However, this may be the more accurate calculation since the previous is double counting 6 mo MMD -- 
## it is included in both 3mo + over & 6 mo. 

trips_averted_mmd=0
for (i in 1:nrow(df_mmd_agency_wide)){
  # Subtract the 6mo total from 3mo to avoid double-counting
  trips_averted_mmd<- trips_averted_mmd+2*(df_mmd_agency_wide$o3mmd[i]-df_mmd_agency_wide$o6mmd[i]) + 
                      2.5*df_mmd_agency_wide$o6mmd[i]
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
  

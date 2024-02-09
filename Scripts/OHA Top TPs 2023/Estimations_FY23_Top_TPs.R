#run join code prior to this 
df_arch1 <- si_path() %>% 
  return_latest("OU_IM_FY15") %>% 
  read_msd()

df_arch <- df_arch1 %>% 
  select(-c(qtr1:qtr4)) 

# MUNGE -----------------------------------------------------------------

#bind archived + current MSD and filter 
df_bind <- df_final %>%
  bind_rows(df_arch) %>% 
  dplyr::filter(funding_agency %in% "USAID")

TX_U15<-df_bind %>% 
  mutate(OU_group=case_when(
    operatingunit=="Nigeria" ~ "Nigeria",
    operatingunit=="Tanzania" ~ "Tanzania",
    TRUE ~ "All Other OUs"
  )) %>% 
  filter(indicator=="TX_CURR",
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus", "MostCompleteAgeDisagg"),
         funding_agency=="USAID",
         !fiscal_year=="2024") %>% 
  group_by(fiscal_year,OU_group,funding_agency,indicator,trendscoarse) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(trendscoarse,cumulative) %>% 
  mutate(total = coalesce(`<15`, 0) + coalesce(`15+`, 0) + coalesce(`Unknown Age`, 0),
         prct_u15=(`<15`/total)*100) %>% 
  arrange(OU_group) %>% 
  prinf()

ind<-distinct(df_bind,indicator,standardizeddisaggregate)

TX_PVLS_U15<-df_bind %>% 
  mutate(OU_group=case_when(
    operatingunit=="Nigeria" ~ "Nigeria",
    operatingunit=="Tanzania" ~ "Tanzania",
    TRUE ~ "All Other OUs"
  )) %>% 
  filter(indicator=="TX_PVLS",
         standardizeddisaggregate %in% c("Age Aggregated/Sex/Indication/HIVStatus", "Age/Sex/Indication/HIVStatus", "Age Aggregated/Sex"),
         funding_agency=="USAID",
         trendscoarse=="<15",
         !fiscal_year=="2024") %>% 
  group_by(fiscal_year,OU_group,funding_agency,indicator,numeratordenom,trendscoarse) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  spread(numeratordenom,cumulative) %>% 
  mutate(vls = (N/D)*100) %>% 
  arrange(OU_group) %>% 
  prinf()

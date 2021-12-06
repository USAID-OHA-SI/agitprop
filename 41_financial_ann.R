# PROJECT:  agiprop
# AUTHOR:   B.Kasdan | USAID
# PURPOSE:  Spending by Funding Type (remake of the Powers memo)
# LICENSE:  MIT
# DATE:     2021-05-14
# UPDATED:  2021-05-27

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(ICPIutilities)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(janitor)
  library(lubridate)
library(googlesheets4)
library(readxl)

#install treemaps
install.packages("treemapify")
library(treemapify)
devtools::install_github("adamdsmith/nrsmisc")
library(nrsmisc)



  source("Scripts/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Ben Kasdan", "Karishma Srikanth")
  

# IMPORT ------------------------------------------------------------------
  
id<- googlesheets4::as_sheets_id('11CRsy-77xtgxVWVhqUQEb7zxTThvPvuAVb0_WJLD734')
funding <- googlesheets4::read_sheet(id, "Machine Readable FY21Q4") 

#funding<-read_csv("Data/USAID-PEPFAR fund graph.csv") %>%
 # dplyr::select( - c(X2))
df1<-funding%>%
  pivot_longer(cols= `2004` : `2021`,
    names_to = "Fiscal Year",
    values_to= "Funding")%>%
  dplyr::mutate(`color` = `Source`)%>%
 
  mutate(color = ifelse(`Source` == "USAID: HQ", "#2057a7", 
                           ifelse(`Source`=="USAID: Countries","#e07653",
                                  ifelse(`Source`=="USAID: GF/UNAIDS","#1e87a5","Other"))))
df1<-df1%>%
  mutate(`Funding`=(`Funding`/1e9))
#Adjust FY labels
my_labels <- seq(2004, 2021, 1) %>% 
  substr(., 3, 4) %>% paste0("FY", .)
# Keep every 4th label but replace those in between with blanks
cust_labels <- nrsmisc::every_nth(my_labels, 4, inverse = T)
# Add this to your ggplot
scale_x_discrete(labels = cust_labels)

#Reshape dataframe to group by variables to show background budget
df2<- df1 %>% 
  distinct(`Fiscal Year`, `Source`,`Funding`) %>% 
  count(`Fiscal Year`, `Source`,`Funding`) %>% 
  group_by(`Fiscal Year`) %>% 
  mutate(total_n = sum(`Funding`)) %>% 
  ungroup()%>%
  dplyr::mutate(`color` = `Source`)%>%
  
  mutate(color = ifelse(`Source` == "USAID: HQ", "#2057a7", 
                        ifelse(`Source`=="USAID: Countries","#e07653",
                               ifelse(`Source`=="USAID: GF/UNAIDS","#1e87a5","Other"))))

v <- df2 %>% 
  ggplot(aes(`Fiscal Year`,`Funding`, n, fill=`color`))+
geom_col(aes(y = total_n), fill = "#8C8985", alpha = .2)+
  geom_col()+
 facet_wrap(~`Source`)+
   scale_fill_identity()+
  si_style_nolines()+
  theme(legend.position = "none")+
  scale_y_continuous(labels = unit_format(.1, unit = "B"))+
  scale_x_discrete(labels = cust_labels)+
  
 labs(
   title =  "USAID Funding for HIV Programming FY2004-2020 (USD $B)",
   caption = glue("Source: USAID FY21Q4 O&O Report for S/GAC 15 November 2021
                  SI analytics: {paste(authors, collapse = '/')}
              US Agency for International Development")) +
  
  si_style_nolines()+
  theme(legend.position = "none")
si_save("23_Powerfunding_trends.png")
si_save(path="~/GitHub/agitprop","23_Powerfunding_trends.svg")

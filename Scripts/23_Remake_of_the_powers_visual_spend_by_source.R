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
library(tidyverse)
library(glitr)
library(glamr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(ICPIutilities)
library(googlesheets4)
library(readxl)
library(here)
#install treemaps
install.packages("treemapify")
library(treemapify)
devtools::install_github("adamdsmith/nrsmisc")
library(nrsmisc)



  source("Scripts/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Ben Kasdan")
  

# IMPORT ------------------------------------------------------------------
  
  #Source: PEPFAR Spotlight (public)-I created a smaller data set from that for just FY20 ER
funding<-read_csv("Data/USAID-PEPFAR fund graph.csv") %>%
  dplyr::select( - c(X2))
df1<-funding%>%
  pivot_longer(cols= `2004` : `2020`,
    names_to = "Fiscal Year",
    values_to= "Funding")%>%
  dplyr::mutate(`color` = `Source`)%>%
 
  mutate(color = ifelse(`Source` == "USAID: HQ", "#2057a7", 
                           ifelse(`Source`=="USAID: Countries","#e07653",
                                  ifelse(`Source`=="USAID: GF/UNAIDS","#1e87a5","Other"))))
#Adjust FY labels
my_labels <- seq(2004, 2020, 1) %>% 
  substr(., 3, 4) %>% paste0("FY", .)
# Keep every 4th label but replace those in between with blanks
cust_labels <- nrsmisc::every_nth(my_labels, 4, inverse = T)
# Add this to your ggplot
scale_x_discrete(labels = cust_labels)




v <- df1 %>% 
  #distinct(`Source`,`Fiscal Year`) %>% 
  #count(`Fiscal Year`,`Funding`, `color`) %>% 
  ggplot(aes(`Fiscal Year`,`Funding`, n, fill=`color`)) +
  scale_fill_identity()+
  geom_col()

v +
  facet_wrap(~`Source`)+
  si_style_nolines()+
  theme(legend.position = "none")+
  scale_y_continuous(labels = unit_format(.1, unit = "B"))+
  scale_x_discrete(labels = cust_labels)+
 # theme(axis.text = element_blank(), 
  #      plot.title = element_markdown(family = "Source Sans Pro Regular",size = 14/.pt),face="bold",
   #     plot.caption=element_text(hjust = 0)) +
  
  # title = "IN FY20, USAID PEPFAR SPENT 65% OF THE BUDGET ON PREVENTION, SOCIO-ECONOMIC SUPPORT, AND TREATMENT",
  labs(
   title =  "USAID Funding for HIV Programming FY2004-2020 (USD $B)",
   caption = glue("Source: USAID Phoenix System Data as of May 2021
                  SI analytics: {paste(authors, collapse = '/')}
              US Agency for International Development")) +
  
  si_style_nolines()+
  theme(legend.position = "none")
si_save("23_Powerfunding_trends.png")
 

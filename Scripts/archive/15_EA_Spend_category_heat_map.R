# PROJECT:  agiprop
# AUTHOR:   B.Kasdan | USAID
# PURPOSE:  treatment scale up since PEPFAR start
# LICENSE:  MIT
# DATE:     2021-05-14
# UPDATED:  2021-05-24

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
#install treemaps
install.packages("treemapify")
library(treemapify)


  source("Scripts/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Ben Kasdan")
  

# IMPORT ------------------------------------------------------------------
  
  #Source: PEPFAR Spotlight (public)-I created a smaller data set from that for just FY20 ER
 df_EATree<-read_csv("Data/EA Tree map for expenditure.csv")%>%
  mutate(PA_ER = glue("{`Program Area`}\n ${`Mutated Dolloar`}M")) 

 

# MUNGE -------------------------------------------------------------------

  #source info


v1<-ggplot(df_EATree, aes(area = Expenditure, fill = Percentage,label=`PA_ER`)) +
  geom_treemap()+
  geom_treemap_text(family="Source Sans Pro", colour = "white", place = "centre",size=11)+
  scale_fill_si(palette = "denims",discrete = FALSE)+
  labs(x = NULL, y = NULL, fill = NULL,
      # title = "IN FY20, USAID PEPFAR SPENT 65% OF THE BUDGET ON PREVENTION, SOCIO-ECONOMIC SUPPORT, AND TREATMENT",
       subtitle = "FY20 Program Area Expenditure (Millions of USD)"
      # caption = glue("Excludes Commodities and M&O
       #  Source: Spotlight Expenditure Data FY20
        #                SI analytics: {paste(authors, collapse = '/')}
         #            US Agency for International Development")) +
  )+
       si_style_nolines()+
  theme(legend.position = "none")+

  si_save("15_EA_trends.png")
  
  si_save("Graphics/04_tx_trends.svg")
  
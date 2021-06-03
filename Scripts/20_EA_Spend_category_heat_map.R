# PROJECT:  agiprop
# AUTHOR:   B.Kasdan | USAID
# PURPOSE:  EA Program Area Heatmap
# LICENSE:  MIT
# DATE:     2021-05-14
# UPDATED:  2021-05-26

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
install.packages("ggforce")
library(treemapify)
library(ggforce)



  source("Scripts/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Ben Kasdan")
  

# IMPORT ------------------------------------------------------------------
  
  #Source: PEPFAR Spotlight (public)-I created a smaller data set from that for just FY20 ER
df_EATree<-read_csv("Data/EA Tree map for expenditure.csv")%>%
  mutate(PA_ER = glue("{`Program Area`}\n ${`Mutated Dolloar`}M")) %>%
  
  mutate(pct = percent(`Percentage`, 1),
         label_color = glue("{`Program Area`}\n ${`Mutated Dolloar`}M {`pct`}"),
         label_color = case_when(
           `Program Area` %in% c("Above Site Programs", "Testing") ~ grey90k,
           TRUE ~ "white")
  )

 

# MUNGE -------------------------------------------------------------------

  #source info


v1<-ggplot(df_EATree, aes(area = Expenditure, fill = Percentage, label= paste(`PA_ER`, " | ", pct), color = label_color)) +
  geom_treemap(color = "white", size = 1, alpha = 0.85)+
  geom_treemap_text(family = "Source Sans Pro", color = "#808080", place = "centre", min.size = 7 ) + 
  scale_fill_si(palette = "genoas",discrete = FALSE) +
  scale_color_identity()+
  labs(x = NULL, y = NULL, fill = NULL,
       # title = "IN FY20, USAID PEPFAR SPENT 65% OF THE BUDGET ON PREVENTION, SOCIO-ECONOMIC SUPPORT, AND TREATMENT",
       subtitle = "FY20 Direct Service Delivery Program Area Expenditure (USD $M)"
       # caption = glue("Excludes Commodities and M&O
       #  Source: Spotlight Expenditure Data FY20
       #                SI analytics: {paste(authors, collapse = '/')}
       #            US Agency for International Development")) +
  )+
  si_style_nolines()+
  theme(legend.position = "none")

  si_save("20_EA_trends.png")
  

  
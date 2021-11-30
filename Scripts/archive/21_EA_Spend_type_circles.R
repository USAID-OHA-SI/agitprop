# PROJECT:  agiprop
# AUTHOR:   B.Kasdan | USAID
# PURPOSE:  EA DSD Spend
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

#Source: PEPFAR Spotlight (public)-I created a smaller data set from that for just FY18-20 ER
df_IT<-read_csv("Data/Interaction Type.csv")%>%
  mutate(dsd_pct= 100*`Direct Service Delivery`/Total)%>%
  dplyr::mutate(`FY`=as.character(`FY`))




# MUNGE -------------------------------------------------------------------

#source info
v2<-df_IT %>% 
    
    ggplot(aes(FY, `Total`)) +
    geom_col(fill = "#e6e6e6") +
    geom_col(aes(y = `Direct Service Delivery`), fill = "#287c6f", na.rm = TRUE) +
    geom_errorbar(aes(x = FY, ymin = `Total`, ymax =`Total`),
                  color = "#808080") +
  geom_text(aes(label = paste0( round(`dsd_pct`,0),"%")), vjust = 1,
            size = 12/.pt)+
  si_style_xline() +
  geom_errorbar(aes(x = `FY`, ymin = Total, ymax =Total),
                color = "#808080") +
  geom_hline(yintercept = 0, color = "#808080") +
 # geom_hline(yintercept = seq(1.0e9, 2.0e9, 1.0e9), color = "white", linetype ="Dotted" ) +
  scale_y_continuous(labels = unit_format(.1, unit = "B", scale = 1e-9))+
  coord_cartesian(expand = F, clip = "off")+
  theme( 
        plot.title = element_markdown(family = "Source Sans Pro Regular",size = 14/.pt),
        plot.caption=element_text(hjust = 0)) + 
  labs(x = NULL, y = NULL,
             subtitle = "FY18-20 Direct Service Delivery Expenditure Trend (USD $B)"
       #caption = glue("Excludes Commodities and M&O
        # Source: Spotlight Expenditure Data FY18-20
         #               SI analytics: {paste(authors, collapse = '/')}
          #           US Agency for International Development")) +
  )+
  theme(legend.position = "none",
        panel.spacing = unit(0.5, "lines"),
        plot.caption=element_text(hjust = 1))+
  si_style_nolines()
si_save("21a_combined_trends.png")
    
    ###combine data from heat map and the table

 v_right<-v2| (plot_spacer()/v1/ plot_spacer())
 v_right + plot_annotation(
   title = str_wrap("USAID HAS FOCUSED ON DIRECT SERVICE DELIVERY FOR PREVENTION AND TREATMENT"),
   caption = glue("Excludes Commodities and M&O
         Source: Spotlight Expenditure Data FY18-20
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")
 ) & 
   theme(plot.title = element_text(family = "Source Sans Pro",
                                   size = 14,
                                   face = "bold",
                                   color =  "#202020",
                                   hjust = 0),
         plot.caption = element_text(family = "Source Sans Pro",
                                     size = 9,
                                     color = "#909090",
                                     hjust = 1, vjust = 1))
 si_save("21_combined_trends.png")
 si_save("21_combined_trends.svg")
 
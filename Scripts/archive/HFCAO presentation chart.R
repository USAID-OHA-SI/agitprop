# PURPOSE: Munge and Analysis of HFCAO data 
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-10-04
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(tidyverse)
    library(gophr)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(here)
library(janitor)
library(glue)
    
    
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  # Functions  
    authors <-  ("Benjamin Kasdan")
  

# LOAD DATA ============================================================================  

 df<- read.csv("~/Data/HFCAO data.csv")%>%
      clean_names()
    


# MUNGE ============================================================================
  
  df<-df%>%
    dplyr::rename("Operating Unit"=i_usaid_operating_unit,
                  "FY21 Budget"=fy21_budget_000s_ghp_usaid)
     df<-df%>%
       dplyr::mutate("FY21 Budget"=as.numeric("FY21 Budget"))
  
# VIZ ============================================================================

  df%>%
      # arrange(as("FY 21 Budget"))%>%
       gt()%>%
       fmt_currency( # add dolar signs
         columns = tidyselect::contains("FY21"),
         decimals = 0,
       currency = "USD")%>%
  tab_options(
    table.font.names = "Source Sans Pro"
  ) %>% 
  cols_width(
    everything() ~ px(200))%>%
  
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1.5),
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    ))%>%
       
       gt::tab_options(
         source_notes.font.size = 8,
         table.font.size = 14, 
         data_row.padding = gt::px(5),
         source_notes.padding = gt::px(1),) %>%
       
       tab_style(
         style = cell_text(weight = "bold"),
         locations = cells_body(
           columns = everything(), # not needed if coloring all columns
           rows = 11:13)
       )%>%
       
       tab_footnote(
         footnote = "Thousands $USD",
         locations = cells_column_labels(
           columns =c("FY21 Budget")))%>%
       tab_header(
         title = ("FY21 GHP USAD Funding"))%>% 
       gt::tab_source_note(
         source_note = gt::md("**Source**: FY2021 653(a) Facts Info NextGen" )) %>%
       gt::tab_source_note(
         source_note = gt::md(glue::glue("{authors} | US Agency for International Development"))
       )%>%
       gtsave("HFCAO_1.png")
     
  gt::tab_source_note(
    source_note = (glue::glue("")
     

      

# SPINDOWN ============================================================================


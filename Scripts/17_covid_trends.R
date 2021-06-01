# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  COVID trends
# LICENSE:  MIT
# DATE:     2021-06-01
# UPDATED: 

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
  library(lubridate)
  library(COVIDutilities)
  library(Wavelength)
  library(ISOcodes)
  library(jsonlite)
  library(zoo)

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam")

  #Stringency Index API url - start/end date 
    ox_start <- "2020-01-01"
    ox_end <- today()
    url_ox <- paste("https://covidtrackerapi.bsg.ox.ac.uk/api/v2/stringency/date-range",
                    ox_start, ox_end, sep = "/")
    rm(ox_end, ox_start)

  #quarter starts
    qtrs <- seq.Date(as.Date("2019-10-01"), today(), by = "3 months")

    
# IMPORT ------------------------------------------------------------------
  
  #Government Response (Oxford - https://covidtracker.bsg.ox.ac.uk/about-api)
    json <- url_ox %>%
      jsonlite::fromJSON(flatten = TRUE)
    
  #COVID cases (JHU)
    df_covid <- pull_jhu_covid()

# MUNGE OXFORD DATA -------------------------------------------------------
    
  #covert from json to dataframe
  df_stringency <- json %>%
    unlist() %>%
    enframe()
  
  #clean up table
  df_stringency <- df_stringency %>% 
    rowwise() %>%
    mutate(
      parts = length(unlist(str_split(name, "[.]"))),
      tbl = first(unlist(str_split(name, "[.]"))),
      tbl = gsub("\\d", "", tbl)
    ) %>%
    filter(parts == 4) %>%    # Keep the data, section with the longest parts
    separate(name,
             into = c("name", "date", "iso", "variable"),
             sep = "[.]") %>%                   # Separate column into multiple parts
    select(date:value) %>%               # Get rid of extra columns
    filter(date != value, iso != value) %>%     # Exclude repetition
    mutate(date = ymd(date), value = as.numeric(value)) %>% 
    spread(variable, value) %>% 
    select(-contains("legacy"))
  
  #add colors from FT - https://ig.ft.com/coronavirus-lockdowns/)
  df_stringency <- df_stringency %>% 
    mutate(bins = case_when(is.na(stringency)  ~ "NA",
                            stringency < 1     ~ "<1",
                            stringency < 25    ~ "1-24",
                            stringency < 50    ~ "25-49",
                            stringency < 75    ~ "50-74",
                            stringency < 85    ~ "75-84",
                            TRUE               ~ "85-100"),
           color = case_when(is.na(stringency) ~ "#D9CDC3",
                             stringency < 1    ~ "#D3E8F0",
                             stringency < 25   ~ "#FAE1AF",
                             stringency < 50   ~ "#FDAC7A",
                             stringency < 75   ~ "#F6736B",
                             stringency < 85   ~ "#DA3C6A",
                             TRUE              ~ "#A90773"
           ))
  
  #filter to PEPFAR countries
  df_stringency <- df_stringency %>% 
    filter(iso %in% iso_map$iso)
  
  #add country name
  df_stringency <- df_stringency %>% 
    left_join(iso_map) %>% 
    rename(countryname = operatingunit) %>% 
    select(-regional)
  
  #order colors
  df_stringency <- df_stringency %>% 
    mutate(bins = factor(bins, c("NA","<1", "1-24", "25-49", "50-74", "75-84", "85-100")),
           color = factor(color, c("#D9CDC3", "#D3E8F0","#FAE1AF", "#FDAC7A", "#F6736B", "#DA3C6A", "#A90773")))
  
  #order vars
  df_stringency <- df_stringency %>% 
    select(-c(confirmed, deaths, stringency_actual)) %>% 
    select(date, countryname, iso, everything())
  
  rm(json)
  
    

# MUNGE COVID DATA --------------------------------------------------------

  #add ISO codes
  df_covid <- ISO_3166_1 %>% 
    select(Name, iso = Alpha_3) %>%
    mutate(Name = recode(Name, 
                         "Congo, The Democratic Republic of the" = "Congo (Kinshasa)",
                         "Myanmar" = "Burma",
                         "C?te d'Ivoire" = "Cote d'Ivoire",
                         "Lao People's Democratic Republic" = "Laos",
                         "Tanzania, United Republic of" = "Tanzania",
                         "Viet Nam" = "Vietnam")) %>% 
    left_join(df_covid, ., by = c("countryname" = "Name")) %>% 
    mutate(countryname = recode(countryname, 
                                "Congo (Kinshasa)" = "Democratic Republic of the Congo"))
  
  #filter to just PEPFAR countries
  df_covid_pepfar <- df_covid %>% 
    filter(iso %in% iso_map$iso)
  
  #create a rolling average
  df_covid_pepfar <- df_covid_pepfar %>% 
    arrange(date) %>% 
    group_by(countryname) %>% 
    mutate(rollingavg_7day = rollmean(daily_cases, 7, fill = NA, align = c("right"))) %>% 
    ungroup()

# VIZ ---------------------------------------------------------------------

  df_viz <- df_covid_pepfar %>% 
    tidylog::left_join(df_stringency)
  
  df_viz <- df_viz %>% 
    group_by(countryname) %>% 
    mutate(max_val = max(daily_cases, na.rm = TRUE)) %>% 
    ungroup()
  
  df_viz %>% 
    filter(max_val > 1000,
           date >= "2020-03-01") %>% 
    ggplot(aes(date, daily_cases)) +
    annotate(geom = "rect",
             xmin = as.Date("2021-01-01"),
             xmax = as.Date("2021-04-01"),
             ymin = 0,
             ymax = Inf,
             color = trolley_grey_light, alpha = .1) +
    geom_col(fill = burnt_sienna, alpha = .8, na.rm = TRUE) +
    # geom_col(aes(y = -50, fill = bins), alpha = 1) +
    # geom_col(aes(y = -10), fill = "white") +
    geom_hline(aes(yintercept = 0), size = 0.5, color = grey20k) +
    geom_line(aes(y = rollingavg_7day), color = si_palettes$burnt_siennas[7], #size = 1,
              na.rm = TRUE) +
    # geom_vline(xintercept = qtrs, size = 0.5, color = grey20k) +
    facet_wrap(~fct_reorder(countryname, daily_cases, max, na.rm = TRUE, .desc = TRUE), scales = "free_y") + 
    scale_y_continuous(label = comma) +
    scale_x_date(date_labels = "%b %y",
                 breaks = c(as.Date("2020-03-01"), today())) +
    # scale_fill_manual(values = c("NA" = "#D9CDC3",
    #                              "<1" = "#D3E8F0",
    #                              "1-24" = "#FAE1AF",
    #                              "25-49" = "#FDAC7A",
    #                              "50-74" = "#F6736B",
    #                              "75-84" = "#DA3C6A",
    #                              "85-100" = "#A90773"),
    #                   na.value = "#D9CDC3") +
    labs(x = NULL, y = NULL, fill = "Stringency Index",
         title = "MANY PEPFAR COUNTRIES EXPERIENCED COVID PEAKS DURING FY21Q2, LIKELY IMPACTING \nMER RESULTS AND COLLECTION",
         subtitle = "Limited to countries that ever experienced more than 1,000 daily cases",
         caption = glue("Source: Source: JHU COVID-19 feed [{today()}]
                      SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_nolines() +
    theme(axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          strip.text = element_text(size = 8),
          plot.caption = element_text(size = 7),
          panel.spacing.x = unit(.5, "line"),
          panel.spacing.y = unit(.5, "line"))
  
  #+ stringecy index from Blavatnik School of Government at Oxford University 
  
  si_save("Images/17_covid_ctry_trends.png")
  si_save("Graphics/17_covid_ctry_trends.svg")
  
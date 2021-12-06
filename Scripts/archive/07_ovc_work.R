# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  OVC care
# LICENSE:  MIT
# DATE:     2021-05-20
# UPDATED:  2021-05-28

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
  library(packcircles)
  
  source("Scripts/99_utilities.R")


# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam")

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd() 
  
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()

# OVC_SERV OVERALL --------------------------------------------------------
  
  msd_source <- df %>% 
    identifypd() %>% 
    msd_period(period = .)
  
  df_kpi <- df %>% 
    filter(indicator == "OVC_SERV",
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID") %>% 
    filter(cumulative > 0) %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(fiscal_year == max(fiscal_year))
  
  
  df_viz <- df_kpi %>% 
    mutate(x = .5,
           y = x,
           x_label = .5,
           y_label = .75,
           x_sub = .72,
           y_sub = .3,
           ind_display = "Beneficiaries of Orphan and Vulnerable Children affected by HIV")
  
  v1 <- df_viz %>% 
    ggplot(aes(x, y)) +
    geom_text(aes(label = clean_number(cumulative, 2)),
              family = "Source Sans Pro Light", color = scooter,
              size = 60/.pt) +
    geom_text(aes(x_label, y_label, label = ind_display),
              family = "Source Sans Pro Light", color = trolley_grey, 
              size = 11/.pt) +
    geom_text(aes(x_sub, y_sub, label = df_kpi$fiscal_year),
              family = "Source Sans Pro Light", color = trolley_grey, 
              size = 11/.pt) +
    expand_limits(x = c(0, 1), y = c(0,1)) +
    facet_grid(~indicator) +
    labs(x = NULL, y = NULL) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_blank(),
          panel.background = element_rect(fill = "#e6e7e84f"),
          panel.border = element_rect(color = trolley_grey, fill = NA))  

# HIV STATUS BREAKDOWN ----------------------------------------------------

  df_hivstat <- df %>% 
    filter(indicator == "OVC_HIVSTAT",
           standardizeddisaggregate == "ReportedStatus",
           fundingagency == "USAID") %>%
    group_by(fiscal_year, indicator, otherdisaggregate, statushiv) %>% 
    summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(fiscal_year == max(fiscal_year)) %>% 
    arrange(desc(cumulative))
  
          
  df_hivstat <- df_hivstat %>% 
    mutate(status = case_when(statushiv == "Positive" ~ glue("{statushiv} {otherdisaggregate}") %>% as.character(),
                             otherdisaggregate == "Test Not Required" ~ otherdisaggregate,
                             TRUE ~ statushiv),
           share = cumulative/sum(cumulative)) %>% 
    mutate(status_lab = glue("{status}\n{clean_number(cumulative)}")) %>% 
    arrange(cumulative)
    
  
  df_coords <- circleProgressiveLayout(df_hivstat$cumulative) 
  
  df_viz_vals <- df_hivstat %>% 
    bind_cols(df_coords)
  
  df_viz_coords <- circleLayoutVertices(df_coords)
  
  v2 <- ggplot() +
    geom_polygon(data = df_viz_coords, aes(x, y, group = id, fill= as.factor(id))) +
    geom_text(data = df_viz_vals, aes(x, y, label = status_lab), 
              family = "Source Sans Pro",color = "white") +
    coord_equal() +
    scale_fill_manual(values = c(trolley_grey,
                                 burnt_sienna,
                                 trolley_grey_light,
                                 scooter,
                                 denim)) +
    labs(subtitle = "Virtually 100% of OVCs <18 who are HIV positive are on treatment") +
    si_style_void() +
    theme(legend.position = "none")
  

# TRENDS IN KNOWN STATUS PROXY --------------------------------------------

  df_knowstatus <- df %>% 
    bind_rows(df_arch) %>% 
    filter((indicator == "OVC_SERV" & standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "Age/Sex") & trendscoarse == "<18") |
           (indicator == "OVC_HIVSTAT"& standardizeddisaggregate == "ReportedStatus"),
           fundingagency == "USAID",
           fiscal_year >=2018) %>%
    mutate(otherdisaggregate = ifelse(is.na(otherdisaggregate), "NA", otherdisaggregate)) %>% 
    filter(otherdisaggregate != "No HIV Status") %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd() %>% 
    arrange(period) %>% 
    pivot_wider(names_from = indicator) %>% 
    mutate(knownstat = OVC_HIVSTAT/OVC_SERV)

  latest_stat <- df_knowstatus %>% 
    filter(period == max(period)) %>% 
    pull()
  
  latest_pd <- df_knowstatus %>% 
    slice_max(order_by = period, n = 1) %>% 
    pull(period)
  
  v3 <- df_knowstatus %>% 
    ggplot(aes(period, knownstat, group = period_type)) +
    geom_blank(aes(y = 1.1 * knownstat)) +
    geom_line(size = 1.5, color = scooter) +
    geom_point(shape = 21, color = scooter, fill = "white", size = 10, stroke = 2) +
    geom_text(aes(label = percent(knownstat, 1)),
              family = "Source Sans Pro", size = 10/.pt) +
    expand_limits(y = .2) +
    labs(subtitle = glue("As of {latest_pd}, {percent(latest_stat, 1)} of OVC <18 now know their status"),
         x = NULL, y = NULL) +
    si_style_nolines() +
    theme(axis.text.y = element_blank())
  



# COMBINE PLOTS -----------------------------------------------------------

  v_right <- (v2/v3) +
    plot_layout(heights = c(2, 1))

  v1 + v_right + plot_annotation(
      title = str_wrap("USAID WORKS TO SUPPORT PEPFAR'S MISSION 
                       TO MITIGATE THE IMPACT OF HIV ON CHILDREN, 
                       ADOLESCENTS AND THEIR FAMILIES AS WELL AS PREVENTING 
                       HIV-RELATED MORTALITY", 95),
      caption = glue("Source: {msd_source}
                      SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) & 
    theme(plot.title = element_text(family = "Source Sans Pro",
                                    size = 14,
                                    face = "bold",
                                    color =  "#202020",
                                    hjust = 0),
          plot.caption = element_text(family = "Source Sans Pro",
                                      size = 9,
                                      color = "#909090",
                                      hjust = 1, vjust = 1))
  
  si_save("Images/07_ovc_usaid.png")
  si_save("Graphics/07_ovc_usaid.svg")
  
  
  
  
  
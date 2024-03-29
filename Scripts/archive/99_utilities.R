#' Identify MSD Period and Type
#'
#' requires having si_path setup from glamr
#' 
#' @param type Type of MSD (OU_IM, PSNU, PSNU_IM, NAT_SUBNAT)
#' @param period can provide period from gophr::identifypd(), or it will run if NULL
#'
#' @return FY00Q0t MSD
#' @export

msd_period <- function(type = "OU_IM", period = NULL){
  file <- glamr::si_path() %>% 
    glamr::return_latest(type)
    
  
   v <- file %>% 
     basename() %>% 
     stringr::str_extract("(?<=_v).{1}")
   
   v <- ifelse(v == "1", "i", "c")
   
   if(is.null(period)){
     period <- file %>% 
      gophr::read_msd() %>% 
      gophr::identifypd()
     
   }
     
   pd <- glue::glue("{period}{v} MSD")
   
   return(pd)
   
}

#' Clean Number
#'
#' @param x number vector
#' @param digits number of digits to round to (default = 0)
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'  clean_number(1500, 1)
#'  df %>% mutate(value_label = clean_number(value)) }
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

#' Dynamic format for scaling in plots
#'
#' @param x number
#'
#' @return
#' @export

dynamic_format <- function(x){
  
  max_x <- max(x, na.rm = TRUE)
  
  if (max_x >= 1e9) {
    return(scales::number(x, scale = 1e-9, suffix = "B"))
  } else if (max_x >= 1e6) {
    return(scales::number(x, scale = 1e-6, suffix = "M"))
  } else if (max_x >= 1e3) {
    return(scales::number(x, scale = 1e-3, suffix = "K"))
  } else if (max_x <3) {
    return(scales::percent(x))
  } else {
    return(f(x))
  }
}


#' Dynamic scaling format for scaling x axis in plots 
#'
#' @param ... 
#'
#' @return
#' @export
#'
scale_x_dynamic <- function(...) {
  ggplot2::scale_x_continuous(..., labels = dynamic_format)
}

#' Dynamic scaling format for scaling y axis in plots 
#'
#' @param ... 
#'
#' @return
#' @export
#'
scale_y_dynamic <- function(...) {
  ggplot2::scale_y_continuous(..., labels = dynamic_format)
}

#' Calculate Achievement
#'
#' @param df MSD data frame
#' @param curr_qtr current quarter, using gophr::identifypd(df, "quarter"), default = 4
#' @param add_color add OHA achievement colors, default = TRUE
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' qtr <- gophr::identifypd(df, "quarter")
#' df <- calc_achv(df, qtr)}
calc_achv <- function(df, curr_qtr = 4, add_color = TRUE){
  df_achv <- df %>% 
    mutate(achievement = cumulative/targets,
           qtr_goal = ifelse(indicator == "TX_CURR", 1, 1*(curr_qtr/4)),
           achv_label = case_when(is.na(achievement) ~ NA_character_,
                                  achievement <= qtr_goal-.25 ~ glue("<{100*(qtr_goal-.25)}%") %>% as.character,
                                  achievement <= qtr_goal-.1 ~ glue("{100*(qtr_goal-.25)}-{100*(qtr_goal-.11)}%") %>% as.character,
                                  achievement <= qtr_goal+.1 ~ glue("{100*(qtr_goal-.1)}-{100*(qtr_goal+.1)}%") %>% as.character,
                                  TRUE ~ glue("+{100*(qtr_goal+.1)}%") %>% as.character),
           achv_color = case_when(is.na(achievement) ~ NA_character_,
                                  achievement <= qtr_goal-.25 ~ old_rose_light,
                                  achievement <= qtr_goal-.1 ~ burnt_sienna_light,
                                  achievement <= qtr_goal+.1 ~ "#5BB5D5",
                                  TRUE ~ trolley_grey_light)) %>% 
    select(-qtr_goal)
  
  return(df_achv)
}

#' Query DATIM
#'
#' @param ctry countryname (for printing)
#' @param ctry_uid uid for the country
#' @param org_lvl org hierarchy level, eg site level for Saturn = 6, recommend glamr::get_outable()
#' @param type API type - HTS (HTS_TST), TX (TX_CURR), LAB (LAB_PTCQI)
#' @param username DATIM username, recommend using glamr::load_secrets()
#' @param password DATIM password, recommend using glamr::load_secrets()
#' @param baseurl default = https://final.datim.org/
#'
#' @return API pull data frame from DATIM
#' @export

query_datim <- function(ctry, ctry_uid, org_lvl, type, username, password, baseurl = "https://final.datim.org/"){
  
  print(paste("running ", ctry, " ... ", type, " ... ", Sys.time()))
  
  core_url <-
    paste0(baseurl,"api/29/analytics?",
           "dimension=ou:LEVEL-", org_lvl, ";", ctry_uid, "&", #level and ou
           "dimension=bw8KHXzxd9i:NLV6dy7BE2O&" #Funding Agency -> USAID
    )
  
  if(type == "TX"){
    type_url <- 
      paste0("dimension=pe:",datim_cy,"Oct&", #period
             "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets/Results - Results W8imnja2Owd,Jh0jDM5yQ2E
             "dimension=LxhLO68FcXm:MvszPTQrUhy&", #technical area, TX_CURR
             "filter=RUkVjD3BsS1:PE5QVF0w4xj&" #Top Level  - Numerator
      )
  } else if (type == "HTS"){
    type_url <- 
      paste0("dimension=pe:",datim_cy,"Oct&", #period
             "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets/Results - Results W8imnja2Owd,Jh0jDM5yQ2E
             "dimension=LxhLO68FcXm:f5IPTM7mieH;wdoUps1qb3V;BTIqHnjeG7l;rI3JlpiuwEK;CUblPgOMGaT&", #technical area,  HTS_TST, HTS_INDEX, PMTCT_STAT, TB_STAT, VMMC_CIRC
             "filter=ra9ZqrTtSQn&", #HTS Modality (USE ONLY for FY20,21 Results/FY21,22 Targets)
             "filter=bDWsPYyXgWP:awSDzziN3Dn;EvyNJHbQ7ZE;mSBg9AZx1lV;viYXyEy7wKi&") #HIV Test Status (Specific)) - Pos/Neg + New Pos/Neg
    
  } else if(type == "LAB"){
    type_url <- 
      paste0("dimension=pe:", datim_cy, "Q3&", #period
             "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets/Results - Results W8imnja2Owd,Jh0jDM5yQ2E
             "dimension=LxhLO68FcXm:scxfIjoA6nt&", #technical area, LAB_PTCQI
             "dimension=HWPJnUTMjEq:T7Z0TtiWqyu;SG4w1HBS23B;MC7Q6BN0Xw9&" #Disaggregation Type = Lab/CQI, Lab/PT, Lab/TestVolume
      )
    
  } 
  
  end_url <- "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true"
  
  full_url <- paste0(core_url, type_url, end_url)
  
  df <- grabr::datim_process_query(full_url, username, password)
  
  return(df)
}

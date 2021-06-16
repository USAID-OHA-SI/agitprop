# PROJECT:  agitprop
# AUTHOR:   B.Kagniniwa | USAID
# PURPOSE:  Test DATIM/API Pull from Analytical end point
# LICENSE:  MIT
# DATE:     2021-05-11
# NOTE:     Move this to Wavelength / glamr


library(tidyverse)
library(glamr)
library(janitor)
library(glue)
library(gisr)


# Base url
baseurl <- "https://final.datim.org/"

# Resources
url_res <- "https://www.datim.org/api/resources"

# Dimensions
url_dims <- "https://final.datim.org/api/dimensions"

#' @title Get dimensions
#' 
#' get_dimensions()
#' 
get_dimensions <- function(var = NULL, 
                           url = "https://final.datim.org/api/dimensions") {
  
  dims <- url %>%
    httr::GET(httr::authenticate(glamr::datim_user(), glamr::datim_pwd())) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten = T) %>%
    purrr::pluck("dimensions") %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    rename(dimension = display_name)
  
  if (!is.null(var) && var %in% c("id", "dimension")) {
    dims <- dims %>% pull(!!sym(var))
  }
  
  return(dims)
}

#' @title Get dimension id
#' 
#' identify_dimension("OU Level")
#' 
identify_dimension <- function(name) {
  
  dim_id <- NULL
  
  df_dims <- get_dimensions()
  
  if (name %in% df_dims$dimension) {
    dim_id = df_dims %>%
      filter(dimension == name) %>%
      pull(id)
  }
  
  return(dim_id)
}


#' @title Dimensions Items
#' 
#' get_dim_items("Funding Agency")
#' 
get_dim_items <- function(dimension,
                          var = NULL,
                          url = NULL){
  
  url_dims <- ifelse(!is.null(url), url,
                     "https://final.datim.org/api/dimensions")
  
  # Get dimension id
  dim_id <- identify_dimension(dimension)
  
  # Update url
  url_dims <- glue::glue("{url_dims}/{dim_id}/items")
  
  print(glue::glue("Items url: {url_dims}"))
  
  # Get items
  items <- url_dims %>%
    httr::GET(httr::authenticate(glamr::datim_user(), glamr::datim_pwd())) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten = T) %>%
    purrr::pluck("items") %>%
    tibble::as_tibble()
  
  if (is.null(items) || nrow(items) == 0) {
    message(glue::glue("Dimension: {dimension}, reponse null / empty"))
    stop("Invalid dimension name")
  }
  
  items <- items %>%
    janitor::clean_names() %>%
    rename(item = display_name)
  
  if (!is.null(var) && var %in% c("id", "item")) {
    items <- items %>% pull(!!sym(var))
  }
  
  return(items)
}


#' @title Get dimension item id
#' 
#' identify_item(dimension = "Funding Agency", name = "USAID")
#' identify_item(dimension = "Targets / Results", name = "MER Results")
#' 
identify_item <- function(dimension, name, url = NULL) {
  
  item_id <- NULL
  
  # Get dimension's items
  items <- get_dim_items(dimension)
  
  if (is.null(items) || nrow(items) == 0) {
    message(glue::glue("Dimension: {dimension}, response is null or empty"))
    stop("Unable to identify items under this dimension")
  }
  
  # validate item
  if ( name %in% items$item) {
    item_id <- items %>%
      filter(item == {{name}}) %>%
      pull(id)
    
  } else {
    message(glue::glue("Dimension: {dimension}, Item: {name}, response = {nrow(items)}"))
    stop("unable to locate item name")
  }
  
  return(item_id)
}


#' @title Query targets from DATIM
#'
query_datim <- function(ou,
                        org_lvl = "facility", 
                        type = "TX",
                        datim_cy = Wavelength::curr_fy - 1,
                        username, password,
                        baseurl = "https://final.datim.org/"){
  
  print(paste("running ", ou, " ... ", type, " ... ", Sys.time()))
  
  # Org & Funding Agency
  ou_uid <- gisr::get_ouuid(ou)
  org_lvl <- gisr::get_ouorglevel(ou, org_type = "facility")
  
  dim_fa <- identify_dimension(name = "Funding Agency")
  dim_fa_usaid <- identify_item(dimension = "Funding Agency", name = "USAID")
  
  core_url <-
    paste0(baseurl,"api/29/analytics?",
           "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
           "dimension=", dim_fa, ":", dim_fa_usaid, "&" #Funding Agency -> USAID
    )
  
  # Technical Areas
  dim_tr <- identify_dimension(name = "Targets / Results")
  dim_tr_r <- identify_item(dimension = "Targets / Results", name = "MER Results")
  
  dim_ta <- identify_dimension(name = "Technical Area")
  
  if (type == "TX") {
    
    dim_ta_tx_curr <- identify_item(dimension = "Technical Area", name = "TX_CURR")
    
    dim_tl <- identify_dimension(name = "Top Level")
    dim_tl_num <- identify_item(dimension = "Top Level", name = "Top Level Numerator")
    
    type_url <-
      paste0("dimension=pe:",datim_cy,"Oct&", #period
             "dimension=", dim_tr, ":", dim_tr_r, "&", #IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets/Results - Results W8imnja2Owd,Jh0jDM5yQ2E
             "dimension=", dim_ta, ":", dim_ta_tx_curr, "&", #LxhLO68FcXm:MvszPTQrUhy&", #technical area, TX_CURR
             "filter=", dim_tl, ":", dim_tl_num, "&" #RUkVjD3BsS1:PE5QVF0w4xj&" #Top Level  - Numerator
      )
    
  } else if (type == "HTS") {
    
    dim_ta_hts <- c("HTS_TST",
                    "HTS_INDEX",
                    "PMTCT_STAT",
                    "TB_STAT",
                    "VMMC_CIRC") %>%
      map(~identify_item(dimension = "Technical Area", name = .x)) %>%
      unlist() %>%
      paste(collapse = ';')
    
    dim_hts_mod <- identify_dimension(name = "HTS Modality (USE ONLY for FY20,21 Results/FY21,22 Targets)")
    
    dim_hts_tst <- identify_dimension(name = "HIV Test Status (Specific)")
    
    dim_hts_tst_r <- c("HIV Positive (Specific)",
                       "HIV Negative (Specific)",
                       "Newly Tested Positives (Specific)",
                       "New Negatives (Specific)") %>%
      map(~identify_item(dimension = "HIV Test Status (Specific)", name = .x)) %>%
      unlist() %>%
      paste(collapse = ';')
    
    type_url <-
      paste0("dimension=pe:",datim_cy,"Oct&", #period
             "dimension=", dim_tr, ":", dim_tr_r, "&", #IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets/Results - Results W8imnja2Owd,Jh0jDM5yQ2E
             "dimension=", dim_ta, ":", dim_ta_hts, "&", #LxhLO68FcXm:f5IPTM7mieH;wdoUps1qb3V;BTIqHnjeG7l;rI3JlpiuwEK;CUblPgOMGaT&", #technical area,  HTS_TST, HTS_INDEX, PMTCT_STAT, TB_STAT, VMMC_CIRC
             "filter=", dim_hts_mod, "&", #ra9ZqrTtSQn&", #HTS Modality (USE ONLY for FY20,21 Results/FY21,22 Targets)
             "filter=", dim_hts_tst, ":", dim_hts_tst_r, "&" #bDWsPYyXgWP:awSDzziN3Dn;EvyNJHbQ7ZE;mSBg9AZx1lV;viYXyEy7wKi&" #HIV Test Status (Specific)) - Pos/Neg + New Pos/Neg
      )
    
  } else if (type == "LAB") {
    
    dim_ta_lab <- identify_item(dimension = "Technical Area", name = "LAB_PTCQI")
    dim_dty <- identify_dimension(name = "Disaggregation Type")
    dim_dty_labs <- c("Lab/CQI", "Lab/PT", "Lab/TestVolume",
                      "POCT/CQI", "POCT/PT", "POCT/TestVolume") %>%
      map(~identify_item(dimension = "Disaggregation Type", name = .x)) %>%
      unlist() %>%
      paste(collapse = ';')
    
    type_url <-
      paste0("dimension=pe:", datim_cy, "Q3&", #period
             "dimension=", dim_tr, ":", dim_tr_r, "&", #IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets/Results - Results W8imnja2Owd,Jh0jDM5yQ2E
             "dimension=", dim_ta, ":", dim_ta_lab, "&", #LxhLO68FcXm:scxfIjoA6nt&", #technical area, LAB_PTCQI
             "dimension=", dim_dty, ":", dim_dty_labs, "&" #HWPJnUTMjEq:T7Z0TtiWqyu;SG4w1HBS23B;MC7Q6BN0Xw9;hfaBo0nrQok;oBbMk5GjX4a;PJEPs8sHAk5&" #Disaggregation Type = Lab/CQI, Lab/PT, Lab/TestVolume, POCT/CQI, POCT/PT, POCT/TestVolume"
      )
    
  }
  
  end_url <- "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true"
  
  full_url <- paste0(core_url, type_url, end_url)
  
  print(glue::glue("URL: {full_url}"))
  
  df <- Wavelength::get_datim_targets(full_url, username, password)
  
  return(df)
}

# Test
query_datim(ou = "Nigeria", 
             org_lvl = "facility", 
             type = "TX", 
             username = glamr::datim_user(),
             password = glamr::datim_pwd())

query_datim(ou = "Nigeria", 
             org_lvl = "facility", 
             type = "HTS", 
             username = glamr::datim_user(),
             password = glamr::datim_pwd())

query_datim(ou = "Nigeria", 
            org_lvl = "facility", 
            type = "LAB", 
            username = glamr::datim_user(),
            password = glamr::datim_pwd())
#' Identify MSD Period and Type
#'
#' requires having si_path setup from glamr
#' 
#' @param type Type of MSD (OU_IM, PSNU, PSNU_IM, NAT_SUBNAT)
#' @param period can provide period from ICPIutilities::identifypd(), or it will run if NULL
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
      ICPIutilities::read_msd() %>% 
      ICPIutilities::identifypd()
     
   }
     
   pd <- glue::glue("{period}{v} MSD")
   
   return(pd)
   
}
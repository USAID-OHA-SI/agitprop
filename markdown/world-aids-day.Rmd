---
title: "Data Call for World AIDS Day"
author: "Karishma Srikanth & Nada Petrovic"
date: "11/22/2022"
output:
  word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(janitor)
  library(lubridate)
  library(googlesheets4)
```

# Data Call for World AIDS Day

Audience: The public, Agency stakeholders, HIV/AIDS stakeholders, development 

Purpose: The purpose of this document is to create one place for the data points we will be sharing on World AIDS Day. On World AIDS Day we will be sharing an Agency notice, an Agency-wide social media toolkit and we will be updating our OHA webpage on World AIDS Day [last year’s version here](https://www.usaid.gov/world-aids-day). These points will be repeated throughout these deliverables and language may vary slightly from what is included below. The USG World AIDS Day theme is “Putting Ourselves to the Test: Achieving Equity to End HIV.”  The UNAIDS theme is “Equalize. UNAIDS is urging each of us to address the inequalities which are holding back progress in ending AIDS.”

Given the World AIDS Day Theme of equity, we are planning to focus on achievements related to making prevention methods more accessible (like PrEP) and priority populations in addition to our overall achievements. We are not interested in exact numbers for this exercise; over X, nearly X, etc. are fine. 

Our plan is to share the Agency social media toolkit by Nov 23. 

The clearance timeline is:

* Division chiefs to clear by COB Tuesday Nov 15th
* Front Office to clear by COB Friday November 18th
* Nov 21st - SIEI & comms to address any outstanding issues/comments from clearance process
* Nov 22nd - SIEI to plug in numbers for Q4
* Nov 23rd - Social Media tool kit is released


```{r cars, include=FALSE, echo=FALSE}
#Current MSD
msd_path <- si_path() %>% 
  return_latest("MER_Structured_Datasets_OU_IM_FY20-23_20221216_v2_1")

get_metadata(msd_path)

df <- read_msd(msd_path)

# load source functions
#source("Scripts/archive/99_utilities.R")

#LOCAL PARTNER
#Read in the google sheet hyperfile with local partner
sheet_id <- "1MQviknJkJDttGdNEJeNaYPKmHCw6BqPuJ0C5cslV5IE"

df_partner <- read_sheet(sheet_id, sheet = "MechID-PartnerType", range = "A:B") %>% 
  clean_names() %>% 
  rename(mech_code = mechanism_id) %>% 
  mutate(mech_code = as.character(mech_code),
         partner_type = case_when(partner_type == "Regional" ~ "Local",
                                  partner_type == "TBD Local" ~ "Local", TRUE ~ partner_type))


#clean number function
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)} billion"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)} million"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)} thousand"),
                   TRUE ~ glue("{x}"))
}

load_secrets()

```

## Topline Achievements

```{r tx_curr, echo=FALSE, include = FALSE}

 # Derived from agitprop/Scripts/26_treat_qtr_historic-txcurr-trends.R
  
  #create a PEPFAR duplicate and aggregate up to country/global level
  df_tx <- df %>%
    bind_rows(df %>% mutate(funding_agency = "PEPFAR")) %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, funding_agency, indicator) %>% 
    summarise(value = sum(cumulative, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(funding_agency %in% c("PEPFAR", "USAID"),
           value != 0) %>% 
    arrange(indicator, funding_agency, fiscal_year) %>% 
    mutate(source = "MSD")

 df_tx <- df_tx %>% 
    filter(fiscal_year == metadata$curr_fy,
           indicator == "TX_CURR") %>%
    select(fiscal_year, indicator, funding_agency, value) %>% 
    pivot_wider(names_from = funding_agency) %>% 
    mutate(usaid_share = USAID/PEPFAR)  
  
  tx_val <<- df_tx %>% 
    mutate(USAID_Lab = USAID %>% clean_number()) %>%
    pull(USAID_Lab) 


```
```{r VLS, echo=FALSE, include = FALSE}

# derived from agitprop/Scripts/42_trend_ou_viral_load_coverage.R

df_vls_global <- df %>% 
    clean_indicator() %>% 
    clean_agency() %>% 
    clean_countries(colname = "country") %>% 
   filter(
      fiscal_year != metadata$curr_fy + 1,
      funding_agency == "USAID",
      indicator %in% c("TX_CURR", "TX_PVLS_D", "TX_PVLS"),
      standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")
    ) %>% 
    group_by(fiscal_year, funding_agency, country, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    group_by(funding_agency, country) %>% 
    mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period)) %>% 
    relocate(TX_CURR_LAG2, .before = TX_CURR) %>% 
    filter(!is.na(TX_PVLS_D), TX_PVLS_D > 0) %>% 
    group_by(period, funding_agency) %>% 
    summarise(across(starts_with("TX"), sum, na.rm = T), .groups = "drop") %>% 
    mutate(VLC = TX_PVLS_D / TX_CURR_LAG2,
           VLS = TX_PVLS/TX_PVLS_D)

  df_tx_ou <- df %>% 
    clean_indicator() %>% 
    clean_agency() %>% 
    clean_countries(colname = "country") %>% 
    filter(
      fiscal_year != metadata$curr_fy + 1,
      funding_agency == "USAID",
      indicator %in% c("TX_CURR", "TX_PVLS_D", "TX_PVLS"),
      standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")
    ) %>% 
    group_by(fiscal_year, funding_agency, country, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    dplyr::select(-period_type) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    group_by(funding_agency, country) %>% 
    mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period),
           VLC = TX_PVLS_D / TX_CURR_LAG2,
           VLS = TX_PVLS/TX_PVLS_D) %>% 
    ungroup() %>% 
    relocate(TX_CURR_LAG2, .before = TX_CURR) %>% 
    filter(!is.na(TX_PVLS_D), TX_PVLS_D > 0)

glbl_vls <<- df_vls_global %>%
  filter(period == metadata$curr_pd) %>%
  mutate(VLS_pct = percent(VLS)) %>%
  pull(VLS_pct)

meet_90_vls <<- df_tx_ou %>%
  filter(period == metadata$curr_pd, VLS >= .9) %>%
  distinct(country) %>% nrow()

meet_95_vls <<- df_tx_ou %>%
  filter(period == metadata$curr_pd, VLS >= .95) %>%
  distinct(country) %>% nrow()

```


```{r HTS, echo=FALSE, include = FALSE}

  # derived from agitprop/Scripts/19_test_qtr-hts-trends.R

#HTS_TST dataset
munge_hts <- function(indic) {
  df_hts <- df %>% 
    filter(indicator == indic,
           standardizeddisaggregate == "Total Numerator") 
  
  df_hts <- suppressMessages(
    df_hts %>%
    bind_rows(df_hts %>% mutate(funding_agency = "PEPFAR")) %>%
    group_by(fiscal_year, funding_agency) %>%
    summarise(across(c(cumulative), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(funding_agency %in% c("PEPFAR", "USAID")) %>%
    mutate(source = "MSD") %>%
    rename(value = cumulative) %>%
    pivot_wider(names_from = funding_agency, values_from = value) %>%
    group_by(fiscal_year) %>%
    mutate(share = USAID / PEPFAR)  %>%
    pivot_longer(cols = PEPFAR:USAID, names_to = "funding_agency"))
  
  hts_val <- df_hts %>% 
    filter(fiscal_year == metadata$curr_fy,
           funding_agency == "USAID") %>% 
    select(fiscal_year, value, share) %>% 
    mutate(share = percent(round(share, 2)),
           value = round(value, 2),
           value_clean = value %>%  clean_number()) %>%
    pull(value_clean)
  
  
  return(hts_val)
}

hts_val <- munge_hts("HTS_TST")
hts_pos_val <- munge_hts("HTS_TST_POS")
```

```{r site counts, echo=FALSE, include = FALSE}

  # still figuring out API call for this TP - just run this script on your own

 # source("Scripts/xx_hss_ann_usaid-site-count.R")

```

```{r LP, echo=FALSE, include = FALSE}

#catch-22/Scripts/20211102_Local Partner Meeting/20211022_LP_TX_trends.R

#result_type = value or share
grab_lp_results <- function(indic, result_type) {
  
  df_munge <- suppressMessages(df %>% 
    # bind_rows(df_arch) %>%
    left_join(df_partner, by = c("mech_code")) %>%
    filter(funding_agency == "USAID",
           indicator == indic,
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(funding_agency, fiscal_year, indicator, partner_type) %>% 
    summarise(across(cumulative, sum, na.rm = TRUE)) %>%
    ungroup() %>% 
    filter(partner_type != "TBD") %>%
    #filter(fiscal_year != 2022) %>%
    pivot_wider(names_from = partner_type, values_from = cumulative) %>%
    group_by(fiscal_year) %>%
    mutate(Total = International + Local,
           share = Local / Total)  %>%
    pivot_longer(cols = International:Total, names_to = "partner_type"))
  
  title_info_lp <- df_munge %>% 
    filter(partner_type == "Local", fiscal_year == metadata$curr_fy) %>% 
    select(fiscal_year, indicator, value, share) %>% 
    mutate(
      value = value %>%  clean_number(0), #change to 1 if you want 1 decimal accuracy
      share = percent(round(share, 2))) %>% 
    pull(result_type)
  
  return(title_info_lp)
}

tx_lp_val <- grab_lp_results("TX_CURR", "value")
tx_lp_share <- grab_lp_results("TX_CURR", "share")
hts_lp_val <- grab_lp_results("HTS_TST", "value")


```

Results through **`r metadata$curr_pd`**

1. Through PEPFAR, the U.S. government has saved more than 21 million lives, prevented millions of HIV infections, and helped 20 countries with HIV burden achieve epidemic control of HIV or reach UNAIDS treatment targets. Country leadership on sustaining the HIV response will require bold political, programmatic, and financial leadership.

2. Currently, USAID provides over **`r tx_val`** clients with life-saving HIV-treatment. **`r glbl_vls`** of these beneficiaries who received a viral load test are virally suppressed, meaning that they can live longer, healthier lives and the virus can no longer be transmitted.
 
3. Over the last year, USAID has provided HIV testing services to over **`r hts_val`** people. 

4. Over the last year, USAID helped over **`r hts_pos_val`** people learn of their HIV positive status. 

5. Over the last year, USAID and its partners supported **`r meet_90_vls`** countries to reach over 90 percent viral load suppression among people living with HIV, and of those, **`r meet_95_vls`** countries reached over 95 percent viral load suppression.

6. USAID investments strengthen host country systems that drive responsive, resilient, and enduring health care through more than **10,000** HIV testing sites, over **9,000** HIV treatment sites, and over **3,000** sites that provide lab-based or point-of-care testing making critical services accessible in over 50 supported PEPFAR countries.

7. The partnerships between USAID and local organizations and government are critical for achieving HIV epidemic control. In 2022, **$878 million** was implemented through PEPFAR local partners, up from **$451 million** in 2018. In 2022, local partners helped nearly **`r hts_lp_val`** people learn their HIV status and provided over **`r tx_lp_val`** people with life-saving HIV medication, representing **`r tx_lp_share`** of all USAID-supported people living with HIV.


## Achievements related to Key Populations 

```{r kp prep, echo=FALSE, include = FALSE}


pull_kp <- function(indic) {
kp_val <- df %>%
    filter(funding_agency == "USAID",
           str_detect(standardizeddisaggregate, "KeyPop(?!\\/Status)"),
           indicator %in% c("PrEP_NEW", "TX_CURR", "KP_PREV"),
           fiscal_year == metadata$curr_fy) %>%
    count(indicator, wt = cumulative) %>%
  mutate(n = clean_number(n, 1)) %>% 
    pivot_wider(names_from = indicator, values_from = "n") %>% 
  pull(indic)

return(kp_val)
}

kp_prev_val <- pull_kp("KP_PREV")
kp_tx_val <- pull_kp("TX_CURR")
kp_prep_val <- pull_kp("PrEP_NEW")

kp_tx_ou <- df %>%
    filter(funding_agency == "USAID",
           str_detect(standardizeddisaggregate, "KeyPop(?!\\/Status)"),
           indicator %in% c("TX_CURR"),
           fiscal_year == metadata$curr_fy) %>% 
      group_by(fiscal_year, funding_agency, country, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  filter(period == metadata$curr_pd) %>% 
  distinct(country) %>% 
  nrow() 


#AGYW prep
prep_agyw_val <- df %>%
  filter(funding_agency == "USAID",
         standardizeddisaggregate %in% c("Age/Sex"),
         indicator %in% c("PrEP_NEW"),
         fiscal_year == metadata$curr_fy,
         sex == "Female",
         age_2019 %in% c("15-19", "20-24")
         ) %>%
  count(indicator, wt = cumulative) %>%
  mutate(n = clean_number(n))

```

```{r ovc, echo=FALSE, include = FALSE}
ovc_serv_val <- df %>% 
    filter(indicator %in% c("OVC_SERV"),
           standardizeddisaggregate %in% c("Total Numerator"),
         #  trendscoarse == "<18",
           fiscal_year == metadata$curr_fy,
           funding_agency == "USAID") %>% 
    group_by(fiscal_year, funding_agency) %>% 
    summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
    ungroup() %>% 
    mutate(cumulative = clean_number(cumulative, 2)) %>% 
  pull(cumulative)
```

```{r gbv, echo=FALSE, include = FALSE}

# agitprop/Scripts/archive/13_gend_gbv_BAN_map.R

# for now, assume this is GEND_GBV total numerator - check with Allison S. tomorrow

gbv_val <- df %>% 
  filter(indicator == "GEND_GBV",
         standardizeddisaggregate == "Total Numerator",
         funding_agency == "USAID",
         fiscal_year == metadata$curr_fy) %>% 
    group_by(fiscal_year, funding_agency) %>% 
    summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
        ungroup() %>% 
    mutate(cumulative = clean_number(cumulative)) %>% 
  pull(cumulative)

#local partner - GBV
gbv_lp_pct <- grab_lp_results("GEND_GBV", "share")



```

```{r vmmc, echo=FALSE, include = FALSE}
vmmc_val <- df %>% 
  filter(indicator == "VMMC_CIRC",
         standardizeddisaggregate == "Total Numerator",
         funding_agency == "USAID",
         fiscal_year == metadata$curr_fy) %>% 
    group_by(fiscal_year, funding_agency) %>% 
    summarise(across(starts_with("cumulative"), sum, na.rm = T), .groups = "drop") %>% 
        ungroup() %>% 
    mutate(cumulative = clean_number(cumulative)) %>% 
  pull(cumulative)
```

8. USAID initiated almost **`r kp_prep_val`** members of key populations (KP) and **`r prep_agyw_val`** adolescent girls and young women (AGYW) on Pre-Exposure Prophylaxis (PrEP) during the last year, expanding effective HIV prevention to key and priority populations. Together KP and AGYW constitute *more than half* of all USAID PrEP initiations, and KP and AGYW PrEP initiations grew quarter over quarter throughout the entire year.

9. This year alone, more than **`r kp_tx_val`** members of key populations in **`r kp_tx_ou`** countries are receiving life-saving HIV medication through USAID support.

10. This year USAID and our partners supported almost **`r kp_prev_val`** members of key populations, including sex workers, transgender people, men who have sex with men and people who inject drugs, with HIV prevention services and supportive interventions, which improve their lives and the lives of their children.

11. In the last year, USAID supported over **`r ovc_serv_val`** orphans and vulnerable children (OVC) and their families affected by HIV.  Services provided to OVC families included, among others, household economic strengthening, education support, parenting skills building, facilitating access to maternal and child healthcare, and HIV specific testing, care, and treatment. 

12. Over the last year, USAID contributed to supporting over *2.3 million* adolescent girls and young women aged 10-24 years through DREAMS programming with combination HIV prevention services. Tailored services include HIV and violence prevention training, education support to complete secondary school, financial literacy and links to employment, PrEP & family planning services.
 
13. USAID's DREAMS programming contributed to providing more than *295,000 adolescent girls* and young women aged 15-24 years with economic strengthening interventions, which empower participants with technical skills and networks to become entrepreneurs or be linked to wage employment.
 
14. USAID provides integrated gender-based violence clinical services in *23* countries, including post-violence care services for almost **`r gbv_val`** survivors, with local partners delivering the majority of care (**`r gbv_lp_pct`**)

15. USAID voluntary medical male circumcision programs have circumcised almost **`r vmmc_val`** clients this year

## Broader PrEP Achievements

```{r prep, echo=FALSE, include = FALSE}
#bind archived + current MSD and filter for PrEP
df_prep <- df %>%
 # bind_rows(df_arch) %>% 
  filter(funding_agency == "USAID",
         indicator == "PrEP_NEW",
         standardizeddisaggregate == "Total Numerator") 

#count number of countries with PrEP
prep_cntry_cnt <- df_prep %>% 
  filter(cumulative != 0,
         fiscal_year == metadata$curr_fy) %>% 
  distinct(fiscal_year, country) %>% 
  count(fiscal_year, name = "n_countries") %>% 
  pull(n_countries)

prep_new_val <- df_prep %>% 
distinct(fiscal_year, country, cumulative) %>% 
  count(name = "n_countries", wt = cumulative) %>% 
  clean_number(2)

lp_prep_val <- grab_lp_results("PrEP_NEW", "value")
lp_prep_share <- grab_lp_results("PrEP_NEW", "share")
```

16. In the last two years, USAID has initiated over **`r prep_new_val`** people across **`r prep_cntry_cnt`** countries onto pre-exposure prophylaxis (PrEP) for HIV prevention.

17. USAID Local Partners initiated almost **`r lp_prep_val`** individuals onto PrEP in the last year, representing **`r lp_prep_share`** of all USAID PrEP initiations. 





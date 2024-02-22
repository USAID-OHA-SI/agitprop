# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  
# REF ID:   498b023a 
# LICENSE:  MIT
# DATE:     2024-02-22
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(cascade)
library(gagglr)
library(tidyverse)

# IMPORT  -------------------------------------------------------------------

# Setup local paths where msds live
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata, pattern = "OU_IM_FY21")

# load msd to be used
df <- read_psd(file_path)

# PREP -----------------------------------------------------------------------

#filter to OUs you want
df <- df %>% 
  filter(operatingunit %ni% c("Asia Region","Western Hemisphere Region", "Dominican Republic",
                              "Haiti", "Ukraine", "Vietnam", "Nigeria", "Tanzania"))

# populate all the metadata needed for cascade
gophr::get_metadata(file_path)

# Check cascades avaible for creation
plot_name

df_usaid <- df %>% filter(funding_agency == "USAID")

# Return a cascade data frame (number corresponds to position in char list)
# 1 = Standard cascade
return_cascade(df, 1) %>% View()

# Plot the cascade
# You will be promted to enter a cascade number
return_cascade_plot(df_usaid, export = F)

#batch return these to a folder - you will need to create those folders first
batch_cascade_plot(df_usaid, imgpath = "Images/Cascade/AFRICA/USAID", imgtype = ".svg")
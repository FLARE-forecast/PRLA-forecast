
if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, VIM, naniar, missMDA, Amelia, 
               mice, FactoMineR, broom, aws.s3)

remotes::install_github("cboettig/neonstore", force = T)
remotes::install_github("eco4cast/EFIstandards", force = T)
remotes::install_github("FLARE-forecast/flare", force = T)
remotes::install_github("FLARE-forecast/noaaGEFSpoint", force = T)

# Set up the sites to exctract data
siteID = "PRLA"
siteID_neon = c("PRLA","DCFS")
ECtower = "DCFS"

# Set up the directories and databases
lake_directory <- getwd()
local_directory <- file.path(getwd(), "data", "NOAA_data")
neon_database <- file.path(getwd(), "data", "neonstore")
noaa_data_location <- file.path(getwd(),"data","NOAA_data","noaa","NOAAGEFS_1hr",siteID)
forecast_location <- file.path(getwd(), "glm")

# Specify the products to download
products = c("DP1.00098.001", "DP1.00002.001", "DP1.00023.001", "DP1.00006.001", "DP1.00001.001", "DP1.00004.001")
buoy_products = c("DP1.20264.001","DP1.20252.001","DP1.20254.001")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### DONLOAD THE NEWEST NOAA DATA ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(file.path(lake_directory, "data_download/NOAA_downloads.R"))

date = seq(from = Sys.Date()-10, to = Sys.Date()-1, by = "days") # --> downloads the previous 10 days
cycle = c("00")

for(i in 1:length(date)){
  for(g in 1:length(cycle)){
      download_noaa_files_s3(siteID = siteID,
                             date = date[i], 
                             cycle = cycle[g], 
                             local_directory <- local_directory)
  }
}

# Confirm downloads
files_noaa <- list.files(noaa_data_location)
print(files_noaa)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### DOANLOAD THE NEWEST NEON DATA ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(file.path(lake_directory, "data_download/NEON_downloads.R"))

subDir <- "neonstore"
if (file.exists(file.path(getwd(),"data",subDir))){
  Sys.setenv("NEONSTORE_DB" = neon_database)
  Sys.setenv("NEONSTORE_HOME" = neon_database)
  neonstore::neon_dir()
} else {
  dir.create(file.path(getwd(),"data",subDir))
  Sys.setenv("NEONSTORE_HOME" = neon_database)
  Sys.setenv("NEONSTORE_DB" = neon_database)
  neonstore::neon_dir()
}

download_neon_files(siteID_neon = siteID_neon,
                    siteID = siteID,
                    ECtower = ECtower,
                    products = products,
                    buoy_products = buoy_products)

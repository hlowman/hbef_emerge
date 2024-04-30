### LAI Data Exploration
### April 30, 2024
### Heili Lowman

#### README ####

# The following script will perform some initial data exploration
# and visualization of the HBEF canopy data.

# I will be using the MODIS download protocol outlined by Phil
# Savoy at https://psavoy.github.io/StreamLight/articles/2%20Download%20and%20process%20MODIS%20LAI.html

# Note, MODIS has a 500m spatial resolution, which means some
# of the watersheds may have overlapping pixels, but I am still
# going to request each of them individually.

# I've chosen the SW corners of the bounding boxes for the south
# facing slopes and the NE corner of the bounding box for the
# north facing slope (W9) to better approximate LAI on the correct
# side of the ridgelines. Bounding boxes were used in the Q
# dataset at https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-hbr.1.17

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(StreamLight)

# Load data
sites <- read_csv("data_raw/ws_coords.csv")

#### Prep data for AppEARS ####

# Make a table for the MODIS request 
request_sites <- sites[, c("Site_ID", "Lat", "Lon")] 

# Export sites as a .csv for the AppEEARS request  
write.table(
  request_sites, 
  paste0("data_working/HB_sites.csv"), 
  sep = ",", 
  row.names = FALSE,
  quote = FALSE, 
  col.names = FALSE
)

# Request submitted to AppEARS.

#### Process data from AppEARS ####

# First, unpack the .zip file created.
MOD_unpack <- StreamLight::AppEEARS_unpack_QC(
  zip_file = "HubbardBrook.zip", 
  zip_dir = "data_raw", 
  request_sites[, "Site_ID"])

# Then, process and plot the data using the "gu" fit 
# (i.e.,)
MOD_processed <- StreamLight::AppEEARS_proc(
  unpacked_LAI = MOD_unpack,  
  fit_method = "Gu", 
  plot = TRUE)

#### Calculate dates when median is reached in spring/fall ####

# End of script

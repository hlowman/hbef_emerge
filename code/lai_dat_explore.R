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

# First, examine the LAI data.
lai_data <- read_csv("data_raw/HB_LAI_AppEEARS/Hubbard-Brook-LAI-MCD15A3H-061-results.csv")

# Then, plot the data
# Cannot use StreamLightUtils package unfortunately
# due to discontinuation of rgdal, so doing it manually
(lai_plot <- ggplot(lai_data, aes(x = Date, y = MCD15A3H_061_Lai_500m)) +
    geom_point() +
    labs(x = "Date", y = "LAI") +
    theme_bw() +
    facet_wrap(.~ID))

# Export data.
saveRDS(lai_data, "data_working/hb_lai_043024.rds")

#### Calculate dates when median is reached in spring/fall ####

# Need to add columns to estimate percentage of maximum,
# and then mark these dates in spring/fall.
lai_data <- lai_data %>%
  mutate(year = year(Date)) %>%
  group_by(ID, year) %>%
  mutate(max_LAI = max(MCD15A3H_061_Lai_500m)) %>%
  mutate(perc_LAI = MCD15A3H_061_Lai_500m/max_LAI) %>%
  ungroup()

# Next, need to figure out Phil's smoothing technique to
# calculate median on the way up and down since these values
# bounce around some.

# End of script

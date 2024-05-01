### LAI Data Exploration
### April 30, 2024
### Heili Lowman

#### README ####

# The following script will perform some initial data exploration
# and visualization of the HBEF canopy data.

# It will also create the interpolated LAI dataset to be used to
# determined the 50% leaf in and leaf out dates for each calendar year.

# I will be using the MODIS download protocol outlined by Phil at:
# https://psavoy.github.io/StreamLight/articles/2%20Download%20and%20process%20MODIS%20LAI.html

# Note, MODIS has a 500m spatial resolution, which means some
# of the watersheds may have overlapping pixels, but I am still
# going to request each of them individually.

# I've chosen the SW corners of the bounding boxes for the south
# facing slopes and the NE corner of the bounding box for the
# north facing slope (W9) to better approximate LAI on the correct
# side of the ridgelines. Bounding boxes were used in the Q data at:
# https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-hbr.1.17

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(lubridate)
library(StreamLight)
library(phenofit)

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

# Need to figure out Phil's smoothing technique to
# calculate median on the way up and down since these values
# bounce around some.

#### Interpolating daily LAI ####

# I'll be following the approximate workflow from StreamLightUtils::LAI_proc_phenofit
# but working through manually since the package and some dependencies (rgdal) are
# no longer supported by this version of R.

# First, import the raw LAI data.
lai_data <- readRDS("data_working/hb_lai_043024.rds")

# And subset to a single site for testing function below.
w1_lai <- lai_data %>%
  filter(ID == "W1")

# Also need to create a list of all sites over which to
# loop this function.
lai_list <- split(lai_data, f = lai_data$ID)

# Next, working through his custom LAI_proc_phenofit function here: 
# https://github.com/psavoy/StreamLightUtils/blob/master/R/LAI_proc_phenofit.R

LAI_proc_phenofit <- function(Site, fit_method){

  # From here, copying, notating, and revising Phil's code.
  ts <- w1_lai
  
  # Check date column format
  #str(ts)
  
  # Calculate weights from 5-level QC score
  #ts$w <- QC_weights(SCF_QC = ts[, "MCD15A3H_061_FparLai_QC_SCF_QC"])  # these are the weights assigned depending on data quality  

  # Ok, Phil's way wasn't working for some reason so I'm converting this to tidyverse format
  ts <- ts %>%
  mutate(w = case_when(MCD15A3H_061_FparLai_QC_SCF_QC %in% c("0b000", "0b001") ~ 1, #wmax
                       MCD15A3H_061_FparLai_QC_SCF_QC %in% c("0b010", "0b011") ~ 0.5, #wmid
                       MCD15A3H_061_FparLai_QC_SCF_QC %in% c("0b100") ~ 0.2, #wmin
                       TRUE ~ NA)) #Yay!
  
  # Calculate the temporal resolution of the dataset
  date_diff <- diff(ts$Date, lag = 1)
  uniqv <- unique(date_diff)
  t_res <- uniqv[which.max(tabulate(match(date_diff, uniqv)))] # 4 days
  
  # Skipping padding the timeseries since we have multiple years and 
  # have padding prior to 2018 anyway
  #ts_padded <- pad_ts(ts = ts, t_res = t_res)
  
  # Also skipping setting fill values as NA since highest value is 7.
  # Note, for right now this only catches the 255 fill value  
  # since that is the only one that has cropped up. Could adjust to 249-255 if needed. #PS 2022
  #ts_padded[ts_padded[, "Lai"] == 255, "Lai"] <- NA
  
  #-------------------------------------------------  
  # Perform the LAI smoothing & curve fitting
  #-------------------------------------------------
  # Get # of scenes per year
  scene_count <- 365 / t_res  
  
  # Check the input LAI data
  # This function checks "input data, interpolate NA values in y, 
  # remove spike values, and set weights for NA in y and w."
  # ECOSYSTEM OF CODE NEEDS TO BE UPDATED SO THE SOUTH PARAMETER VARIES BASED ON SITE LOCATION
  lai_input <- phenofit::check_input(t = ts$Date, # date
    y = ts$MCD15A3H_061_Lai_500m, # veg time series
    w = ts$w, # weights of y
    nptperyear = scene_count, # number of images per year
    south = FALSE, # which hemisphere
    maxgap = scene_count / 4, # deals with missingness
    alpha = 0.02, # quantile prob of replacement for missing value
    wmin = 0.2)    
  
  # Divide the growing season
  # This function defines the breaks or seasons prior to fitting and
  # roughly smooths the curve using a weighted Whittaker smoother.
  # MAY WANT TO CHECK ON LAMBDA VALUE IN THE FUTURE
  breaks <- phenofit::season_mov(lai_input, 
    options = list(
      FUN = "smooth_wWHIT", 
      wFUN = "wTSM",
      maxExtendMonth = 12,
      r_min = 0.1, 
      IsPlot = FALSE, 
      IsPlot.OnlyBad = FALSE, 
      print = FALSE,
      iters = 4,
      lambda = 1600)) # much larger than in vignettes but leaving since Phil used this value

  # Check how the test dataset looks
  #plot_season(lai_input, breaks)
  
  # Curve fitting
  # This function roughly fits the curve using an asymmetric Gaussian function.
  lai_fit <- phenofit::curvefits(lai_input, 
    brks = breaks,
    options = list(
      methods = c(fit_method), # "Gu" preferred approach per Phil's vignette
      wFUN = wTSM, 
      nextend = 2, 
      maxExtendMonth = 3, 
      minExtendMonth = 1, 
      minPercValid = 0.2, 
      print = FALSE, 
      verbose = FALSE, 
      iters = 4, 
      use.rough = FALSE,
      use.y0 = TRUE))    
  
  # Check the curve fitting parameters
  l_param <- get_param(lai_fit)
  d_fit <- get_fitting(lai_fit)
  
  # Get GOF information
  d_gof <- get_GOF(lai_fit)
  
  # Visualization (performed for test run)
  #g <- phenofit::plot_curvefits(d_fit, breaks)
  #grid::grid.newpage(); grid::grid.draw(g)# plot to check the curve fitting      
  
  # Bind together the curve fitting data
  # Note, this is still at the MODIS 4 day timestep
  fit_bound <- setNames(data.frame(
    d_fit[d_fit$meth == fit_method, ]$t, 
    d_fit[d_fit$meth == fit_method, ]$ziter2), 
    c("date", "Lai_fit"))
  
  #-------------------------------------------------  
  #Generating continuous series of LAI
  #------------------------------------------------- 
  # Generate a complete set of dates between the first and last record
  full_dates <- setNames(data.frame(seq.Date(from = min(ts$Date), 
                                             to = max(ts$Date), by = "day")), "Date")
  
  full_dates$Year <- as.numeric(format(full_dates$Date, "%Y"))
  full_dates$DOY <- as.numeric(format(full_dates$Date, "%j"))
  
  # Interpolate to daily values
  interpolated <- approx(fit_bound$date, fit_bound$Lai_fit, xout = full_dates$Date)
  # Huh, for some reason this doesn't populate the first two months but that's
  # not a huge deal for my purposes.
  
  interpolated_df <- setNames(data.frame(interpolated$x, interpolated$y), 
                              c("Date", "LAI_proc"))
  
  # Merge all of the data together
  dfs <- list(full_dates, ts[, c("Date", "MCD15A3H_061_Lai_500m")], 
              interpolated_df)
  final_merged <- plyr::join_all(dfs, by = "Date")

  return(final_merged)
  
  } #End LAI_proc_phenofit function - PHEW

# Now, for the moment of truth, apply this function to the full
# list of sites created above.
# Takes about 1 minute to run.
lai_processed <- lapply(lai_list, 
                        function(x) LAI_proc_phenofit(x, fit_method = "Gu"))

# Check to see if things populated.
View(lai_processed$W6) # YASSSS

plot(lai_processed$W6$Date, lai_processed$W6$LAI_proc)

# Export LAI data for future use.
saveRDS(lai_processed, "data_working/hb_lai_processed_050124.rds")

# End of script.

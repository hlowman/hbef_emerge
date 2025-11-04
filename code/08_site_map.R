### Site Map
### November 4, 2025
### Heili Lowman

#### README ####

# The following script will create a site map of the WS at HB.

#### Setup ####

# Load necessary packages.
library(here)
library(tidyverse)
library(patchwork)
library(ggmap)
library(ggrepel)
library(sf)
library(ggspatial)
library(tigris)
library(tidyterra)
library(elevatr)
library(nhdplusTools)
library(mapview)

# Load data.
# Downloaded shapefile from:
# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-hbr.90.4
hb_boundary <- st_read("data_raw/hbef_boundary/hbef_boundary.shp")
# Downloaded shapefile from:
# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-hbr.94.3
ws_boundary <- st_read("data_raw/hbef_wsheds/hbef_wsheds.shp")
# Downloaded shapefile from:
# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-hbr.95.3
streamlines <- st_read("data_raw/hbef_hydro/hbef_hydro.shp")
# Downloaded shapefile from:
# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-hbr.107.3
weirs <- st_read("data_raw/hbef_weirs/hbef_weirs.shp")

#### Plot ####

# Download state shapefiles
all_states_sf <- states(resolution = '500k', cb = TRUE, year = 2000)
nh_sf <- all_states_sf %>% 
  filter(NAME == "New Hampshire")

# Download elevation data
nh_elevation <- get_elev_raster(nh_sf, z = 12, clip = "bbox")

# Calculate hillshade
hillshade <- rast(terrain(nh_elevation, v = "hillshade", 
                     angle = 45, direction = 315))

# Crop to the state shapefile
nh_cropped <- crop(hillshade, nh_sf)
nh_trimmed <- mask(nh_cropped, nh_sf)

# Plot the state of NH.
(nh_fig <- ggplot() +
  geom_spatraster(data = nh_trimmed, aes(fill = slope)) +
  scale_fill_hypso_c(palette = "c3t1") +
  geom_sf(data = nh_sf, fill = NA, color = "black") +
  geom_sf(data = hb_boundary, fill = NA, color = "red",
          linewidth = 1) +
  theme_bw() +
  theme(legend.position = "none"))

# Trim down to only watersheds of interest.
# Had to do individually, otherwise was giving me issues.
ws1 <- ws_boundary %>%
  filter(WS == "WS1")
ws2 <- ws_boundary %>%
  filter(WS == "WS2")
ws3 <- ws_boundary %>%
  filter(WS == "WS3")
ws4 <- ws_boundary %>%
  filter(WS == "WS4")
ws5 <- ws_boundary %>%
  filter(WS == "WS5")
ws6 <- ws_boundary %>%
  filter(WS == "WS6")
ws9 <- ws_boundary %>%
  filter(WS == "WS9")

# Filter for weirs of interest
weirs_trim <- weirs %>%
  filter(WEIR_NUM %in% c(1,2,3,4,5,6,9))

# And set to proper coordinate reference system
streamlines_crs <- st_set_crs(streamlines, 26919)

# The c3t1 palette from above
hypso.colors2(20, palette = "c3t1", alpha = 1, rev = FALSE)
"#89B994" "#D5E2C4" "#F4F3C8" "#FFF8C7" "#FFF0C5" "#FFEDBE" "#FFE9B7" "#FFE6B1" "#FFE2AB"
[10] "#FFDFA5" "#FFDB9F" "#FFD899" "#FFD594" "#FFD390" "#FFD390" "#FFD390" "#FFD390" "#FFD390"
[19] "#FFD390" "#FFD390"

# Basic plot to be sure it's working
(hb_fig <- ggplot() +
  geom_sf(data = hb_boundary, fill = "#D5E2C4", color = "black") +
  geom_sf(data = streamlines_crs, color = "cornflowerblue") +
  geom_sf(data = ws1, fill = NA, color = "black") +
  geom_sf(data = ws2, fill = NA, color = "black") +
  geom_sf(data = ws3, fill = NA, color = "black") +
  geom_sf(data = ws4, fill = NA, color = "black") +
  geom_sf(data = ws5, fill = NA, color = "black") +
  geom_sf(data = ws6, fill = NA, color = "black") +
  geom_sf(data = ws9, fill = NA, color = "black") +
  geom_sf(data = weirs_trim, color = "black") +
  theme_bw())

# Join figures together.
(full_map <- nh_fig + hb_fig +
    plot_annotation(tag_levels = "A"))

# Export figure.
ggsave(plot = full_map,
       filename = "figures/HB_valley_map_110425.jpg",
       width = 20,
       height = 10,
       units = "cm")

# End of script.

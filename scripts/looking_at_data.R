
# Load some libraries
library(sf) # vector geospatial data
library(leaflet) # your own google map!
library(dplyr)

seagrass <- readRDS("data/joined_seagrass_cover.Rds")
loss_gain <- readRDS("data/loss_gain_map_2022.Rds")
max_extent <- readRDS("data/max_seagras_extent_sf.Rds")
boat_ramps <- st_read("data/Maine_Boat_Launches_GeoLibrary/Maine_Boat_Launches_GeoLibrary.shp")

leaflet() |>
  addTiles() |>
  addPolygons(data = max_extent[4,])

leaflet() |>
  addTiles() |>
  addMarkers(data = boat_ramps)

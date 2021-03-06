---
title: "Geography 176A"
author: "[Wesley Noble](https://wes-noble.github.io/)"
date: "09-14-2020"
subtitle: "Lab 06: Flood Risk in Mission Creek: Past, Present, Future"
output:
  html_document:
  theme: journal
---

library(tidyverse)
library(sf)
library(raster)
library(fasterize)
library(whitebox)
library(units)

# Data libraries
library(osmdata)
library(elevatr)

#creating basin boundary

basin = read_sf("https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-11119750/basin") %>%
  st_transform(4326)

AOI = AOI::aoi_get(basin)

#getting elevation data & saving raster

elev = elevatr::get_elev_raster(basin, z = 13) %>%
  crop(basin)

elev = elev * 3.281

writeRaster(elev, "/users/noblex/github/geog-176A-labs/data/mission-creek-basin-elev.tif", overwrite = TRUE)

#Buildings and river-network data

osm_buildings = osmdata::opq(basin) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()

osm_streams = osmdata::opq(AOI) %>%
  add_osm_feature(key = "waterway", value = "stream") %>%
  osmdata_sf()

buildings = osm_buildings$osm_polygons %>%
  st_transform(4326) %>%
  st_centroid() %>%
  st_intersection(basin)

railway_point = buildings %>%
  filter(amenity == "railway")

streams = osm_streams$osm_lines %>%
  st_transform(4326) %>%
  st_intersection(basin)

#creating hillshade

wbt_hillshade("/users/noblex/github/geog-176A-labs/data/mission-creek-basin-elev.tif", "/users/noblex/github/geog-176A-labs/data/mission-creek-basin-hillshade.tif")

hillshade = raster("/users/noblex/github/geog-176A-labs/data/mission-creek-basin-hillshade.tif")

plot(hillshade, col = gray.colors(256, alpha = .5), box = FALSE, axes = FALSE, main = "Hillshade", legend = FALSE)
plot(basin, add = TRUE)
plot(streams, add = TRUE, col = "blue", lwd = 2)

#creating river raster

streams_polygon = streams %>%
  st_transform(5070) %>%
  st_buffer(10) %>%
  st_transform(4326)

fast_streams = fasterize::fasterize(streams_polygon, elev)

writeRaster(fast_streams, "/users/noblex/github/geog-176A-labs/data/mission-creek-basin-faster-elev.tiff", overwrite=TRUE)

#Creating the hydrologically corrected surface

wbt_breach_depressions("/users/noblex/github/geog-176A-labs/data/mission-creek-basin-elev.tif", "/users/noblex/github/geog-176A-labs/data/mission-creek-basin-depression.tif")

#Creating the HAND raster

wbt_elevation_above_stream("/users/noblex/github/geog-176A-labs/data/mission-creek-basin-depression.tif", "/users/noblex/github/geog-176A-labs/data/mission-creek-basin-faster-elev.tif", "/users/noblex/github/geog-176A-labs/data/mission-creek-basin-hand.tif")

#Correcting to local reference datum

elev_abv_stream = raster("/users/noblex/github/geog-176A-labs/data/mission-creek-basin-hand.tif")

river_raster = raster("/users/noblex/github/geog-176A-labs/data/mission-creek-basin-faster-elev.tif")

elev_abv_stream = elev_abv_stream + 3.69

writeRaster(elev_abv_stream, "/users/noblex/github/geog-176A-labs/data/mission-creek-basin-offset.tif", overwrite=TRUE)

offset_raster = raster("/users/noblex/github/geog-176A-labs/data/mission-creek-basin-offset.tif")

offset_raster[offset_raster >= 10.02] <- NA

plot(hillshade, col = gray.colors(256, alpha = .5), box = FALSE, axes = FALSE, legend = FALSE)
plot(offset_raster, col = rev(blues9), add = TRUE, legend = FALSE)
plot(railway_point, col = "green", cex = 1, pch = 16, add = TRUE)

# Yes, this map seems to be a accurate representation of what a large flood event would look like in the Goleta area
# The flood expanse increases nearing the mouth of the river

# Estimating Impacts

flood_depth = raster::extract(offset_raster, buildings)

sum(!is.na(flood_depth))

cols = ifelse(!is.na(extract(offset_raster, buildings)), "red", "black")

plot(hillshade, col = gray.colors(256, alpha = .5), box = FALSE, axes = FALSE, legend = FALSE, main = paste(sum(!is.na(flood_depth)), " impacted structures"))
plot(offset_raster, col = rev(blues9), add = TRUE, legend = FALSE)
plot(railway_point, col = "green", cex = 1, pch = 16, add = TRUE)
plot(buildings, col = cols, pch = 16, cex = .08, add = TRUE)





---
  title: "Geography 176A"
author: "[Wesley Noble](https://wes-noble.github.io/)"
date: "08-31-2020"
subtitle: "Lab 04: Tesselations & Point-in-Polygon"
output:
  html_document:
  theme: journal
---

#Libraries

library(tidyverse)
library(sf)
library(rmapshaper)
library(mapview)
library(units)
library(knitr)
library(readxl)
library(viridis)
library(gghighlight)
library(leaflet)
library(leafpop)


# Question 1

#1.1: Getting CONUS

us_counties = USAboundaries::us_counties()


get_conus = function(data, var){
    filter(data, !get(var) %in%
             c("Hawaii", "Puerto Rico", "Alaska",
               "Guam", "District of Columbia"))
}

conus_counties = get_conus(us_counties, "name") %>%
  st_transform(5070)

counties = st_transform(us_counties, 5070) %>%
  select(name, geoid, state_name) %>%
  get_conus("state_name")

#1.2: Creating point "Anchors"

county_cent = st_centroid(counties) %>%
  st_union()

#1.3: Making tesselations

county_voroni = county_cent %>%
  st_voronoi() %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id = 1:n())

county_triangulation = county_cent %>%
  st_triangulate() %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id = 1:n())

county_grid = counties %>%
  st_make_grid(n = 70) %>%
  st_as_sf() %>%
  mutate(id = 1:n())

county_hexgrid = counties %>%
  st_make_grid(n = 70, square = FALSE) %>%
  st_as_sf() %>%
  mutate(id = 1:n())

#1.4: Cutting tessellations to CONUS

counties_union = counties %>%
  st_union()

county_voroni = county_voroni %>%
  st_intersection(counties_union)

county_triangulation = county_triangulation %>%
  st_intersection(counties_union)

#1.5: Resolving CONUS border

npts(counties_union)

resolved_counties_union = counties_union %>%
  ms_simplify(keep = .05)

npts(resolved_counties_union)


# Original object = 3239 points
# Resolved Object = 161 points
#I was able to remove 3078 points, which allows for the tessellations to compute a much smaller amount of calculations when intersected with the CONUS border

county_voroni = county_voroni %>%
  st_intersection(resolved_counties_union)

county_triangulation = county_triangulation %>%
  st_intersection(resolved_counties_union)

#1.6: Making function to plot Tessellation/Coverage

plot_tess = function(data, title){
  ggplot() +
    geom_sf(data = data, fill = "white", col = "navy", size = .2) +
    theme_void() +
    labs(title = title, caption = paste("This Tessellation has:", nrow(data), "tiles" )) +
    theme(plot.title = element_text(hjust = .5, color =  "navy", face = "bold"))
}

#1.7: Using our created function to plot our 4 coverages & original county data

plot_tess(county_voroni, "US Counties: Voroni Tessellation")

plot_tess(county_triangulation, "US Counties: Triangulation Tesselation")

plot_tess(county_grid, "US Counties: Grid Coverage")

plot_tess(county_hexgrid, "US Counties: Hexagonal Grid Coverage")

plot_tess(counties, "US Counties")

# Question 2

#2.1: Creating a function to summarize our tessellated surfaces


creat_dataframe = function(object, description){

  object_area = st_area(object) %>% units::set_units("km^2") %>% units::drop_units()

  data.frame(
    type = description,
    num = length(object_area),
    meanA = mean(object_area),
    sdA = sd(object_area),
    totA = sum(object_area)
  )

}


#2.2: Using function to summarize tessellations

voroni_dataframe = creat_dataframe(county_voroni, "voroni tessellation")

triangulation_dataframe = creat_dataframe(county_triangulation, "Traingulation tessellation")

grid_dataframe = creat_dataframe(county_grid, "Grid coverage")

hexgrid_dataframe = creat_dataframe(county_hexgrid, "Hexagonal Grid coverage")

originalcounties_dataframe = creat_dataframe(counties, "No Tessellation")

#2.3: Binding rows

tess_summary = bind_rows(
  voroni_dataframe,
  triangulation_dataframe,
  grid_dataframe,
  hexgrid_dataframe,
  originalcounties_dataframe)

#2.4: Data frame in Knitr table

kable(tess_summary, caption = "Tessellation, Coverage, & Raw County Data",
      col.names = c("Type", "Number of Features", "Mean Area of Features (km^2)", "Standard Deviation of Features", "Total Area (km^2)"),
      format.args = list(big.mark = ","))


#2.5: Comments

# Of the tessellations, the voroni tessellation closely matched the original conus counties well while simplifying points.
# I thought the grid and hexagonal coverage to be good uses for say population estimates in regional point-in-polygon research because of the size of their smaller polygons, but not for county or state level comparisons.
# It might just be that the triangulation tessellation has a lot going on in it, but I can't really render a good use for it with my spatial knowledge yet.

# Question 3

#3.1: Getting Raw data into data frame with CRS

tmp = read_excel("/users/noblex/github/geog-176A-labs/data/NID2019_U.xlsx")
NID2019_U = tmp %>%
  filter(!is.na(LONGITUDE), !is.na(LATITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(5070) %>%
  st_filter(counties_union)


#3.2: Developing point in polygon function

point_in_polygon = function(points, polygon, group){
  st_join(polygon, points) %>%
    st_drop_geometry() %>%
    count(get(group)) %>%
    setNames(c(group, "n")) %>%
    left_join(polygon, by = group) %>%
    st_as_sf()
}

#3.3: Using point in polygon function with tessellations

voroni_pip = point_in_polygon(NID2019_U, county_voroni, "id")

triangulation_pip = point_in_polygon(NID2019_U, county_triangulation, "id")

grid_pip = point_in_polygon(NID2019_U, county_grid, "id")

hexgrid_pip = point_in_polygon(NID2019_U, county_hexgrid, "id")

county_pip = point_in_polygon(NID2019_U, counties, "geoid")

#3.4: Creating function to plot point in polygon results

plot_pip = function(data, title){
  ggplot() +
    geom_sf(data = data, aes(fill = n), col = NA, alpha = .9, size = .2) +
    scale_fill_viridis_c() +
    theme_void() +
    labs(title = title, caption = paste0(sum(data$n), " dams represented" )) +
    theme(plot.title = element_text(hjust = .5, color =  "navy", face = "bold"))
}

#3.5: Plotting point in polygons with plot_pit function

plot_pip(voroni_pip, "Voroni Tessellation Point in Polygon")

plot_pip(county_pip, "Raw County Data Point in Polygon")

plot_pip(triangulation_pip, "Triangulation Tessellation Point in Polygon")

plot_pip(hexgrid_pip, "Hexagonal Coverage Point in Polygon")

plot_pip(grid_pip, "Grig Coverage Point in Polygon")

#3.6: Comments

#Because of the difference in the area feature of our polygons between the created coverages and tessellations, point in polygon amount and distribution are represented differetly.
#The voroni tessellation does a good job matching the original county distribution of dam points in the county polygons, while the grid coverage represents more polygons with fewer points becasue of their smaller area features.
#This relates to the MAUP problem because it shows how spatial rendering of districts or simply polygons can influence and mutate statistical analysis in point-based research.
#As we move on with this lab, I will use the voroni tessellation because I think it well represents what could be the watershed of a given river or river system that can be influenced by dams.

# Question 4

#4.1: Creating point in polygon for specific dam usage

#I will be choosing water supply, irrigation, hydroelectric, & flood control, because I think those are the most reasonable uses for dams to still be in operation

water_supply_dams = NID2019_U %>%
  filter(grepl("S", PURPOSES))

irrigation_dams = NID2019_U %>%
  filter(grepl("I", PURPOSES))

hydroelectric_dams = NID2019_U %>%
  filter(grepl("H", PURPOSES))

flood_control_dams = NID2019_U %>%
  filter(grepl("C", PURPOSES))

wsd_pip = point_in_polygon(water_supply_dams, county_voroni, "id")

id_pip = point_in_polygon(irrigation_dams, county_voroni, "id")

hd_pip = point_in_polygon(hydroelectric_dams, county_voroni, "id")

fcd_pip = point_in_polygon(flood_control_dams, county_voroni, "id")

#4.2: Plotting new point in polygon's with gghighlight for select dams

plot_pip(wsd_pip, "Water Supply Dams Across US") +
  gghighlight(n > (mean(n) + sd(n)))

plot_pip(id_pip, "Irrigation Dams Across US") +
  gghighlight(n > (mean(n) + sd(n)))

plot_pip(hd_pip, "Hydroelectric Dams Across US") +
  gghighlight(n > (mean(n) + sd(n)))

plot_pip(fcd_pip, "Flood Control Dams Across US") +
  gghighlight(n > (mean(n) + sd(n)))

#4.3: Comments

#I think the flood control plot is really interesting because it highlights where dams may in places along the Mississippi River, which makes a lot of sense due to the river's geographic extent
#The distribution of hydroelectric dams was a little odd to me. I don't really know why many of them are clumped to the far Northeast and West Coast.
#The fact that most damns in the US are not hydroelectric confuses me because I would assume most dams should be utilizing such technology as a source or income as well.
#Also, being that dams are a major burden to natural fish migration and spawning in the NW, with the removal of dams in the PNW to revive stream ecology, will such regions and states begin to result back to fossil fuels or move to other renewable energy sources.

# Extra Credit

#reading in the data

miss = read_sf('/users/noblex/github/geog-176A-labs/data/MajorRivers.shp') %>%
  filter(SYSTEM == "Mississippi") %>%
  st_transform(4326)

#filter to largest and most high risk dam in each state

biggest_riskyest_dams = NID2019_U %>%
  filter(HAZARD == "H", grepl("C", PURPOSES)) %>%
  group_by(STATE) %>%
  slice_max(NID_STORAGE) %>%
  select("DAM_NAME", "NID_STORAGE", "YEAR_COMPLETED", "PURPOSES") %>%
  st_transform(4326)

#Make Leaflet

leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  addPolylines(data = miss) %>%
  addCircleMarkers(data = biggest_riskyest_dams, fillOpacity = .5, radius = ~NID_STORAGE/1500000, color = "red", stroke = FALSE,
                   popup = popupTable(st_drop_geometry(biggest_riskyest_dams), feature.id = FALSE, row.number = FALSE))




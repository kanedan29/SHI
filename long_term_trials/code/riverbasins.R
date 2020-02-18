# World Bank Climate Data API
# River basin ids and polygon mappings from https://github.com/hrbrmstr/basins.git
# 
library(rgeos)
library(geojsonio)
library(sp)

load("basins-master/basins.rda")

# Code from https://stackoverflow.com/a/33654743
# Save local versions of basin data for querying API

basins <- do.call(rbind, basin_list)
save(basin_list, basins, file="data/basins.rda")
geojsonio::geojson_write(basins, geometry="polygon", group="basin_id", "basins.json",
                         file = "data/riverbasins.geojson")

source("code/libraries.R")

load("data/d.carbon.RData")
load("data/d.yield.RData")

d.yield <- d
rm(d)

d.yield %>%
  group_by(Paper, Study_name, begin_obs, end_obs) %>%
  count() %>%
  select(Paper, Study_name, begin_obs, end_obs) -> yield.climate

d.carbon %>%
  group_by(Paper, Study_name, Year_of_observation) %>%
  count() %>%
  select(Paper, Study_name, Year_of_observation) -> carbon.climate

# Need to clean up location names to searchable formats; default to city, country
# with more specificity if available

yield.climate <- yield.climate %>%
  mutate(Location = Study_name) %>%
  mutate(Location = str_replace_all(Location, 
                                c("Makoka Agricultural Research Station" = "Thondwe, Malawi",
                                  "Bulbertson" = "Culbertson",
                                  "Pietranera farm, Universita degli Studi di Palermo" = "Pietranera farm, Agrigento, Sicily, Italy",
                                  "Research Station of the Riograndense Rice Institute \\(IRGA\\), Cachoeirinha City, Rio Grande do Sul State, Southern Brazil" = "Cachoeirinha City, Rio Grande do Sul State, Brazil",
                                  "Purdue University Agronomy Center for Research and Education \\(ACRE\\), West Lafayette, Indiana" = "West Lafayette, IN",
                                  "La Canaleja \\(INIA\\), " = "",
                                  "USDA-ARS Conservation and Production Research Laboratory, " = "",
                                  "USDA-ARTS Central Great Plains Research Station, " = "",
                                  "Loess Plateau, Chenghuang village, " = "",
                                  "University Farm, Institue for Sustainable Agro-ecosystem Services, " = "",
                                  "Southern Illinois University Belleville Research Center, " = "",
                                  "Mississippi Agricultural and Forestry Experiment Station at Holly Springs" = "Holly Springs, MS",
                                  "Macdonald Campus Research Farm, McGill University, " = "",
                                  "Southeast Norway, morainic loam soil" = "Hedmark, Norway",
                                  "USDA-ARS J. Phil Campbell Sr. Natural Resource Conservation Center, " = "",
                                  "No name" = "Clay Center, NE", # Based on statement in paper that manure was transported from nearby US Meat Animal Research Center
                                  "La Chimenea field Station, " = "",
                                  "Columbia Basin Agricultural Research Center, " = "",
                                  "Yadkin Valley Vineyard, North Carolina, USA" = "New Hope, NC",
                                  "Hayathnagar Research Farm, Central Research Institute for Dryland Agriculture, " = "",
                                  "Macdonald Research Farm of McGill University, Ste-Anne de Beelevue, " = "Ste-Anne-de-Bellevue, ",
                                  "Hyndevad, Jutland, Denmark \\s+Odum, Jutland, Denmark" = "Jutland, Denmark",
                                  "Shang Tuhe village, " = "",
                                  "Instituto Madrileno de Investigacion y Desarollo Rural Agrario y Alimentario Experimental Station, " = "",
                                  "Farm of Southwest Agricultural University, " = "",
                                  "Beiqiu, " = "",
                                  "Research farm, Rajendra Agricultural University, " = "",
                                  "Haycreek Township, Goodhue Country, Minnesota, USA" = "Red Wing, MN",
                                  "Clemson University Research and Education Center, " = "",
                                  "Iowa State University \\(ISU\\) Agronomy/Ag Engineering Research and Education Center, Boone County, Iowa, USA" = "Boone County, IA",
                                  "Research farm, International Institute of Tropical Agriculture \\(IITA\\), Ihadan, " = "Ibadan, ",
                                  "Tidewater Research Station, " = "",
                                  "International Center for Agricultural Research in the Dry Areas \\(ICARDA\\), Tel Hadya, " = "",
                                  "Oklahoma Pandhandle Research and Extension Center \\(OPREC\\), " = "",
                                  "Northwestern Agricultural Research Station, " = "",
                                  "Northwestern Branch, " = "",
                                  "Ohio Agricultural Research and Development Center, " = "",
                                  "Waterman Farm, " = "",
                                  "Site = Agramunt" = "Agramunt, Catalonia, Spain",
                                  "Site = El Canos" = "El Canos, Catalonia, Spain",
                                  "Site = Selvanera" = "Selvanera, Catalonia, Spain",
                                  "University of Minnesota Agricultural Experiment Station, " = "",
                                  "Palouse Conservation Field Station, " = "",
                                  "Shiwang Village, " = "",
                                  "Enrico Avanzi Interdepartmental Center for Agro-Environmental Research, " = "",
                                  "Agriculture and Agri-Food Canada Research Centre, " = "",
                                  "Cantuar, " = "Swift Current, ",
                                  "Horticultural Experiment Station, " = "",
                                  "L'Acadie Research Station, Agriculture and Agri-Food Canada" = "St-Jean-sur-Richelieu, Quebec",
                                  "Swedish University of Agricultural Sciences, " = "",
                                  "Eastern South Dakota Soil and Water Research Farm, " = "",
                                  "Rasmussen dryland farm site, " = "",
                                  "Regional Agricultural Research Station, Bhairhawa, " = "Bhairahawa, ",
                                  "Baden-Wurttemberg site" = "Forchtenberg, Germany",
                                  "Fire" = "Forchtenberg, Germany",
                                  "Forchtenberg site" = "Forchtenberg, Germany",
                                  "Stagnic Luvisol" = "Forchtenberg, Germany",
                                  "Haplic Luvisol" = "Forchtenberg, Germany",
                                  "Fort Ellis Research and Extension Center, Montana State University, " = "",
                                  "Hisar" = "Hisar, India",
                                  "Solapur" = "Solapur, India",
                                  "Eastern site" = "Horse Heaven, WA",
                                  "Western site" = "Horse Heaven, WA",
                                  "University of Nebraska Agricultural Research and Development Center, " = "",
                                  "Tennessee Valley Research and Extension Center, Alabama Agriculture Experiment Station, " = "",
                                  "Taihu Lake region, China" = "Taihu Lake, China",
                                  "Asasa research stations, south-eastern highlands, Ethiopia" = "Asassa, Ethiopia",
                                  "Kulumsa research stations, south-eastern highlands, Ethiopia" = "Kulumsa, Ethiopia",
                                  "Shenmu erosion and environmental testing station, Institute of Soil and Water Conservation, Shen Mu county, " = "",
                                  "Chilato village, Zimuto Communal Area \\(Chikato\\), " = "",
                                  "Henderson Research Station, " = "",
                                  "Hereford Farm, " = "",
                                  "National Agriculture Research Center, " = "",
                                  "ADAS Bridgets, " = "",
                                  "ADAS Drayton, " = "",
                                  "ADAS High Mowthorpe, " = "",
                                  "ADAS Terrington, " = "",
                                  "Campo Experimental Norman E. Borlaug, " = "",
                                  "University of California Coachella Valley Agricultural Research Station, " = "",
                                  "Sand Mountain Research and Extension Center, Appalachian Plateau region, " = "",
                                  "Southern Loess Plateau" = "Zhouzhi, Xi'an, Shaanxi, China",
                                  "L'Acadie Experimental Farm of Agriculture and Agri-Food Canada, " = "St-Jean-sur-Richelieu, ")))

carbon.climate <- carbon.climate %>%
  mutate(Location = Study_name) %>%
  mutate(Location = str_replace_all(Location,
                                   c("Oklahoma Pandhandle Research and Extension Center \\(OPREC\\), " = "",
                                     "Southeast Norway, morainic loam soil" = "Hedmark, Norway",
                                     "La Chimenea field Station, " = "",
                                     "Columbia Basin Agricultural Research Center, " = "",
                                     "Shang Tuhe village, " = "",
                                     "Farm of Southwest Agricultural University, " = "",
                                     "Beiqiu, " = "",
                                     "Research farm, Rajendra Agricultural University, " = "",
                                     "Clemson University Research and Education Center, " = "",
                                     "Northwestern Agricultural Research Station, " = "",
                                     "Ohio Agricultural Research and Development Center, " = "",
                                     "Waterman Farm, " = "",
                                     "Pietranera farm, Universita degli Studi di Palermo" = "Pietranera farm, Agrigento, Sicily, Italy",
                                     "Enrico Avanzi Interdepartmental Center for Agro-Environmental Research, " = "",
                                     "L'Acadie Research Station, Agriculture and Agri-Food Canada" = "St-Jean-sur-Richelieu, Quebec",
                                     "Eastern South Dakota Soil and Water Research Farm, " = "",
                                     "Regional Agricultural Research Station, Bhairhawa, " = "Bhairahawa, ",
                                     "University of Nebraska Agricultural Research and Development Center, " = "",
                                     "Shenmu erosion and environmental testing station, Institute of Soil and Water Conservation, Shen Mu county, " = "",
                                     "Chilato village, Zimuto Communal Area \\(Chikato\\), " = "",
                                     "Henderson Research Station, " = "",
                                     "Hereford Farm, " = "",
                                     "ADAS Bridgets, " = "",
                                     "ADAS Drayton, " = "",
                                     "ADAS High Mowthorpe, " = "",
                                     "ADAS Terrington, " = "",
                                     "Southern Loess Plateau" = "Zhouzhi, Xi'an, Shaanxi, China"
                                     )))
  
  
yield.climate %>%
  group_by(Paper, Study_name, Location) %>%
  select(Paper, Study_name, Location) %>%
  distinct(Paper, Study_name, Location) -> yield.locations


yield.locations$lat <- NA
yield.locations$lon <- NA


get_geo <- function(study_name){
  result <- geocode_OSM(study_name, return.first.only = TRUE, as.data.frame = TRUE)
  return(result)
}

# Takes awhile

for (i in 1:nrow(yield.locations)){
  geo <- try(get_geo(yield.locations$Location[i]))
  if(inherits(geo, "try-error"))
  {
    next
  }
  else
  {
    yield.locations$lat[i] <- geo["lat"]
    yield.locations$lon[i] <- geo["lon"]
  }
}

yield.locations$lat[which(yield.locations$Location=="Cangwu Agri-ecological Station of the Loess Plateau, China")] <- 35.200
yield.locations$lon[which(yield.locations$Location=="Cangwu Agri-ecological Station of the Loess Plateau, China")] <- 107.667

yield.locations$lat <- as.numeric(as.character(yield.locations$lat))
yield.locations$lon <- as.numeric(as.character(yield.locations$lon))

save(yield.locations, file = "data/locations.RData")

################################################################################
## The following is moot from the incorporation of TerraClimate data
################################################################################
# Climate data

# World Bank Climate Data API
# River basin ids and polygon mappings from https://github.com/hrbrmstr/basins.git

# load("data/basins.RData")
# load("data/climate_data.RData")


# Convert lat/lon into spatial points
# library(sp)
# points <- SpatialPointsDataFrame(cbind(yield.locations$lon, yield.locations$lat),
#                                 data = yield.locations,
#                                 proj4string = CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
                      
                     

# With points and basins, need to find which basin each point is in
# yield.locations$wb_basin <- NA

# library(rgeos)

# for (row in 1:nrow(yield.locations)) {
#  spot <- points[row,]
#  b <- which(sapply(1:468, function(i) gContains(basin_list[[i]], spot)))
#  yield.locations[row, 'wb_basin'] <- b
#}


# save(yield.locations, file = "data/locations.RData")

################################################################################
## Add lat/lon and basin data back into climate dataframes

# yield.climate <- yield.climate %>%
#  left_join(yield.locations, by = c("Paper", "Location"))

# carbon.climate <- carbon.climate %>%
#  left_join(yield.locations, by = "Location") %>%
#  select(-Paper.y) %>%
#  rename(Paper = Paper.x)

### Climate data from APIs ###
## Before 2000: World Bank Climate Data API
## Date ranges:
##   1920-1939; 1940-1959; 1960-1979; 1980-1999
## 2014-2018: Open Land Maps Precipitation
## 2000-2018: Open Land Maps Temperature


# See how data is returned from API
# prep_test_wb <- fromJSON("http://climatedataapi.worldbank.org/climateweb/rest/v1/basin/annualavg/pr/1980/1999/413")
# prep_test_olm <- fromJSON("http://landgisapi.opengeohub.org/query/point?lat=7.58033&lon=35.6561&coll=layers1km&regex=clm_precipitation_imerge.(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)_m_1km_s0..0cm_.*_v0.1.tif")
# response <- prep_test_olm[['response']]

# Load parsing functions
# source("code/get_climate_functions.R")

# Easier to run function for both datasets than to join due to year mismatches

# yield.climate$precipitation <- NA
# yield.climate$temperature <- NA
# carbon.climate$precipitation <- NA
# carbon.climate$temperature <- NA

# yield.climate <- getPrecipData(yield.climate)
# yield.climate <- getTempData(yield.climate)

# carbon.climate <- getPrecipData(carbon.climate)
# carbon.climate <- getTempData(carbon.climate)


# save(yield.climate, carbon.climate, file = "data/climate_data.RData")






  









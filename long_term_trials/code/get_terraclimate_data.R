## Functions to download relevant climate data from TerraClimate database
## Good for years 1958-2015
#
## Source: Abatzoglou, J. T.et al. TerraClimate, a high-resolution global dataset of monthly climate and climatic
## water balance from 1958–2015. Sci. Data 5:170191 doi: 10.1038/sdata.2017.191(2018).
#
## Data Limitations as expressed by authors:
## 1. Long-term trends in data are inherited from parent datasets. TerraClimate 
##    should not be used directly for independent assessments of trends.
## 2. TerraClimate will not capture temporal variability at finer scales than 
##    parent datasets and thus is not able to capture variability in orographic 
##    precipitation ratios and inversions.
## 3. The water balance model is very simple and does not account for heterogeneity 
##    in vegetation types or their physiological response to changing environmental conditions.
## 4. Limited validation in data-sparse regions (e.g., Antarctica)


source("code/libraries.R")
library(ncdf4)
load("data/locations.RData")

# Create month and year labels for data

years <- c(1958:2018)
months <- c(1:12)
timeframe <- expand.grid(years, months)
timeframe <- timeframe %>%
  rename("year" = "Var1", "month" = "Var2") %>%
  arrange(year, month)

# Create list of variables to query from database:

vars <- c("tmin", "tmax", "ppt", "soil", "pet", "aet", "def", "PDSI")

# "tmin": minimum monthly temperature (C)
# "tmax": maximum monthly temperature (C)
# "ppt": precipitation (mm)
# "soil": soil moisture (?mm)
# "pet": potential evapotranspiration (mm/month)
# "aet": actual evapotranspiration (mm/month)
# "def": climate water deficit (mm/month; pet - aet, i.e., the amount of additional water that would have 	      
#								evaporated or transpired if it had been present at the forcing temperature)
# "PDSI": Palmer Drought Severity Index (0 = normal, < 0 = drought, > 0 = wet)

dfList <- list()

# Need to manually enter lon index for row 97 (Turley et al. 2003;	ADAS Terrington, Norfolk, England;	Norfolk, England),
# as either 0 or 2 lon indexes are returned.
# Used 4344 as manual lon index

for (i in 1:nrow(yield.locations)) {
  df <- merge(yield.locations[i,], timeframe)
  
  lat <- yield.locations[[i, 'lat']]
  lon <- yield.locations[[i, 'lon']]
  
  for (var in vars) {
    pathname <- paste("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_", 
                      var, "_1958_CurrentYear_GLOBE.nc", sep="")
    nc <- nc_open(pathname)
    
    x <- ncvar_get(nc, "lon")
    y <- ncvar_get(nc, "lat")
    flat = match(abs(y - lat) < 1/48, 1)
    latindex = which(flat %in% 1)
    if (i == 97){
      	lonindex = 4344
    }
    else {
    	flon = match(abs(x - lon) < 1/48, 1)
      	lonindex = which(flon %in% 1)
    }
    start <- c(lonindex, latindex, 1)
    count <- c(1, 1, -1)
    
    data <- as.numeric(ncvar_get(nc, varid = var, start = start, count))
    df <- cbind(df, data)
    df <- df %>% rename(!!var := "data")
    
    nc_close(nc)
  }
  
  dfList[[i]] <- df
}

# Create one dataset for all monthly variables
full_climate <- bind_rows(dfList)

add_spei <- function(df) {
  SPEI.3 <- spei((df$ppt - df$pet), scale = 3)
  SPEI.6 <- spei((df$ppt - df$pet), scale = 6)
  SPEI.9 <- spei((df$ppt - df$pet), scale = 9)
  SPEI.12 <- spei((df$ppt - df$pet), scale = 12)
  
  cbind(df, as.numeric(SPEI.3$fitted), as.numeric(SPEI.6$fitted), 
        as.numeric(SPEI.9$fitted), as.numeric(SPEI.12$fitted))
}

intermediate <- split(full_climate, full_climate$Paper, drop = FALSE)

intermediate <- lapply(intermediate, add_spei)

full_climate <- bind_rows(intermediate)

full_climate %>%
  rename("SPEI.3" = "as.numeric(SPEI.3$fitted)",
         "SPEI.6" = "as.numeric(SPEI.6$fitted)",
         "SPEI.9" = "as.numeric(SPEI.9$fitted)",
         "SPEI.12" = "as.numeric(SPEI.12$fitted)") -> full_climate

  
  

# Collapse dataset by year
meanmonthly_climate <- full_climate %>%
	group_by(Paper, Study_name, Location, lat, lon, year) %>%
	summarise(
		tmin = mean(tmin),
		tmax = mean(tmax),
		ppt = mean(ppt),
		soil = mean(soil),
		pet = mean(pet),
		aet = mean(aet),
		def = mean(def),
		PDSI = mean(PDSI),
		SPEI.3 = mean(SPEI.3),
		SPEI.6 = mean(SPEI.6),
		SPEI.9 = mean(SPEI.9),
		SPEI.12 = mean(SPEI.12))
		
save("full_climate", file = "data/full_climate.RData")
save("meanmonthly_climate", file = "data/meanmonthly_climate.RData")

### Functions to parse API data ################################################

### Climate data from APIs ###
## Before 2000: World Bank Climate Data API
## Date ranges:
##   1920-1939; 1940-1959; 1960-1979; 1980-1999
## 2014-2018: Open Land Maps Precipitation
## 2000-2018: Open Land Maps Temperature

# Creates column of average annual precipitation in mm for a given location
getPrecipData <- function(df) {
  for (row in 1:nrow(df)) {
    if ('begin_obs' %in% colnames(df)) {
      year <- df[row, 'begin_obs']
    }
    else {
      year <- df[row, 'Year_of_observation']
    }
    basin <- df[row, 'wb_basin']
    lat <- df[row, 'lat']
    lon <- df[row, 'lon']
    
    if (year < 2000) {
      if (year >= 1980) {
        dates = "1980/1999"
      }
      else if (year >= 1960) {
        dates = "1960/1979"
      }
      else if (year >= 1940) {
        dates = "1940/1959"
      }
      else if (year >= 1920) {
        dates = "1920/1939"
      }
      
      query <- paste("http://climatedataapi.worldbank.org/climateweb/rest/v1/basin/annualavg/pr/", dates, "/", basin, sep="")
      data <- fromJSON(query)
      df[row, 'precipitation'] <- mean(as.numeric(data$annualData))
    }
    
    else if (year > 2013) {
      query <- paste("http://landgisapi.opengeohub.org/query/point?lat=", lat, "&lon=", lon, "&coll=layers1km&regex=clm_precipitation_imerge.(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)_m_1km_s0..0cm_.*_v0.1.tif", sep="")
      response <- fromJSON(query)
      data <- response[['response']]
      df[row, 'precipitation'] <- sum(as.numeric(data[3:14]))
    }
    
    ## data might not be available for the required years, still need to find new dataset
    else {
      df[row, 'precipitation'] <- NA
    }
  }
  
  return(df)
}

# Creates column of average annual temperature in Celsius for given location
getTempData <- function(df) {
  for (row in 1:nrow(df)) {
    if ('begin_obs' %in% colnames(df)) {
      year <- df[row, 'begin_obs']
    }
    else {
      year <- df[row, 'Year_of_observation']
    }
    basin <- df[row, 'wb_basin']
    lat <- df[row, 'lat']
    lon <- df[row, 'lon']
    
    if (year < 2000) {
      if (year >= 1980) {
        dates = "1980/1999"
      }
      else if (year >= 1960) {
        dates = "1960/1979"
      }
      else if (year >= 1940) {
        dates = "1940/1959"
      }
      else if (year >= 1920) {
        dates = "1920/1939"
      }
      
      query <- paste("http://climatedataapi.worldbank.org/climateweb/rest/v1/basin/annualavg/tas/", dates, "/", basin, sep="")
      data <- fromJSON(query)
      df[row, 'temperature'] <- mean(as.numeric(data$annualData))
    }
    
    else if (year >= 2000) {
      query <- paste("https://landgisapi.opengeohub.org/query/point?lat=", lat, "&lon=", lon, "&coll=layers1km&&regex=clm_lst_mod11a2.(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec).day_m_1km_s0..0cm_.*_v1.0.tif", sep="")
      response <- fromJSON(query)
      data <- response[['response']]
      
      # Convert from displayed Kelvin to Celsius
      df[row, 'temperature'] <- (mean(as.numeric(data[3:14])) * 0.02) - 273.15
    }
    
    # Shouldn't have same issue for temp data, but just in case
    else {
      df[row, 'temperature'] <- NA
    }
  }
  
  return(df)
}

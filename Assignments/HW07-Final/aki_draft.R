# Data Exploration for Final project (MUSA5080)

# Akira Di Sandro, 11/28/23

# load packages ----
{
  # remotes::install_github("CityOfPhiladelphia/rphl")
  library(tidyverse)
  library(tidycensus)
  library(sf)
  # library(rphl)
  library(kableExtra)
  library(grid)
  library(gridExtra)
}

# set working directory
setwd("~/Documents/MUSA5080")

# load functions ----
{
  source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
}


# load data ----
{
  # philly permit data
  # dat_permit <- st_read("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+permits&filename=permits&format=geojson&skipfields=cartodb_id")
  # 
  # # only keep new construction permits
  # newcon_permits <- dat_permit %>% 
  #   filter(grepl("NEW CON|NEWCON",typeofwork)) %>% 
  # st_transform('ESRI:102728')
  
  # write out smaller geojson file
  # st_write(newcon_permits,"Assignments/HW07-Final/data/newcon_permits.geojson")
  
  # rm(dat_permit)
  
  newcon_permits <- st_read("Assignments/HW07-Final/data/newcon_permits.geojson")
  
  # OPA housing permit data
  {
    # missing
  }
  
  # census data
  census_vars <- c() # census variables of interest 
  
  # philly neighborhood data
  nhoods_path <- 'https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson'
  nhoods <- st_read(nhoods_path, quiet = T) %>%
    st_transform('ESRI:102728') %>%
    dplyr::select(mapname)
  
  # philly bounds
  philly <- st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson") %>%
    st_transform('ESRI:102728') %>% 
    dplyr::select(OBJECTID,geometry)
  
  # vacant lots/buildings
  vacant_centroids <- st_read("https://opendata.arcgis.com/datasets/b990222a527849229b4192feb4c42dc0_0.geojson") %>% 
    st_transform('ESRI:102728') %>% 
    st_centroid() %>%
    mutate(Legend = "Vacants") %>%
    dplyr::select(Legend)
  
  # parks & rec
  ppr_sites <- st_read("https://opendata.arcgis.com/api/v3/datasets/9eb26a787a6e448ba426eea7f9f0d93a_0/downloads/data?format=geojson&spatialRefId=4326") %>% 
    st_transform('ESRI:102728') %>% 
    mutate(Legend = "Parks and Rec") %>%
    dplyr::select(Legend)
  
  # transit stops
  el <- st_read("https://opendata.arcgis.com/datasets/8c6e2575c8ad46eb887e6bb35825e1a6_0.geojson") %>% 
    st_transform('ESRI:102728')
  bsl <- st_read("https://opendata.arcgis.com/datasets/2e9037fd5bef406488ffe5bb67d21312_0.geojson") %>% 
    st_transform('ESRI:102728')
  
  septaStops <-
    rbind(
      el %>%
        mutate(Line = "El") %>%
        dplyr::select(Station, Line),
      bsl %>%
        mutate(Line ="Broad_St") %>%
        dplyr::select(Station, Line)) %>%
    st_transform('ESRI:102728') %>%
    mutate(Legend = "Subway Stops") %>%
    dplyr::select(Legend)
  
  # proximity to CBD using city_hall as proxy
  city_hall <- bsl %>%
    st_transform('ESRI:102728') %>%
    filter(Station=="City Hall") %>%
    mutate(Legend = "City Hall") %>%
    dplyr::select(Legend)
  
  # schools
  schools <- st_read('https://opendata.arcgis.com/datasets/d46a7e59e2c246c891fbee778759717e_0.geojson') %>% 
    st_transform('ESRI:102728') %>% 
    mutate(Legend = "Schools") %>% 
    dplyr::select(Legend)
  
  
}

# Data Exploration
{
  
  
  # predictors for permit count model
  # - inflation rate, unemployment rate,
  # - demographics of neighborhood
  # - building safety codes
}




# Data Exploration for Final project (MUSA5080)

# Akira Di Sandro, 11/28/23

# load packages
{
  # remotes::install_github("CityOfPhiladelphia/rphl")
  library(tidyverse)
  library(tidycensus)
  library(sf)
  library(rphl)
  library(kableExtra)
  library(grid)
  library(gridExtra)
}

# set working directory
setwd("~/Documents/MUSA5080")

# load functions
{
  
}


# load data
{
  # code from Nissim to download Philadelphia permit data
  base_url <- "https://phl.carto.com/api/v2/sql"
  
  three_years_ago <- (lubridate::ymd(Sys.Date()) - lubridate::years(11))
  one_year_ago <- (lubridate::ymd(Sys.Date()) - lubridate::years(1))
  
  building_perms_query <- sprintf("
                  SELECT 
                  address,
                  addressobjectid,
                  approvedscopeofwork,
                  commercialorresidential,
                  opa_account_num,
                  permittype,
                  status,
                  unit_num,
                  unit_type,
                  permitissuedate, 
                  typeofwork, 
                  ST_Y(the_geom) AS lat, ST_X(the_geom) AS lng
                  FROM permits
                  WHERE permitissuedate >= '%s' AND permitissuedate < '%s'
                 ", three_years_ago, one_year_ago)
  
  building_permits <- st_as_sf(get_carto(building_perms_query,
                                         format = 'csv',
                                         base_url = base_url,
                                         stringsAsFactors = FALSE) |>
                                 dplyr::filter(
                                   !is.na(lat),
                                   !is.na(lng)),
                               coords = c("lng", "lat"),
                               crs = st_crs('EPSG:4326')) |>
    st_transform(crs = st_crs("EPSG:2272")) %>%
    mutate(permits_count = 1)
  
  # write permits to the data subfolder
  building_permits_path <- "data/building_permits.geojson"
  saveRDS(building_permits, building_permits_path)
}

# Data Exploration
{
  # making simple tables of types of permits
  permit_type <- sort(unique(building_permits$permittype))
  work_type <- sort(unique(building_permits$typeofwork))
  
  
}




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
  
}


# load data ----
{
  ## philly permit data ----
  {
    dat_permit <- st_read("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+permits&filename=permits&format=geojson&skipfields=cartodb_id")
    
    # only keep new construction permits
    newcon_permits <- dat_permit %>% 
      filter(grepl("NEW CON|NEWCON",typeofwork))
  }
  
  
  
  
  # OPA housing permit data
  
  
  # census data
  census_vars <- c() # census variables of interest
  
  # philly neighborhood data
  nhoods_path <- 'https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson'
  nhoods <- st_read(nhoods_path, quiet = T) %>%
    st_transform('ESRI:102728') %>%
    dplyr::select(mapname)
}

# Data Exploration
{
  # making simple tables of types of permits
  permit_type <- sort(unique(dat_permit$permittype))
  work_type <- sort(unique(dat_permit$typeofwork))
  
  tow_tokeep <- work_type[grep("NEW|MAJOR",unique(work_type))] # typeofwork values to keep: values that contain "new", "major"
  
  dat_permit_tokeep <- dat_permit %>% 
    mutate(year = format(mostrecentinsp, format="%Y")) %>% 
    filter(typeofwork %in% tow_tokeep)
  
  dat_permit_complete <- dat_permit_tokeep %>% 
    filter(status == "COMPLETED")  # only keep completed permits for now
  
  # predictors for permit count model
  # - inflation rate, unemployment rate,
  # - demographics of neighborhood
  # - building safety codes
}




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
  # reading geojson file
  dat_permit <- st_read("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+permits&filename=permits&format=geojson&skipfields=cartodb_id")
  
  st_write(dat_permit,"Assignments/HW07-Final/data/permit_data.geojson")
  
}

# Data Exploration
{
  # making simple tables of types of permits
  permit_type <- sort(unique(dat_permit$permittype))
  work_type <- sort(unique(dat_permit$typeofwork))
  
  
}




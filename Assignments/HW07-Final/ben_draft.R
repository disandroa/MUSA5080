# Data Exploration for Final project (MUSA5080)

# Benjamin Myers, 11/28/23

# Load packages
{library(sf)
  library(spdep)
  library(caret)
  library(ckanr)
  library(FNN)
  library(grid)
  library(gridExtra)
  library(ggcorrplot) # plot correlation plot
  library(corrr)      # another way to plot correlation plot
  library(kableExtra)
  library(jtools)     # for regression model plots
  library(ggstance) # to support jtools plots
  library(ggpubr)    # plotting R^2 value on ggplot point scatter
  library(broom.mixed) # needed for effects plots
  library(sfdep)
  library(tidycensus)
  library(stargazer)
  library(httr)
  library(tmap)
  library(scales)
  library(lubridate)
  library(MASS)}
{
library(pracma)
library(segmented)
library(ggforce)
library(tidyverse)
library(kableExtra)
library(caret)
library(knitr) 
library(pscl)
library(plotROC)
library(pROC)
library(lubridate)
library(viridis)
library(tidyverse)
library(sf)
library(spdep)
library(caret)
library(ckanr)
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot) # plot correlation plot
library(corrr)      # another way to plot correlation plot
library(kableExtra)
library(jtools)     # for regression model plots
library(ggstance) # to support jtools plots
library(ggpubr)    # plotting R^2 value on ggplot point scatter
library(broom.mixed) # needed for effects plots
library(sfdep)
library(tidycensus)
library(stargazer)
library(httr)
library(tmap)
library(scales)
library(lubridate)
library(MASS)
library(lubridate)
library(tidyverse)
library(caret)
library(kableExtra)
library(ModelMetrics)
library(plotROC)
library(knitr)
library(grid)
library(gridExtra)
library(QuantPsyc)
library(lubridate)
library(tidyverse)
library(caret)
library(kableExtra)
library(ModelMetrics)
library(plotROC)
library(knitr)
library(grid)
library(gridExtra)
library(QuantPsyc)
library(rphl)
library(geojsonR)
}


# LOAD VARIABLES AND MAKE FISHNETs
## City Limits
{
  cityLims <- st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson")%>%
    st_transform(crs=2272)
}

## Create Fishnet
{
  fishnet <- st_make_grid(cityLims,cellsize=500,crs = 2272, square = TRUE)%>%
    st_sf()%>%
    mutate(UniqueID = row_number())
}
## Permit Reading
{
  permits <- st_read("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+permits&filename=permits&format=geojson&skipfields=cartodb_id")%>%
    filter(.,typeofwork == "NEW CONSTRUCTION" & status == "COMPLETED"
           )%>%
    st_transform(crs=2272)
}
{
permitnet <- dplyr::select(permits) %>% 
  mutate(countPermit = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countPermit = replace_na(countPermit, 0),
         uniqueID = 1:n(),
         #cvID = sample(round(nrow(fishnet) / 24)
                       )
   #      )
}  
  ## City Limits
{
cityLims <- st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson")%>%
    st_transform(crs=2272)
}

## Create Fishnet
{
fishnet <- st_make_grid(cityLims,cellsize=500,crs = 2272, square = TRUE)%>%
    st_sf()%>%
    mutate(UniqueID = row_number())
}
  
## Neighborhood Reading
{
  phl.nh <- st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson") %>%
    st_transform(crs=2272) 
}

## Tree Reading
{
  PHLtrees<- st_read("https://opendata.arcgis.com/api/v3/datasets/30ef36e9e880468fa74e2d5b18da4cfb_0/downloads/data?format=geojson&spatialRefId=4326") %>%
    st_transform(crs=2272)
}
{
    Treenet <- dplyr::select(PHLtrees) %>% 
      mutate(countTree = 1) %>% 
      aggregate(., fishnet, sum) %>%
      mutate(countTree = replace_na(countTree, 0),
             uniqueID = 1:n(),
             #cvID = sample(round(nrow(fishnet) / 24)
                           )
             #)
}
## School Reading
{
  SchoolsPHL <- st_read("https://opendata.arcgis.com/datasets/d46a7e59e2c246c891fbee778759717e_0.geojson")%>%
    st_transform(crs=2272)
}
{
  Schoolnet <- dplyr::select(SchoolsPHL) %>% 
    mutate(countSchool = 1) %>% 
    aggregate(., fishnet, sum) %>%
    mutate(countSchool = replace_na(countSchool, 0),
           uniqueID = 1:n(),
      #     cvID = sample(round(nrow(fishnet) / 24)
    )
  #)
}

# {
#     schoolDistNet <- fishnet %>%
#     mutate(
#       schools1nn = nn_function(st_coordinates(fishnet),  st_coordinates(SchoolsPHL), k = 1))
# }
#   
# {
#   schoolDistNet <-
#   fishnet %>%
#   mutate(
#     MeanSchoolDist = mean(st_distance(st_coordinates(st_centroid(fishnet)), st_coordinates(SchoolsPHL)))is.na()
#   )
#   }
{
fishnetCentroid <- st_centroid(fishnet)
MeanSChoolDist <- st_distance(fishnetCentroid,SchoolsPHL)%>% 
  st_sf()
schoolDistNet <- st_join(fishnet,MeanSChoolDist)
}
## Parks and Recreation Program Sites
## https://opendataphilly.org/datasets/parks-recreation-program-sites/
{
  PPRLocs <- st_read("https://opendata.arcgis.com/api/v3/datasets/9eb26a787a6e448ba426eea7f9f0d93a_0/downloads/data?format=geojson&spatialRefId=4326")%>%
    st_transform(crs=2272)
}

## Grocery info
## https://opendataphilly.org/datasets/neighborhood-food-retail/
{ 
  GroceryInfo <- st_read("https://opendata.arcgis.com/datasets/53b8a1c653a74c92b2de23a5d7bf04a0_0.geojson")%>%
    st_transform(crs=2272)
}

## Bike Network
## https://opendataphilly.org/datasets/bike-network/
## can we set this up to be distance based (similar as )
{
  BikeNet <- st_read("https://opendata.arcgis.com/datasets/b5f660b9f0f44ced915995b6d49f6385_0.geojson")%>%
    st_transform(crs=2272)
}



## Historic District (Assume negative relationship)
{
  historicDist <- st_read("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+historicdistricts_local&filename=historicdistricts_local&format=geojson&skipfields=cartodb_id")%>%
    st_transform(crs=2272)
}
# {
#   Treenet <- dplyr::select(PHLtrees) %>% 
#     mutate(countTree = 1) %>% 
#     aggregate(., fishnet, sum) %>%
#     mutate(countTree = replace_na(countTree, 0),
#            uniqueID = 1:n(),
#            #cvID = sample(round(nrow(fishnet) / 24)
#     )
#   #)
# }

# MAKE
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
root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
  
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
  
options(scipen=999)
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
# {
#   #philly permit data
#   dat_permit <- st_read("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+permits&filename=permits&format=geojson&skipfields=cartodb_id")
#   # 
#   # # only keep new construction permits
#   newcon_permits <- dat_permit %>% 
#     filter(grepl("NEW CON|NEWCON",typeofwork)) %>% 
#     st_transform('ESRI:102728')
#   
#   # write out smaller geojson file
#   st_write(newcon_permits,"C:/Users/benja/Documents/GitHub/MUSA5080/Assignments/HW07-Final")
#   
#   rm(dat_permit)
#   
#   newcon_permits <- st_read("C:/Users/benja/Documents/GitHub/MUSA5080/Assignments/HW07-Finaldata/newcon_permits.geojson")
#   
#   # OPA housing permit data
#     # missing
#   }
#   
# 

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
  mutate(value = replace_na(countPermit, 0),
                legend = "Permit Count",
         UniqueID = 1:n()
         )%>%
           dplyr::select(legend, UniqueID, value, geometry)
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
      mutate(value = replace_na(countTree, 0),
             UniqueID = 1:n(),
             legend = "Tree Network")%>%
    dplyr::select(legend, UniqueID, value, geometry)
}
## School Reading
{
  SchoolsPHL <- st_read("https://opendata.arcgis.com/datasets/d46a7e59e2c246c891fbee778759717e_0.geojson")%>%
    st_transform(crs=2272)
}
{
    schoolDistNet <- fishnet %>%
    mutate(
      value = nn_function(st_coordinates(st_centroid(fishnet)),  st_coordinates(SchoolsPHL), k = 3),
      UniqueID = 1:n(),
      legend = "3NN School")%>%
        dplyr::select(legend, UniqueID, value, geometry)
      
      
}
# {
#   SchoolDistNH <- schoolDistNet%>%
#   st_drop_geometry() %>%
#   group_by(name) %>%
#   summarise(avg_schoolDist = mean((schools1nn))) %>%
#   ungroup() %>%
#   dplyr::select(name,avg_schoolDist)%>%
#   left_join(phl.nh) %>%
#   st_as_sf()
# }
# {
#   schoolDistNet <-
#   fishnet %>%
#   mutate(
#     MeanSchoolDist = mean(st_distance(st_coordinates(st_centroid(fishnet)), st_coordinates(SchoolsPHL)))is.na()
#   )
#   }
# {
# fishnetCentroid <- st_centroid(fishnet)
# MeanSChoolDist <- st_distance(fishnetCentroid,SchoolsPHL)%>% 
#   st_sf()
# schoolDistNet <- st_join(fishnet,MeanSChoolDist)
# }
## Parks and Recreation Program Sites
## https://opendataphilly.org/datasets/parks-recreation-program-sites/
{
  PPRLocs <- st_read("https://opendata.arcgis.com/api/v3/datasets/9eb26a787a6e448ba426eea7f9f0d93a_0/downloads/data?format=geojson&spatialRefId=4326")%>%
    st_transform(crs=2272)
}
{
  ParkDistNet <- fishnet %>%
    mutate(
      value = nn_function(st_coordinates(st_centroid(fishnet)),  st_coordinates(PPRLocs), k = 3),
      UniqueID = 1:n(),
      legend = "3NN PPR Locations")%>%
    dplyr::select(legend, UniqueID, value, geometry)
}

## Grocery info
## https://opendataphilly.org/datasets/neighborhood-food-retail/
# BEN: CHANGE THIS --> join the centroids of the grocery net to the fishnet, not the other way around. ALso make another variable for the total restaurants, and one for the low/no access va. moderate & high access (HPSS_ACCESS)
{ 
  GroceryInfo <- st_read("https://opendata.arcgis.com/datasets/53b8a1c653a74c92b2de23a5d7bf04a0_0.geojson")%>%
    st_transform(crs=2272)
    #dplyr::select(TOTAL_HPSS)
}
{
ggplot()+
    geom_sf(data = phl.nh)+
    geom_sf(data=GroceryInfo, aes(fill = HPSS_ACCESS))
}


{
groceryNet <- st_join(st_centroid(fishnet),GroceryInfo)%>%
    mutate(
      value = replace_na(TOTAL_HPSS, 0),
      UniqueID = 1:n(),
      legend = "Total High Produce Stores")%>%
    dplyr::select(legend, UniqueID, value, geometry)
groceryNet <- groceryNet%>%
  st_drop_geometry()%>%
  left_join(fishnet,groceryNet,by = "UniqueID")%>%
  st_sf()
  dplyr::select(legend, UniqueID, value, geometry)
}
#still need to convert back to fishnets

## Bike Network
## https://opendataphilly.org/datasets/bike-network/
## can we set this up to be distance based (similar as )
{
  BikeData <- st_read("https://opendata.arcgis.com/datasets/b5f660b9f0f44ced915995b6d49f6385_0.geojson")%>%
    st_transform(crs=2272)%>%
    mutate(innetwork = 1)%>%
    dplyr::select(innetwork)
}
{
  BikeNet <- st_join(fishnet,BikeData)%>%
    mutate(
      value = ifelse(innetwork == 1, 1,0),
           legend = "Bike Network")%>%
    select(legend, UniqueID, value, geometry)%>%
    replace_na(list(value = 0))
}
{ggplot()+
    geom_sf(data=phl.nh)+
    geom_sf(data=BikeNet, aes(fill=value))}

## Historic District (Assume negative relationship)
{
  historicDist <- st_read("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+historicdistricts_local&filename=historicdistricts_local&format=geojson&skipfields=cartodb_id")%>%
    st_transform(crs=2272)%>%
    dplyr::select(name)
}
{
  historicNet <- st_join(st_centroid(fishnet),historicDist)%>%
    mutate(
      value = replace_na(name, "none"),
      legend = "Historic Districts")%>%
    select(legend, UniqueID, value, geometry)
  historicNet <- historicNet%>%
    st_drop_geometry()%>%
    left_join(fishnet,historicNet,by = "UniqueID")%>%
    st_sf()
  dplyr::select(legend, UniqueID, value, geometry)
}

# MAKE final net
{
final_net <- rbind(BikeNet,groceryNet,historicNet, ParkDistNet, schoolDistNet,Treenet, permitnet)
final_net2 <- gather(st_drop_geometry(final_net), key = "UniqueID", value = "legend")
  }
# Weighting
{
  final_net.nb <- poly2nb(as_Spatial(permit), queen=TRUE)
  ## ... and neighborhoods to list of weigths
  final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE)
}
# Moran's I
{
local_morans <- localmoran(permitnet, final_net.weights, zero.policy=TRUE) %>% 
  as.data.frame()
}

# join local Moran's I results to fishnet
final_net.localMorans <- 
  cbind(local_morans, as.data.frame(final_net)) %>% 
  st_sf() %>%
  dplyr::select(Permit_Count = legend, 
                Local_Morans_I = Ii, 
                P_Value = `Pr(z != E(Ii))`) %>%
  mutate(Significant_Hotspots = ifelse(P_Value <= 0.001, 1, 0)) %>%
  gather(Variable, Value, -geometry)



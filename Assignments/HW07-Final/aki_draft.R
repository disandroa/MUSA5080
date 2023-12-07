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
  library(viridis)
  library(FNN)
  library(ggplot2)
  library(ggcorrplot)
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
  dat_permit <- st_read("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+permits&filename=permits&format=geojson&skipfields=cartodb_id")
  # 
  # # only keep new construction permits
  newcon_permits <- dat_permit %>%
    filter(grepl("NEW CON|NEWCON",typeofwork)) %>%
    st_transform(crs = 2272)
  
  # write out smaller geojson file
  # st_write(newcon_permits,"Assignments/HW07-Final/data/newcon_permits.geojson")
  
  # rm(dat_permit)
  
  # newcon_permits <- st_read("Assignments/HW07-Final/data/newcon_permits.geojson") %>% 
  #   st_transform(crs = 2272)
  # for some reason, points are coming out as empty from this geojson
  # this timee it worked?? idk what's going on
  
  # census data
  # variables of interest:
  # B19013_001 - medHHincome
  # B25026_001 - total pop 
  # B02001_002 - white pop
  # med home value (if possible)
  # B25058_001 - median rent
  # B15003_022 - attainment of bachelor's of population 25+
  acs_variable_list.2021 <- load_variables(2021, #year
                                           "acs5", #five year ACS estimates
                                           cache = TRUE)
  
  census_vars <- c("B25026_001E","B15003_022E","B19013_001E","B02001_002E","B02001_002E","B25058_001E") # census variables of interest 
  tracts21 <- 
    get_acs(geography = "tract", 
            variables = census_vars, 
            year = 2021, state = 42,
            geometry = T, output = "wide") %>%
    st_transform(crs = 2272) %>%
    dplyr::select(!matches("M$")) %>% 
    rename(total_pop = B25026_001E, 
           bachelors25 = B15003_022E, 
           white_pop = B02001_002E, 
           medHHinc = B19013_001E, 
           med_rent = B25058_001E) %>%
    mutate(pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
           year = "2021")
  
  # philly neighborhood data
  nhoods_path <- 'https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson'
  nhoods <- st_read(nhoods_path, quiet = T) %>%
    st_transform(crs = 2272) %>%
    dplyr::select(mapname)
  
  # philly bounds
  philly <- st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson") %>%
    st_transform(crs = 2272) %>% 
    dplyr::select(OBJECTID,geometry)
  
  # vacant lots/buildings
  vacant_centroids <- st_read("https://opendata.arcgis.com/datasets/b990222a527849229b4192feb4c42dc0_0.geojson") %>% 
    st_transform(crs = 2272) %>% 
    st_centroid() %>%
    mutate(Legend = "Vacants") %>%
    dplyr::select(Legend)
  
  # parks & rec
  ppr_sites <- st_read("https://opendata.arcgis.com/api/v3/datasets/9eb26a787a6e448ba426eea7f9f0d93a_0/downloads/data?format=geojson&spatialRefId=4326") %>% 
    st_transform(crs = 2272) %>% 
    mutate(Legend = "Parks and Rec") %>%
    dplyr::select(Legend)
  
  # transit stops
  el <- st_read("https://opendata.arcgis.com/datasets/8c6e2575c8ad46eb887e6bb35825e1a6_0.geojson") %>% 
    st_transform(crs = 2272)
  bsl <- st_read("https://opendata.arcgis.com/datasets/2e9037fd5bef406488ffe5bb67d21312_0.geojson") %>% 
    st_transform(crs = 2272)
  
  septaStops <-
    rbind(
      el %>%
        mutate(Line = "El") %>%
        dplyr::select(Station, Line),
      bsl %>%
        mutate(Line ="Broad_St") %>%
        dplyr::select(Station, Line)) %>%
    st_transform(crs = 2272) %>%
    mutate(Legend = "Subway Stops") %>%
    dplyr::select(Legend)
  
  # proximity to CBD using city_hall as proxy
  city_hall <- bsl %>%
    st_transform(crs = 2272) %>%
    filter(Station=="City Hall") %>%
    mutate(Legend = "City Hall") %>%
    dplyr::select(Legend)
  
  # schools
  schools <- st_read('https://opendata.arcgis.com/datasets/d46a7e59e2c246c891fbee778759717e_0.geojson') %>% 
    st_transform(crs = 2272) %>% 
    mutate(Legend = "Schools") %>% 
    dplyr::select(Legend)
  
  # trees
  trees <- st_read("https://opendata.arcgis.com/api/v3/datasets/30ef36e9e880468fa74e2d5b18da4cfb_0/downloads/data?format=geojson&spatialRefId=4326") %>%
    st_transform(crs = 2272) %>% 
    mutate(Legend = "Trees") %>% 
    dplyr::select(Legend)
  
  # grocery stores (last updated 2019)
  # this data is already in a count per block group format
  groshies <- st_read("https://opendata.arcgis.com/datasets/53b8a1c653a74c92b2de23a5d7bf04a0_0.geojson") %>%
    st_transform(crs = 2272) #%>%
    # dplyr::select(TOTAL_HPSS,TOTAL_RESTAURANTS)
  # TOTAL_HPSS = Total number of high-produce supply stores within a half mile walking distance of the block group
  
  # bike info
  # use as count or binary data (does this square in the fishnet have a street that is bike-accessible in it?)
  bikes <- st_read("https://opendata.arcgis.com/datasets/b5f660b9f0f44ced915995b6d49f6385_0.geojson") %>%
    st_transform(crs = 2272) %>%
    mutate(Legend = "Bike Network") #%>%
    # dplyr::select(Legend)
  
  # historic districts
  # use as binary (is the centroid of the fishnet square in one of these historic districts?)
  historicDist <- st_read("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+historicdistricts_local&filename=historicdistricts_local&format=geojson&skipfields=cartodb_id") %>%
    st_transform(crs = 2272) %>%
    dplyr::select(name)
}

# Create Fishnet
{
  # our CRS is in feet, but want to make fishnet a 500m x 500m
  cell_size <- 500 * 3.28084
  
  fishnet <- 
    st_make_grid(philly,
                 cellsize = cell_size,  
                 square = TRUE,
                 crs = 2272) %>% 
    .[philly] %>%
    st_sf() %>%              
    mutate(uniqueID = 1:n()) 
  
  permit_net <- 
    dplyr::select(newcon_permits) %>% 
    mutate(count_permits = 1) %>% 
    aggregate(., fishnet, sum) %>%
    mutate(count_permits = replace_na(count_permits, 0),
           uniqueID = as.numeric(rownames(.)),
           cvID = sample(round(nrow(fishnet) / 16), size=nrow(fishnet), replace = TRUE))
  
  ggplot() +
    geom_sf(data = permit_net, aes(fill = count_permits)) +
    scale_fill_viridis() +
    labs(title = "Count of New Construction Permits for the fishnet",
         caption = "Figure x.") +
    mapTheme()
}

# Add other predictors to Fishnet
{
  # most vars can be added easily
  # special cases: groshies,bikes,historicDist
  vars_net <- rbind(vacant_centroids, ppr_sites, septaStops, city_hall, schools, trees) %>%
    st_join(fishnet, join=st_within) %>%  # if the point from abandonCars is in the fishnet, assign the unique ID to that point
    st_drop_geometry() %>%
    group_by(uniqueID, Legend) %>%
    summarize(count = n()) %>%
    left_join(fishnet, ., by = "uniqueID") %>%  # add geometry back in
    spread(Legend, count, fill=0) %>%  # fill in ones where fishnet was missing, count was NA with 0
    dplyr::select(-`<NA>`) %>%
    ungroup()
  
  # use something like code below (from lab 6) to create nearest neighbor counts
  # convenience to reduce length of function names.
  st_c    <- st_coordinates
  st_coid <- st_centroid
  
  vars_net_ccoid <- st_c(st_coid(vars_net))
  
  ## create NN from abandoned cars
  vars_net <- vars_net %>%
    mutate(vacant_centroids.nn = nn_function(vars_net_ccoid, st_c(vacant_centroids), 8),
           ppr_sites.nn = nn_function(vars_net_ccoid, st_c(ppr_sites), 3),
           septa_stops.nn = nn_function(vars_net_ccoid, st_c(septaStops), 2),
           city_hall.nn = nn_function(vars_net_ccoid, st_c(city_hall), 1),
           schools.nn = nn_function(vars_net_ccoid, st_c(schools), 8),
           trees.nn = nn_function(vars_net_ccoid, st_c(trees), 8)) 
  
  # think about how to add binary variables 
  vars_net <- vars_net %>% 
    mutate(in_historic = ifelse())
  # groceries, get centroids of block groups and map that on to the fishnet
  # from groceries, get HPSS_ACCESS, TOTAL_RESTAURANTS, TOTAL_HPSS
  # historic - binary variable 
  # bike network - if bike network ran through 
  
  all_net <-
    left_join(permit_net, st_drop_geometry(vars_net), by="uniqueID") 
  
  
  # make a subset of the net with variables we're actually interested in putting in the model
  subset_net <- all_net %>% 
    dplyr::select(countVand,uniqueID,cvID,Arson,`Theft from Vehicle`,vacant_centroids.nn,dui.nn,
                  thefts.nn,disorderly.nn) %>% 
    rename(Thefts_from_Vehicle = `Theft from Vehicle`,
           vacant_lots_buildings.nn = vacant_centroids.nn,
           DUI.nn = dui.nn,
           Thefts.nn = thefts.nn,
           Disorderly_conduct.nn = disorderly.nn) %>% 
    st_centroid() %>%
    st_join(dplyr::select(nhoods, mapname)) %>%
    st_drop_geometry() %>%
    left_join(dplyr::select(all_net, geometry, uniqueID), by = "uniqueID") %>%
    st_sf() %>%
    na.omit()
}

# correlation matrix
{
  # making this moreso to see which would actually be good predictors
  for_cormat <- all_net %>% 
    st_drop_geometry() %>% 
    # dplyr::select(countVand,Arson,arson.nn,`City Hall`,city_hall.nn,`Disorderly Conduct`,disorderly.nn, 
    #               `DRIVING UNDER THE INFLUENCE`,dui.nn,`Parks and Rec`,ppr_sites.nn,`Public Drunkenness`,
    #               public_drunk.nn,Schools,schools.nn,`Subway Stops`,septa_stops.nn,`Theft from Vehicle`,
    #               theft_from_veh.nn,Thefts,thefts.nn,Vacants,vacant_centroids.nn)
    dplyr::select(-c(uniqueID,cvID))
  
  ggcorrplot(
    round(cor(for_cormat), 1), 
    # method = "circle",
    p.mat = cor_pmat(for_cormat),
    colors = c("#4b2875", "white", "#9c1339"),
    type="lower",
    insig = "blank",
    digits = 4,
    lab = T, lab_size = 2) +  
    labs(title = "Correlation",
         caption = "Figure x.") 
  
  # strongest pred to keep: 
  # 
}

# Data Exploration
{
  
}




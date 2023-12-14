# Data Exploration for Final project (MUSA5080)

# Akira Di Sandro, 11/28/23

# load packages ----
{
  knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
  library(sf)
  library(tidyverse)
  library(tidycensus)
  library(viridis)
  library(gridExtra)
  library(ggpubr)
  library(scales)
  library(ggcorrplot)
  library(spdep)
  library(FNN)
  library(kableExtra)
  library(spatstat.explore)
  library(raster)
  library(classInt)
  #devtools::install_github("CityOfPhiladelphia/rphl")
  library(rphl)
  library(stringr)
  library(lubridate)
  
  set.seed(172)
  
  setwd("~/Documents/MUSA5080")
  
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
  #   mutate(year = substr(permitissuedate, 1,4)) %>% 
  #   filter(year %in% c(2010:2019,2022)) %>% 
  #   st_transform(crs = 2272)
  
  # TODO:
  # need to further limit this to permits between 2010-2019, to then predict 2022 data
  
  # write out smaller geojson file
  # st_write(newcon_permits,"Assignments/HW07-Final/data/newcon_permits.geojson")
  
  # rm(dat_permit)
  
  newcon_permits <- st_read("Assignments/HW07-Final/data/newcon_permits.geojson")
  
  # census data
  {
    acs_variable_list.2019 <- load_variables(2019, #year
                                             "acs5", #five year ACS estimates
                                             cache = TRUE) %>% 
      filter(geography == "block group")
    
    # variables of interest:
    # B19013_001 - medHHincome
    # B25026_001 - total pop 
    # B02001_002 - white pop
    # B25058_001 - median rent
    # B15003_022 - attainment of bachelor's of population 25+
    # B25071_001: Median gross rent as a percentage of household income (past year)
    # B07010_001: Geographical Mobility in the Past Year by Individual Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) for Current Residence in the United States
    # B07201_001: Geographical Mobility in the Past Year for Current Residence--Metropolitan Statistical Area Level in the United States, estimated total
    # B07201_002: Geographical Mobility in the Past Year for Current Residence--Metropolitan Statistical Area Level in the United States, same house 1 year ago 
    # B07202_001: Geographical Mobility in the Past Year for Current Residence--Micropolitan Statistical Area Level in the United States, estimatedd total
    # B07202_002: Geographical Mobility in the Past Year for Current Residence--Micropolitan Statistical Area Level in the United States, same house 1 year ago
    # B07204_006: Geographical Mobility in the Past Year for Current Residence--State, County and Place Level in the United States, same city, different county as 1 year ago
    # B07401_049: living in area 1 year ago, moved to dif county within same state
    # B07401_050: living in area 1 year ago, moved to dif county within same state 1-4 years
    # B25003_002: tenure, owner occupied
    # B25003_003: tenure, renter occupied
    # B25008_002: total population in occupied housing by tenure, owner occupied
    # B25008_003: total population in occupied housing by tenure, renter occupied
    # B99082_001: allocation of private vehicle occupance
    # B992512_001: allocation of vehicles available
    # B25044_001: tenure by vehicles available
    # B25046_001: aggregate number of vehicles available
    # B09002_001: own children under 18
    # B11004_001: related children under 18
    # B17010_002: Income in the past 12 months below poverty level
    
    census_vars <- c("B01001_001E","B15003_022E","B19013_001E","B02001_002E","B25058_001E",
                     "B25071_001E","B07201_001E","B07201_002E","B07202_001E","B07202_002E",
                     "B25003_002E","B25003_003E","B25008_002E","B25008_003E","B99082_001E",
                     "B992512_001E","B25044_001E","B25046_001E","B09002_001E","B11004_001E","B17010_002E") # census variables of interest that are available
    
    medHHinc2013 <- 78986 # source: https://www.deptofnumbers.com/income/pennsylvania/philadelphia/
    medHHinc2014 <- 76334
    medHHinc2015 <- 75790
    medHHinc2016 <- 74512
    medHHinc2017 <- 74474
    medHHinc2018 <- 71221
    medHHinc2019 <- 70459
    
    tracts13 <- 
      get_acs(geography = "block group", 
              variables = census_vars, 
              year = 2013, state = 42,
              geometry = T, output = "wide") %>%
      st_transform(crs = 2272) %>%
      dplyr::select(!matches("M$")) %>% 
      rename(total_pop = B01001_001E,
             bachelors25 = B15003_022E,
             med_hh_inc = B19013_001E,
             white_pop = B02001_002E,
             med_rent = B25058_001E,
             pct_rent_hhinc = B25071_001E,
             mobility_tot_metro = B07201_001E,
             samehouse1yr_metro = B07201_002E,
             mobility_tot_micro = B07202_001E,
             samehouse1yr_micro = B07202_002E,
             owner_occ1 = B25003_002E,
             renter_occ1 = B25003_003E,
             owner_occ2 = B25008_002E,
             renter_occ2 = B25008_003E,
             private_vehicle_occ = B99082_001E,
             vehicles_avail1 = B992512_001E,
             vehicles_avail2 = B25044_001E,
             vehicles_avail3 = B25046_001E,
             below_poverty = B17010_002E,
      ) %>%
      mutate(children = B09002_001E + B11004_001E,
             pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
             RaceContext = ifelse(pct_white > 0.5, "Majority White", 
                                  ifelse(total_pop != 0 , "Majority non-White", NA)),
             pct_children = ifelse(total_pop > 0 & !is.na(children), children / total_pop,0), # percent of households with children present
             year = "2013") %>% 
      dplyr::select(!matches("^B")) %>% 
      st_centroid()
    
    tracts14 <- 
      get_acs(geography = "block group", 
              variables = census_vars, 
              year = 2014, state = 42,
              geometry = T, output = "wide") %>%
      st_transform(crs = 2272) %>%
      dplyr::select(!matches("M$")) %>% 
      rename(total_pop = B01001_001E,
             bachelors25 = B15003_022E,
             med_hh_inc = B19013_001E,
             white_pop = B02001_002E,
             med_rent = B25058_001E,
             pct_rent_hhinc = B25071_001E,
             mobility_tot_metro = B07201_001E,
             samehouse1yr_metro = B07201_002E,
             mobility_tot_micro = B07202_001E,
             samehouse1yr_micro = B07202_002E,
             owner_occ1 = B25003_002E,
             renter_occ1 = B25003_003E,
             owner_occ2 = B25008_002E,
             renter_occ2 = B25008_003E,
             private_vehicle_occ = B99082_001E,
             vehicles_avail1 = B992512_001E,
             vehicles_avail2 = B25044_001E,
             vehicles_avail3 = B25046_001E,
             below_poverty = B17010_002E,
      ) %>%
      mutate(children = B09002_001E + B11004_001E,
             pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
             RaceContext = ifelse(pct_white > 0.5, "Majority White", 
                                  ifelse(total_pop != 0 , "Majority non-White", NA)),
             pct_children = ifelse(total_pop > 0 & !is.na(children), children / total_pop,0), # percent of households with children present
             year = "2014") %>% 
      dplyr::select(!matches("^B"))
    
    tracts15 <- 
      get_acs(geography = "block group", 
              variables = census_vars, 
              year = 2015, state = 42,
              geometry = T, output = "wide") %>%
      st_transform(crs = 2272) %>%
      dplyr::select(!matches("M$")) %>% 
      rename(total_pop = B01001_001E,
             bachelors25 = B15003_022E,
             med_hh_inc = B19013_001E,
             white_pop = B02001_002E,
             med_rent = B25058_001E,
             pct_rent_hhinc = B25071_001E,
             mobility_tot_metro = B07201_001E,
             samehouse1yr_metro = B07201_002E,
             mobility_tot_micro = B07202_001E,
             samehouse1yr_micro = B07202_002E,
             owner_occ1 = B25003_002E,
             renter_occ1 = B25003_003E,
             owner_occ2 = B25008_002E,
             renter_occ2 = B25008_003E,
             private_vehicle_occ = B99082_001E,
             vehicles_avail1 = B992512_001E,
             vehicles_avail2 = B25044_001E,
             vehicles_avail3 = B25046_001E,
             below_poverty = B17010_002E,
      ) %>%
      mutate(children = B09002_001E + B11004_001E,
             pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
             RaceContext = ifelse(pct_white > 0.5, "Majority White", 
                                  ifelse(total_pop != 0 , "Majority non-White", NA)),
             pct_children = ifelse(total_pop > 0 & !is.na(children), children / total_pop,0), # percent of households with children present
             year = "2015") %>% 
      dplyr::select(!matches("^B"))
    
    tracts16 <- 
      get_acs(geography = "block group", 
              variables = census_vars, 
              year = 2016, state = 42,
              geometry = T, output = "wide") %>%
      st_transform(crs = 2272) %>%
      dplyr::select(!matches("M$")) %>% 
      rename(total_pop = B01001_001E,
             bachelors25 = B15003_022E,
             med_hh_inc = B19013_001E,
             white_pop = B02001_002E,
             med_rent = B25058_001E,
             pct_rent_hhinc = B25071_001E,
             mobility_tot_metro = B07201_001E,
             samehouse1yr_metro = B07201_002E,
             mobility_tot_micro = B07202_001E,
             samehouse1yr_micro = B07202_002E,
             owner_occ1 = B25003_002E,
             renter_occ1 = B25003_003E,
             owner_occ2 = B25008_002E,
             renter_occ2 = B25008_003E,
             private_vehicle_occ = B99082_001E,
             vehicles_avail1 = B992512_001E,
             vehicles_avail2 = B25044_001E,
             vehicles_avail3 = B25046_001E,
             below_poverty = B17010_002E,
      ) %>%
      mutate(children = B09002_001E + B11004_001E,
             pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
             RaceContext = ifelse(pct_white > 0.5, "Majority White", 
                                  ifelse(total_pop != 0 , "Majority non-White", NA)),
             pct_children = ifelse(total_pop > 0 & !is.na(children), children / total_pop,0), # percent of households with children present
             year = "2016") %>% 
      dplyr::select(!matches("^B"))
    
    tracts17 <- 
      get_acs(geography = "block group", 
              variables = census_vars, 
              year = 2017, state = 42,
              geometry = T, output = "wide") %>%
      st_transform(crs = 2272) %>%
      dplyr::select(!matches("M$")) %>% 
      rename(total_pop = B01001_001E,
             bachelors25 = B15003_022E,
             med_hh_inc = B19013_001E,
             white_pop = B02001_002E,
             med_rent = B25058_001E,
             pct_rent_hhinc = B25071_001E,
             mobility_tot_metro = B07201_001E,
             samehouse1yr_metro = B07201_002E,
             mobility_tot_micro = B07202_001E,
             samehouse1yr_micro = B07202_002E,
             owner_occ1 = B25003_002E,
             renter_occ1 = B25003_003E,
             owner_occ2 = B25008_002E,
             renter_occ2 = B25008_003E,
             private_vehicle_occ = B99082_001E,
             vehicles_avail1 = B992512_001E,
             vehicles_avail2 = B25044_001E,
             vehicles_avail3 = B25046_001E,
             below_poverty = B17010_002E,
      ) %>%
      mutate(children = B09002_001E + B11004_001E,
             pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
             RaceContext = ifelse(pct_white > 0.5, "Majority White", 
                                  ifelse(total_pop != 0 , "Majority non-White", NA)),
             pct_children = ifelse(total_pop > 0 & !is.na(children), children / total_pop,0), # percent of households with children present
             year = "2017") %>% 
      dplyr::select(!matches("^B"))
    
    tracts18 <- 
      get_acs(geography = "block group", 
              variables = census_vars, 
              year = 2018, state = 42,
              geometry = T, output = "wide") %>%
      st_transform(crs = 2272) %>%
      dplyr::select(!matches("M$")) %>% 
      rename(total_pop = B01001_001E,
             bachelors25 = B15003_022E,
             med_hh_inc = B19013_001E,
             white_pop = B02001_002E,
             med_rent = B25058_001E,
             pct_rent_hhinc = B25071_001E,
             mobility_tot_metro = B07201_001E,
             samehouse1yr_metro = B07201_002E,
             mobility_tot_micro = B07202_001E,
             samehouse1yr_micro = B07202_002E,
             owner_occ1 = B25003_002E,
             renter_occ1 = B25003_003E,
             owner_occ2 = B25008_002E,
             renter_occ2 = B25008_003E,
             private_vehicle_occ = B99082_001E,
             vehicles_avail1 = B992512_001E,
             vehicles_avail2 = B25044_001E,
             vehicles_avail3 = B25046_001E,
             below_poverty = B17010_002E,
      ) %>%
      mutate(children = B09002_001E + B11004_001E,
             pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
             RaceContext = ifelse(pct_white > 0.5, "Majority White", 
                                  ifelse(total_pop != 0 , "Majority non-White", NA)),
             pct_children = ifelse(total_pop > 0 & !is.na(children), children / total_pop,0), # percent of households with children present
             year = "2018") %>% 
      dplyr::select(!matches("^B"))
    
    tracts19 <- 
      get_acs(geography = "block group", 
              variables = census_vars, 
              year = 2019, state = 42,
              geometry = T, output = "wide") %>%
      st_transform(crs = 2272) %>%
      dplyr::select(!matches("M$")) %>% 
      rename(total_pop = B01001_001E,
             bachelors25 = B15003_022E,
             med_hh_inc = B19013_001E,
             white_pop = B02001_002E,
             med_rent = B25058_001E,
             pct_rent_hhinc = B25071_001E,
             mobility_tot_metro = B07201_001E,
             samehouse1yr_metro = B07201_002E,
             mobility_tot_micro = B07202_001E,
             samehouse1yr_micro = B07202_002E,
             owner_occ1 = B25003_002E,
             renter_occ1 = B25003_003E,
             owner_occ2 = B25008_002E,
             renter_occ2 = B25008_003E,
             private_vehicle_occ = B99082_001E,
             vehicles_avail1 = B992512_001E,
             vehicles_avail2 = B25044_001E,
             vehicles_avail3 = B25046_001E,
             below_poverty = B17010_002E,
      ) %>%
      mutate(children = B09002_001E + B11004_001E,
             pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
             RaceContext = ifelse(pct_white > 0.5, "Majority White", 
                                  ifelse(total_pop != 0 , "Majority non-White", NA)),
             pct_children = ifelse(total_pop > 0 & !is.na(children), children / total_pop,0), # percent of households with children present
             year = "2019") %>% 
      dplyr::select(!matches("^B"))
    
  }
  
  # philly neighborhood data
  nhoods_path <- 'https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson'
  nhoods <- st_read(nhoods_path, quiet = T) %>%
    st_transform(crs = 2272) %>%
    dplyr::select(mapname)
  
  # philly bounds
  philly <- st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson") %>%
    st_transform(crs = 2272) %>% 
    dplyr::select(OBJECTID,geometry)
  
  # variables that we are assuming are constant/don't change enough/we don't have data for
  {
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
  
  # 311 incident data
  # Ben will figure this part out
  {
    incidents <- st_read("https://phl.carto.com/api/v2/sql?filename=incidents_part1_part2&format=shp&skipfields=cartodb_id&q=SELECT%20*%20FROM%20incidents_part1_part2%20WHERE%20dispatch_date_time%20%3E=%20%272022-01-01%27%20AND%20dispatch_date_time%20%3C%20%272023-01-01%27") %>% 
      st_transform(crs = 2272)
    
    incidents21 <- st_read("~/Documents/MUSA5080/Assignments/HW04/incidents_21/incidents_part1_part2.shp") %>% 
      st_transform('ESRI:102728') %>% rename(Legend = text_gener)
    
    # filter to only keep vandalism incidents
    vandalism <- incidents21 %>% filter(Legend == "Vandalism/Criminal Mischief") # 13670 rows
    
    # only keep what's inside the philly boundary
    vandalism <- vandalism[philly,] # 13578 rows
    
    # same for other risk factors: public drunkenness, arson, disorderly conduct, thefts, theft from vehicle, dui
    # also make feature for nearest neighbor count of these incidents
    public_drunk <- incidents21 %>% filter(Legend == "Public Drunkenness") %>% 
      dplyr::select(Legend,geometry)
    public_drunk <- public_drunk[philly,]
    
    arson <- incidents21 %>% filter(Legend == "Arson") %>% 
      dplyr::select(Legend,geometry)
    arson <- arson[philly,]
    
    disorderly <- incidents21 %>% filter(Legend == "Disorderly Conduct") %>% 
      dplyr::select(Legend,geometry)
    disorderly <- disorderly[philly,]
    
    thefts <- incidents21 %>% filter(Legend == "Thefts") %>% 
      dplyr::select(Legend,geometry)
    thefts <- thefts[philly,]
    
    theft_from_veh <- incidents21 %>% filter(Legend == "Theft from Vehicle") %>% 
      dplyr::select(Legend,geometry)
    theft_from_veh <- theft_from_veh[philly,]
    
    dui <- incidents21 %>% filter(Legend == "DRIVING UNDER THE INFLUENCE") %>% 
      dplyr::select(Legend,geometry)
    dui <- dui[philly,]
  }
  
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
  
  permit_net13 <- 
    dplyr::select(newcon_permits %>% filter(year == "2013")) %>% 
    mutate(count_permits = 1) %>% 
    aggregate(., fishnet, sum) %>%
    mutate(count_permits = replace_na(count_permits, 0),
           uniqueID = as.numeric(rownames(.)),
           cvID = sample(round(nrow(fishnet) / 16), size=nrow(fishnet), replace = TRUE))
  #this accomplishes the cross fold validation, we need to 
  
  permit_count_map <- ggplot() +
    geom_sf(data = permit_net, aes(fill = count_permits)) +
    scale_fill_viridis() +
    labs(title = "Count of New Construction Permits for the fishnet",
         caption = "Figure x.") +
    mapTheme()
  
  permit_count_map
}

# Adding census variables to fishnet
{
  # vars to fishnet
  {
    
  }
  
  # correlation of census variables to count permit
  {
    # making this moreso to see which would actually be good predictors
    for_cormat <- all_net %>% 
      st_drop_geometry() %>% 
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
    
  }
}

# Add other predictors to Fishnet
{
  # most vars can be added easily
  # special cases: groshies,bikes,historicDist
  vars_net <- rbind(vacant_centroids, ppr_sites, septaStops, city_hall, schools, trees) %>%
    st_join(fishnet, join=st_within) %>%
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
  # groceries, get centroids of block groups and map that on to the fishnet
  # from groceries, get HPSS_ACCESS (gave up on this one because it's categorical), TOTAL_RESTAURANTS, TOTAL_HPSS
  total_hpss <- st_join(groshies %>% dplyr::select(TOTAL_HPSS), fishnet) %>% 
    group_by(uniqueID) %>% 
    summarize(total_hpss = mean(TOTAL_HPSS,na.rm=T)) %>%  # take average because there might be some overlap if a fishnet encompasses centroids of multiple block groups.
    mutate(total_hpss = ifelse(is.nan(total_hpss), NA, total_hpss)) %>% 
    st_drop_geometry()
  
  total_restaurants <- st_join(groshies %>% dplyr::select(TOTAL_RESTAURANTS), fishnet) %>% 
    group_by(uniqueID) %>% 
    summarize(total_restaurants = sum(TOTAL_RESTAURANTS,na.rm=T)) %>% 
    st_drop_geometry()
  
  # historic - binary variable 
  # find fishnet squares that intersect with historicDistricts
  sqr_ishistoric <- st_intersection(fishnet,historicDist)$uniqueID # vector of uniqueIDs that intersect with a historic district
  
  # bike network - if bike network ran through 
  bike_net <- st_join(fishnet, bikes) %>%
    group_by(uniqueID) %>%
    summarise(objects = sum(OBJECTID)) %>% 
    mutate(in_bike_net = ifelse(!is.na(objects), 1,0)) %>% 
    dplyr::select(uniqueID, in_bike_net) %>%
    st_drop_geometry()
  
  # checking the bike_net code
  # ggplot() + 
  #   geom_sf(data = fishnet) +
  #   geom_sf(data = bikes, aes(color = CLASS)) + scale_color_gradient(low = "yellow",high = "red")
  
  # vars_net with groshies, historic, bike_net added
  vars_net1 <- cbind(vars_net, 
                     total_hpss %>% dplyr::select(-uniqueID), 
                     total_restaurants %>% dplyr::select(-uniqueID), 
                     bike_net %>% dplyr::select(-uniqueID)) %>% 
    mutate(is_historic = ifelse(uniqueID %in% sqr_ishistoric, 1, 0),
           total_hpss = ifelse(is.na(total_hpss), 0, total_hpss))
  
}

# all variables together
{
  all_net <- left_join(permit_net, st_drop_geometry(vars_net1), by="uniqueID")
  
  # make a subset of the net with variables we're actually interested in putting in the model
  # add neighborhood names to data
  subset_net <- all_net %>%
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
  
}

# Data Exploration ----
{
  # all predictors
  {
    vars_net.long <- gather(subset_net %>% dplyr::select(-count_permits),
                            variable, value, -geometry, -uniqueID, -cvID, -mapname)
    
    vars <- unique(vars_net.long$variable)
    varList <- list()
    
    for (i in vars) {
      varList[[i]] <- ggplot() +
        geom_sf(data = filter(vars_net.long, variable == i),aes(fill = value), colour=NA) +
        scale_fill_viridis(name = "") +
        labs(title = i) +
        mapTheme(title_size = 14) + theme(legend.position="bottom")
    }
    
    do.call(grid.arrange,c(varList, ncol = 3, top = "Predictors of Permit Count (on fishnet)", bottom = "Figure x."))
  }
  
  # spatial features
  {
    final_net.nb <- poly2nb(as_Spatial(subset_net), queen=TRUE)
    final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE) # turn neighborhood weights into list of weights
    
    local_morans <- localmoran(subset_net$count_permits, final_net.weights, zero.policy=TRUE) %>%
      as.data.frame() # Ii moran's I at ith cell, Ei expected/mean from neighbors
    
    # join local Moran's I results to fishnet
    final_net.localMorans <- 
      cbind(local_morans, as.data.frame(subset_net)) %>% 
      st_sf() %>%
      dplyr::select(`Permit Count` = count_permits, 
                    `Local Morans I` = Ii, 
                    `P Value` = `Pr(z != E(Ii))`) %>%
      mutate(`Significant Hotspots` = ifelse(`P Value` <= 0.001, 1, 0)) %>%
      gather(variable, value, -geometry)
    
    # now plot
    vars <- unique(final_net.localMorans$variable)
    varList <- list()
    
    for(i in vars){
      varList[[i]] <- 
        ggplot() +
        geom_sf(data = filter(final_net.localMorans, variable == i),aes(fill = value), colour=NA) +
        scale_fill_viridis(name="") +
        labs(title=i) +
        mapTheme(title_size = 14) + theme(legend.position="bottom")
    }
    
    do.call(grid.arrange,c(varList, ncol = 2, top = "Local Moran's I Statistics for Permit Count in Philadelphia", 
                           bottom = "Figure x."))
    
    final_net <-
      subset_net %>% 
      mutate(permitct.isSig = 
               ifelse(localmoran(subset_net$count_permits, 
                                 final_net.weights)[,5] <= 0.001, 1, 0)) %>%
      mutate(permitct.isSig.dist = 
               nn_function(st_coordinates(st_centroid(subset_net)),
                           st_coordinates(st_centroid(
                             filter(subset_net, permitct.isSig == 1))), 1))
    
  }
  
  # scatter plots of predictors
  {
    correlation.long <-
      st_drop_geometry(final_net) %>%
      dplyr::select(-uniqueID, -cvID, -mapname) %>%
      gather(variable, value, -count_permits)
    
    correlation.cor <-
      correlation.long %>%
      group_by(variable) %>%
      summarize(correlation = cor(value, count_permits, use = "complete.obs"))
    
    ggplot(correlation.long, aes(value, count_permits)) +
      geom_point(size = 0.1) +
      geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
                x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
      geom_smooth(method = "lm", se = FALSE, colour = "black") +
      facet_wrap(~variable, ncol = 4, scales = "free") +
      labs(title = "Permit count as a function of predictors",
           caption = "Figure x.") +
      plotTheme(title_size = 14)
  }
  
  # Poisson regression
  {
    final_net %>% ggplot(aes(x = count_permits)) +
      geom_histogram(bins = 66) +
      theme_minimal() +
      labs(title = "Permit Distribution",
           x = "Count of New Construction Permits", y = "Count",
           caption = "Figure x.")
  }
}

# Model ----
{
  # building model
  {
    # just risk factors
    reg.vars <- c("city_hall.nn", "in_bike_net", "is_historic", "ppr_sites.nn", "schools.nn", "septa_stops.nn", 
                  "total_hpss", "total_restaurants", "Trees", "Vacants")
    
    ## RUN REGRESSIONS
    reg.CV <- crossValidate(
      dataset = final_net,
      id = "cvID",
      dependentVariable = "count_permits",
      indVariables = reg.vars) %>%
      mutate(error = count_permits - Prediction)
    
    # MAE
    reg.MAE <- mean(abs(reg.CV$error)) # 45.08406
    
    
    # with local Moran's I spatial process features
    reg.sp.vars <- c("city_hall.nn", "in_bike_net", "is_historic", "ppr_sites.nn", "schools.nn", "septa_stops.nn", 
                     "total_hpss", "total_restaurants", "Trees", "Vacants","permitct.isSig", "permitct.isSig.dist")
    
    ## RUN REGRESSIONS
    reg.spatialCV <- crossValidate(
      dataset = final_net,
      id = "cvID",                           
      dependentVariable = "count_permits",
      indVariables = reg.sp.vars) %>% 
      dplyr::select(cvID, count_permits, Prediction, geometry) %>% 
      mutate(error = count_permits - Prediction)
    
    # MAE
    reg.spatial.MAE <- mean(abs(reg.spatialCV$error)) # 36.35607
    
    
    # adding neighborhood for LOGO CV, risk factors only
    reg.logoCV <- crossValidate(
      dataset = final_net,
      id = "mapname",
      dependentVariable = "count_permits",
      indVariables = reg.vars) %>%
      mutate(error = count_permits - Prediction)
    
    # MAE
    reg.logo.MAE <- mean(abs(reg.logoCV$error)) # 46.49673
    
    
    # adding neighborhood for LOGO CV, risk factors + spatial process
    reg.logo.spatialCV <- crossValidate(
      dataset = final_net,
      id = "mapname",                           
      dependentVariable = "count_permits",
      indVariables = reg.sp.vars) %>% 
      dplyr::select(cvID = mapname, count_permits, Prediction, geometry) %>% 
      mutate(error = count_permits - Prediction)
    
    # MAE
    reg.logo.spatial.MAE <- mean(abs(reg.logo.spatialCV$error)) # 37.12363
    
  }
  
  # map of MAE
  # TO DO: add observed count of permits as well
  {
    reg.summary <- rbind(
      mutate(reg.spatialCV,
             Error = Prediction - count_permits,
             Regression = "Random k-fold CV"),
      mutate(reg.logo.spatialCV, 
             Error = Prediction - count_permits,
             Regression = "Spatial LOGO-CV")) %>%
      st_sf() 
    
    error_by_reg_and_fold <- 
      reg.summary %>%
      group_by(Regression, cvID) %>% 
      summarize(Mean_Error = mean(Prediction - count_permits, na.rm = T),
                MAE        = mean(abs(Mean_Error), na.rm = T),
                SD_MAE     = mean(abs(Mean_Error), na.rm = T)) %>%
      ungroup()
    
    # make map
    error_by_reg_and_fold %>%
      ggplot() +
      geom_sf(aes(fill = MAE)) +
      facet_wrap(~Regression) +
      scale_fill_viridis() +
      labs(title = "Errors by Cross Validation method",
           caption = "Figure x.") +
      mapTheme(title_size = 14) + theme(legend.position="bottom")
  }
  
  # distribution of MAE
  {
    error_by_reg_and_fold %>%
      ggplot(aes(MAE)) + 
      geom_histogram(bins = 30, colour="black", fill = "#FDE725FF") +
      facet_wrap(~Regression) +  
      geom_vline(xintercept = 0) + scale_x_continuous(breaks = seq(0, 450, by = 50)) + 
      labs(title="Distribution of MAE", subtitle = "k-fold cross validation vs. LOGO-CV",
           x="Mean Absolute Error",     y="Count",
           caption = "Figure x.") +
      plotTheme(title_size = 14) + theme(legend.position="bottom")
  }
  
  # table of summary of regressions
  {
    st_drop_geometry(error_by_reg_and_fold) %>%
      group_by(Regression) %>% 
      summarize(`Mean MAE` = round(mean(MAE), 2),
                `SD MAE` = round(sd(MAE), 2)) %>%
      kable(caption = "Table 1: Summary of Regressions") %>%
      kable_styling("striped", full_width = F) %>% 
      kable_classic(full_width = F, html_font = "Cambria")
    
  }
  
  # mean error by neighborhood racial context
  {
    RaceContext <- tracts22 %>% 
      dplyr::select(GEOID,total_pop,RaceContext,geometry) %>% 
      .[nhoods,]
    
    reg.summary %>% 
      st_centroid() %>%
      st_join(tracts22 %>% dplyr::select(RaceContext, geometry)) %>%
      na.omit() %>%
      st_drop_geometry() %>%
      group_by(Regression, RaceContext) %>%
      summarize(mean.Error = mean(Error, na.rm = T)) %>%
      spread(RaceContext, mean.Error) %>%
      kable(caption = "Table 2. Mean Error by Neighborhood Racial Context") %>%
      kable_styling("striped", full_width = F) %>% 
      kable_classic(html_font = "Cambria")
  }
  
  # TO DO:
  # mean error by neighborhood income context
  {
    IncomeContext <- tracts22 %>% 
      dplyr::select(GEOID,total_pop,IncomeContext,geometry) %>% 
      .[nhoods,]
    
    reg.summary %>% 
      st_centroid() %>%
      st_join(tracts22 %>% dplyr::select(IncomeContext, geometry)) %>%
      na.omit() %>%
      st_drop_geometry() %>%
      group_by(Regression, IncomeContext) %>%
      summarize(mean.Error = mean(Error, na.rm = T)) %>%
      spread(IncomeContext, mean.Error) %>%
      kable(caption = "Table 2. Mean Error by Neighborhood Income Context") %>%
      kable_styling("striped", full_width = F) %>% 
      kable_classic(html_font = "Cambria")
  }
}

# generalizing over time
{
  # kernel density plot
  {
    # kernel density
    vand_ppp <- as.ppp(st_coordinates(vandalism), W = st_bbox(final_net))
    vand_KD.1000 <- spatstat.explore::density.ppp(vand_ppp, 1000)
    vand_KD.1500 <- spatstat.explore::density.ppp(vand_ppp, 1500)
    vand_KD.2000 <- spatstat.explore::density.ppp(vand_ppp, 2000)
    vand_KD.df <- rbind(
      mutate(data.frame(rasterToPoints(mask(raster(vand_KD.1000), as(nhoods, 'Spatial')))), Legend = "1000 Ft."),
      mutate(data.frame(rasterToPoints(mask(raster(vand_KD.1500), as(nhoods, 'Spatial')))), Legend = "1500 Ft."),
      mutate(data.frame(rasterToPoints(mask(raster(vand_KD.2000), as(nhoods, 'Spatial')))), Legend = "2000 Ft.")) 
    
    vand_KD.df$Legend <- factor(vand_KD.df$Legend, levels = c("1000 Ft.", "1500 Ft.", "2000 Ft."))
    
    # ggplot(data=vand_KD.df, aes(x=x, y=y)) +
    #   geom_raster(aes(fill=layer)) + 
    #   facet_wrap(~Legend) +
    #   coord_sf(crs=st_crs(final_net)) + 
    #   scale_fill_viridis(name="Density") +
    #   labs(title = "Kernel density with 3 different search radii") +
    #   mapTheme(title_size = 14)
    
    
    # as.data.frame(vand_KD.1000) %>%
    #   st_as_sf(coords = c("x", "y"), crs = st_crs(final_net)) %>%
    #   aggregate(., final_net, mean) %>%
    #    ggplot() +
    #      geom_sf(aes(fill=value)) +
    #      geom_sf(data = sample_n(vandalism, 1500), size = .5) +
    #      scale_fill_viridis(name = "Density") +
    #      labs(title = "Kernel density of 2021 Vandalism Incidents") +
    #      mapTheme(title_size = 14)
    
    
    # download data from 2022
    # incidents22 <- st_read("~/Documents/MUSA5080/Assignments/HW04/incidents_22/incidents_part1_part2.shp") %>% 
    #   st_transform('ESRI:102728') %>% rename(Legend = text_gener)
    # 
    
    carto_url = "https://phl.carto.com/api/v2/sql"
    
    # Crime incidents
    table_name = "incidents_part1_part2"
    
    # query
    where2 = "dispatch_date >= '2022-01-01' AND dispatch_date < '2023-01-01' AND text_general_code IN ('DRIVING UNDER THE INFLUENCE','Theft from Vehicle','Thefts','Disorderly Conduct','Public Drunkenness', 'Arson', 'Vandalism/Criminal Mischief')"
    
    query2 = paste("SELECT *",
                   "FROM", table_name,
                   "WHERE", where)
    
    incidents22 = rphl::get_carto(query2, format = "csv", base_url = carto_url, stringsAsFactors = F)%>%
      rename(Legend = text_general_code)
    incidents22 <- incidents22 %>%
      filter(!is.na(point_x) & !is.na(point_y)) %>%
      st_as_sf(coords = c("point_x","point_y"),crs=4326) %>%
      st_transform('ESRI:102728') %>%
      dplyr::select(Legend, geometry)
    
    
    
    
    # filter to only keep vandalism incidents
    vandalism22 <- incidents22 %>% filter(Legend == "Vandalism/Criminal Mischief") %>% 
      .[fishnet,]
    
    
    # from lab
    vand_KDE_sum <- as.data.frame(vand_KD.1000) %>%
      st_as_sf(coords = c("x", "y"), crs = st_crs(final_net)) %>%
      aggregate(., final_net, mean) 
    kde_breaks <- classIntervals(vand_KDE_sum$value, 
                                 n = 5, "fisher")
    vand_KDE_sf <- vand_KDE_sum %>%
      mutate(label = "Kernel Density",
             Risk_Category = classInt::findCols(kde_breaks),
             Risk_Category = case_when(
               Risk_Category == 5 ~ "5th",
               Risk_Category == 4 ~ "4th",
               Risk_Category == 3 ~ "3rd",
               Risk_Category == 2 ~ "2nd",
               Risk_Category == 1 ~ "1st")) %>%
      cbind(
        aggregate(
          dplyr::select(vandalism22) %>% mutate(vandCount = 1), ., sum) %>%
          mutate(vandCount = replace_na(vandCount, 0))) %>%
      dplyr::select(label, Risk_Category, vandCount)
    
    
    ml_breaks <- classIntervals(reg.spatialCV$Prediction, 
                                n = 5, "fisher")
    vand_risk_sf <-
      reg.spatialCV %>%
      mutate(label = "Risk Predictions",
             Risk_Category =classInt::findCols(ml_breaks),
             Risk_Category = case_when(
               Risk_Category == 5 ~ "5th",
               Risk_Category == 4 ~ "4th",
               Risk_Category == 3 ~ "3rd",
               Risk_Category == 2 ~ "2nd",
               Risk_Category == 1 ~ "1st")) %>%
      cbind(
        aggregate(
          dplyr::select(vandalism22) %>% mutate(vandCount = 1), ., sum) %>%
          mutate(vandCount = replace_na(vandCount, 0))) %>%
      dplyr::select(label,Risk_Category, vandCount)
    
    
    rbind(vand_KDE_sf, vand_risk_sf) %>%
      na.omit() %>%
      gather(Variable, Value, -label, -Risk_Category, -geometry) %>%
      ggplot() +
      geom_sf(aes(fill = Risk_Category), colour = NA) +
      geom_sf(data = sample_n(vandalism22, 3000), size = .25, colour = "black") +
      facet_wrap(~label, ) +
      scale_fill_viridis(discrete = TRUE) +
      labs(title="Comparison of Kernel Density and Risk Predictions",
           subtitle="2021 Vandalism; 2022 Vandalism risk predictions",
           caption = "Figure 10.") +
      theme(legend.position="bottom") +
      mapTheme(title_size = 14)
    
  }
  
  # comparison of kernel density to risk prediction model
  {
    rbind(vand_KDE_sf, vand_risk_sf) %>%
      st_drop_geometry() %>%
      na.omit() %>%
      gather(Variable, Value, -label, -Risk_Category) %>%
      group_by(label, Risk_Category) %>%
      summarize(countVand = sum(Value)) %>%
      ungroup() %>%
      group_by(label) %>%
      mutate(Pcnt_of_test_set_crimes = countVand / sum(countVand)) %>%
      ggplot(aes(Risk_Category,Pcnt_of_test_set_crimes)) +
      geom_bar(aes(fill=label), position="dodge", stat="identity") +
      scale_fill_viridis(discrete = TRUE, name = "Model") +
      labs(title = "Risk prediction vs. Kernel density, 2022 Vandalism Incidents",
           y = "% of Test Set Vandalism Incidents (per model)",
           x = "Risk Category",
           caption = "Figure 11.") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
    
  }
}




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
  #   filter(year %in% c(2013:2019,2022)) %>% 
  #   st_transform(crs = 2272)
  
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
    # B19013_001: medHHincome
    # B01001_001: total pop 
    # B02001_002: white pop
    # B25058_001: median rent
    # B15003_022: attainment of bachelor's of population 25+
    # B25071_001: Median gross rent as a percentage of household income (past year)
    # B07201_001: Geographical Mobility in the Past Year for Current Residence--Metropolitan Statistical Area Level in the United States, estimated total
    # B07201_002: Geographical Mobility in the Past Year for Current Residence--Metropolitan Statistical Area Level in the United States, same house 1 year ago 
    # B25008_002: total population in occupied housing by tenure, owner occupied
    # B25008_003: total population in occupied housing by tenure, renter occupied
    # B99082_001: allocation of private vehicle occupancy
    # B25044_001: tenure by vehicles available
    # B09002_001: own children under 18
    # B11004_001: related children under 18
    
    census_vars <- paste0(c("B01001_001", "B19013_001", "B02001_002", "B25058_001", "B15003_022", "B25071_001", "B07201_001",
                            "B07201_002", "B25008_002", "B25008_003", "B99082_001", "B25044_001", "B09002_001", "B11004_001"),"E") # census variables of interest that are available
    
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
             med_hh_inc = B19013_001E,
             white_pop = B02001_002E,
             med_rent = B25058_001E,
             bachelors25 = B15003_022E,
             pct_rent_hhinc = B25071_001E,
             mobility_tot_metro = B07201_001E,
             samehouse1yr_metro = B07201_002E,
             owner_occ = B25008_002E,
             renter_occ = B25008_003E,
             private_vehicle_occ = B99082_001E,
             vehicles_avail = B25044_001E,
      ) %>%
      mutate(children = B09002_001E + B11004_001E,
             pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
             RaceContext = ifelse(pct_white > 0.5, "Majority White", 
                                  ifelse(total_pop != 0 , "Majority non-White", NA)),
             year = "2013") %>% 
      dplyr::select(!matches("^B|white_pop")) %>% 
      st_centroid()
    
    tracts14 <- 
      get_acs(geography = "block group", 
              variables = census_vars, 
              year = 2014, state = 42,
              geometry = T, output = "wide") %>%
      st_transform(crs = 2272) %>%
      dplyr::select(!matches("M$")) %>% 
      rename(total_pop = B01001_001E,
             med_hh_inc = B19013_001E,
             white_pop = B02001_002E,
             med_rent = B25058_001E,
             bachelors25 = B15003_022E,
             pct_rent_hhinc = B25071_001E,
             mobility_tot_metro = B07201_001E,
             samehouse1yr_metro = B07201_002E,
             owner_occ = B25008_002E,
             renter_occ = B25008_003E,
             private_vehicle_occ = B99082_001E,
             vehicles_avail = B25044_001E,
      ) %>%
      mutate(children = B09002_001E + B11004_001E,
             pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
             RaceContext = ifelse(pct_white > 0.5, "Majority White", 
                                  ifelse(total_pop != 0 , "Majority non-White", NA)),
             year = "2014") %>% 
      dplyr::select(!matches("^B|white_pop")) %>% 
      st_centroid()
    
    tracts15 <- 
      get_acs(geography = "block group", 
              variables = census_vars, 
              year = 2015, state = 42,
              geometry = T, output = "wide") %>%
      st_transform(crs = 2272) %>%
      dplyr::select(!matches("M$")) %>% 
      rename(total_pop = B01001_001E,
             med_hh_inc = B19013_001E,
             white_pop = B02001_002E,
             med_rent = B25058_001E,
             bachelors25 = B15003_022E,
             pct_rent_hhinc = B25071_001E,
             mobility_tot_metro = B07201_001E,
             samehouse1yr_metro = B07201_002E,
             owner_occ = B25008_002E,
             renter_occ = B25008_003E,
             private_vehicle_occ = B99082_001E,
             vehicles_avail = B25044_001E,
      ) %>%
      mutate(children = B09002_001E + B11004_001E,
             pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
             RaceContext = ifelse(pct_white > 0.5, "Majority White", 
                                  ifelse(total_pop != 0 , "Majority non-White", NA)),
             year = "2015") %>% 
      dplyr::select(!matches("^B|white_pop")) %>% 
      st_centroid()
    
    tracts16 <- 
      get_acs(geography = "block group", 
              variables = census_vars, 
              year = 2016, state = 42,
              geometry = T, output = "wide") %>%
      st_transform(crs = 2272) %>%
      dplyr::select(!matches("M$")) %>% 
      rename(total_pop = B01001_001E,
             med_hh_inc = B19013_001E,
             white_pop = B02001_002E,
             med_rent = B25058_001E,
             bachelors25 = B15003_022E,
             pct_rent_hhinc = B25071_001E,
             mobility_tot_metro = B07201_001E,
             samehouse1yr_metro = B07201_002E,
             owner_occ = B25008_002E,
             renter_occ = B25008_003E,
             private_vehicle_occ = B99082_001E,
             vehicles_avail = B25044_001E,
      ) %>%
      mutate(children = B09002_001E + B11004_001E,
             pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
             RaceContext = ifelse(pct_white > 0.5, "Majority White", 
                                  ifelse(total_pop != 0 , "Majority non-White", NA)),
             year = "2016") %>% 
      dplyr::select(!matches("^B|white_pop")) %>% 
      st_centroid()
    
    tracts17 <- 
      get_acs(geography = "block group", 
              variables = census_vars, 
              year = 2017, state = 42,
              geometry = T, output = "wide") %>%
      st_transform(crs = 2272) %>%
      dplyr::select(!matches("M$")) %>% 
      rename(total_pop = B01001_001E,
             med_hh_inc = B19013_001E,
             white_pop = B02001_002E,
             med_rent = B25058_001E,
             bachelors25 = B15003_022E,
             pct_rent_hhinc = B25071_001E,
             mobility_tot_metro = B07201_001E,
             samehouse1yr_metro = B07201_002E,
             owner_occ = B25008_002E,
             renter_occ = B25008_003E,
             private_vehicle_occ = B99082_001E,
             vehicles_avail = B25044_001E,
      ) %>%
      mutate(children = B09002_001E + B11004_001E,
             pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
             RaceContext = ifelse(pct_white > 0.5, "Majority White", 
                                  ifelse(total_pop != 0 , "Majority non-White", NA)),
             year = "2017") %>% 
      dplyr::select(!matches("^B|white_pop")) %>% 
      st_centroid()
    
    tracts18 <- 
      get_acs(geography = "block group", 
              variables = census_vars, 
              year = 2018, state = 42,
              geometry = T, output = "wide") %>%
      st_transform(crs = 2272) %>%
      dplyr::select(!matches("M$")) %>% 
      rename(total_pop = B01001_001E,
             med_hh_inc = B19013_001E,
             white_pop = B02001_002E,
             med_rent = B25058_001E,
             bachelors25 = B15003_022E,
             pct_rent_hhinc = B25071_001E,
             mobility_tot_metro = B07201_001E,
             samehouse1yr_metro = B07201_002E,
             owner_occ = B25008_002E,
             renter_occ = B25008_003E,
             private_vehicle_occ = B99082_001E,
             vehicles_avail = B25044_001E,
      ) %>%
      mutate(children = B09002_001E + B11004_001E,
             pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
             RaceContext = ifelse(pct_white > 0.5, "Majority White", 
                                  ifelse(total_pop != 0 , "Majority non-White", NA)),
             year = "2018") %>% 
      dplyr::select(!matches("^B|white_pop")) %>% 
      st_centroid()
    
    tracts19 <- 
      get_acs(geography = "block group", 
              variables = census_vars, 
              year = 2019, state = 42,
              geometry = T, output = "wide") %>%
      st_transform(crs = 2272) %>%
      dplyr::select(!matches("M$")) %>% 
      rename(total_pop = B01001_001E,
             med_hh_inc = B19013_001E,
             white_pop = B02001_002E,
             med_rent = B25058_001E,
             bachelors25 = B15003_022E,
             pct_rent_hhinc = B25071_001E,
             mobility_tot_metro = B07201_001E,
             samehouse1yr_metro = B07201_002E,
             owner_occ = B25008_002E,
             renter_occ = B25008_003E,
             private_vehicle_occ = B99082_001E,
             vehicles_avail = B25044_001E,
      ) %>%
      mutate(children = B09002_001E + B11004_001E,
             pct_white = ifelse(total_pop > 0, white_pop / total_pop,0),
             RaceContext = ifelse(pct_white > 0.5, "Majority White", 
                                  ifelse(total_pop != 0 , "Majority non-White", NA)),
             year = "2019") %>% 
      dplyr::select(!matches("^B|white_pop")) %>% 
      st_centroid()
    
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
  
  # limit permit dat to philly border
  newcon_permits <- newcon_permits %>% 
    .[philly,]
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
  
  # 2013 
  {
    permit_net13 <- 
      dplyr::select(newcon_permits %>% filter(year == "2013")) %>% 
      mutate(count_permits = 1) %>% 
      aggregate(., fishnet, sum) %>%
      mutate(count_permits13 = replace_na(count_permits, 0),
             uniqueID = as.numeric(rownames(.)),
             cvID = sample(round(nrow(fishnet) / 16), size=nrow(fishnet), replace = TRUE))
  }
  
  # 2014
  {
    permit_net14 <- 
      dplyr::select(newcon_permits %>% filter(year == "2014")) %>% 
      mutate(count_permits = 1) %>% 
      aggregate(., fishnet, sum) %>%
      mutate(count_permits14 = replace_na(count_permits, 0),
             uniqueID = as.numeric(rownames(.)),
             cvID = sample(round(nrow(fishnet) / 16), size=nrow(fishnet), replace = TRUE)) %>% 
      dplyr::select(-count_permits)
  }
  
  # 2015
  {
    permit_net15 <- 
      dplyr::select(newcon_permits %>% filter(year == "2015")) %>% 
      mutate(count_permits = 1) %>% 
      aggregate(., fishnet, sum) %>%
      mutate(count_permits15 = replace_na(count_permits, 0),
             uniqueID = as.numeric(rownames(.)),
             cvID = sample(round(nrow(fishnet) / 16), size=nrow(fishnet), replace = TRUE)) %>% 
      dplyr::select(-count_permits)
  }
  
  # 2016
  {
    permit_net16 <- 
      dplyr::select(newcon_permits %>% filter(year == "2016")) %>% 
      mutate(count_permits = 1) %>% 
      aggregate(., fishnet, sum) %>%
      mutate(count_permits16 = replace_na(count_permits, 0),
             uniqueID = as.numeric(rownames(.)),
             cvID = sample(round(nrow(fishnet) / 16), size=nrow(fishnet), replace = TRUE)) %>% 
      dplyr::select(-count_permits)
  }
  
  # 2017
  {
    permit_net17 <- 
      dplyr::select(newcon_permits %>% filter(year == "2017")) %>% 
      mutate(count_permits = 1) %>% 
      aggregate(., fishnet, sum) %>%
      mutate(count_permits17 = replace_na(count_permits, 0),
             uniqueID = as.numeric(rownames(.)),
             cvID = sample(round(nrow(fishnet) / 16), size=nrow(fishnet), replace = TRUE)) %>% 
      dplyr::select(-count_permits)
  }
  
  # 2018
  {
    permit_net18 <- 
      dplyr::select(newcon_permits %>% filter(year == "2018")) %>% 
      mutate(count_permits = 1) %>% 
      aggregate(., fishnet, sum) %>%
      mutate(count_permits18 = replace_na(count_permits, 0),
             uniqueID = as.numeric(rownames(.)),
             cvID = sample(round(nrow(fishnet) / 16), size=nrow(fishnet), replace = TRUE)) %>% 
      dplyr::select(-count_permits)
  }
  
  # 2019
  {
    permit_net19 <- 
      dplyr::select(newcon_permits %>% filter(year == "2019")) %>% 
      mutate(count_permits = 1) %>% 
      aggregate(., fishnet, sum) %>%
      mutate(count_permits19 = replace_na(count_permits, 0),
             uniqueID = as.numeric(rownames(.)),
             cvID = sample(round(nrow(fishnet) / 16), size=nrow(fishnet), replace = TRUE)) %>% 
      dplyr::select(-count_permits)
  }
  
  # combine permit counts from all years
  permit_net_allyrs <- permit_net13 %>% 
    st_drop_geometry() %>%
    dplyr::select(uniqueID,cvID,count_permits13) %>% 
    left_join(permit_net14 %>% st_drop_geometry() %>% dplyr::select(uniqueID,count_permits14), by = "uniqueID") %>% 
    left_join(permit_net15 %>% st_drop_geometry() %>% dplyr::select(uniqueID,count_permits15), by = "uniqueID") %>% 
    left_join(permit_net16 %>% st_drop_geometry() %>% dplyr::select(uniqueID,count_permits16), by = "uniqueID") %>% 
    left_join(permit_net17 %>% st_drop_geometry() %>% dplyr::select(uniqueID,count_permits17), by = "uniqueID") %>% 
    left_join(permit_net18 %>% st_drop_geometry() %>% dplyr::select(uniqueID,count_permits18), by = "uniqueID") %>% 
    left_join(permit_net19 %>% st_drop_geometry() %>% dplyr::select(uniqueID,count_permits19), by = "uniqueID") %>% 
    left_join(permit_net13 %>% dplyr::select(-c(cvID,count_permits13,count_permits)), by = "uniqueID")
  
  # TODO: edit this to show permit count for all years on separate maps
  # would need to pur permit_net_allyrs into long form so i can use facet_wrap()
  # for '13, '15, '17, '19
  
  # permit_count_map <- ggplot() +
  #   geom_sf(data = permit_net_allyrs, aes(fill = count_permits)) +
  #   scale_fill_viridis() +
  #   labs(title = "Count of New Construction Permits for the fishnet",
  #        caption = "Figure x.") +
  #   mapTheme()
  # 
  # permit_count_map
}

# 311 incident data
# Ben will figure this part out
{
  carto_url = "https://phl.carto.com/api/v2/sql"
  
  # Crime incidents
  table_name_311 = "public_cases_fc"
  
  # query
  where_311 = "closed_datetime >= '2010-01-01' AND closed_datetime < '2020-01-01' AND service_name IN ('Rubbish/Recyclable Material Collection','Illegal Dumping','Construction Complaints', 'Building Construction', 'Sanitation Violation', 'Street Trees', 'Dangerous Sidewalk', 'Homeless Encampment Request', 'Parks and Rec Safety and Maintenance', 'Vacant House or Commercial')"
  
  query311 = paste("SELECT *",
                   "FROM", table_name_311,
                   "WHERE", where_311)
  
  reports311 = rphl::get_carto(query311, format = "csv", base_url = carto_url, stringsAsFactors = F)
  reports311 <- reports311 %>%
    mutate(Year = year(format.Date(reports311$closed_datetime)))
  reports311 <- reports311 %>%
    filter(!is.na(lon) & !is.na(lat)) %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
    #st_transform('ESRI:102728') %>%
    st_transform(crs = 2272) %>% 
    mutate(
      type = service_name,
      Legend = "311") %>% 
    dplyr::select(Legend, type, geometry, Year)
  
  # make fishnet of all reports across all years
  {
    reports_15 <- reports311 %>% 
      filter(Year == 2015)
    reports_16 <- reports311 %>% 
      filter(Year == 2016)
    reports_17 <- reports311 %>% 
      filter(Year == 2017)
    reports_18 <- reports311 %>% 
      filter(Year == 2018)
    reports_19 <- reports311 %>% 
      filter(Year == 2019)
    
    long_15 <- reports_15 %>% 
      arrange(type) %>%  
      mutate(Legend = paste(type, Year), .keep = "unused")
    long_16 <- reports_16 %>% 
      arrange(type) %>%  
      mutate(Legend = paste(type, Year), .keep = "unused")
    long_17 <- reports_17 %>% 
      arrange(type) %>%  
      mutate(Legend = paste(type, Year), .keep = "unused")
    long_18 <- reports_18 %>% 
      arrange(type) %>%  
      mutate(Legend = paste(type, Year), .keep = "unused")
    long_19 <- reports_19 %>% 
      arrange(type) %>%  
      mutate(Legend = paste(type, Year), .keep = "unused")
    
    vars311_net <- rbind(long_15, long_16, long_17, long_18, long_19) %>% 
      st_join(fishnet, join = st_within) %>%
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
    
    vars311_net_ccoid <- st_c(st_coid(vars311_net))
    
    # add nearest neighbor features
    vars311_net <- vars311_net %>%
      mutate(buildingCon15.nn = nn_function(vars311_net_ccoid, st_c(long_15 %>% filter(Legend == "Building Construction 2015")), 3),
             dangerSide15.nn = nn_function(vars311_net_ccoid, st_c(long_15 %>% filter(Legend == "Dangerous Sidewalk 2015")), 3),
             illegalDump15.nn = nn_function(vars311_net_ccoid, st_c(long_15 %>% filter(Legend == "Illegal Dumping 2015")), 3),
             parksNrec15.nn = nn_function(vars311_net_ccoid, st_c(long_15 %>% filter(Legend == "Parks and Rec Safety and Maintenance 2015")), 3),
             materialColl15.nn = nn_function(vars311_net_ccoid, st_c(long_15 %>% filter(Legend == "Rubbish/Recyclable Material Collection 2015")), 3),
             streetTrees15.nn = nn_function(vars311_net_ccoid, st_c(long_15 %>% filter(Legend == "Street Trees 2015")), 3),
             vacant15.nn = nn_function(vars311_net_ccoid, st_c(long_15 %>% filter(Legend == "Vacant House or Commercial 2015")), 3),
             
             buildingCon16.nn = nn_function(vars311_net_ccoid, st_c(long_16 %>% filter(Legend == "Building Construction 2016")), 3),
             dangerSide16.nn = nn_function(vars311_net_ccoid, st_c(long_16 %>% filter(Legend == "Dangerous Sidewalk 2016")), 3),
             illegalDump16.nn = nn_function(vars311_net_ccoid, st_c(long_16 %>% filter(Legend == "Illegal Dumping 2016")), 3),
             parksNrec16.nn = nn_function(vars311_net_ccoid, st_c(long_16 %>% filter(Legend == "Parks and Rec Safety and Maintenance 2016")), 3),
             materialColl16.nn = nn_function(vars311_net_ccoid, st_c(long_16 %>% filter(Legend == "Rubbish/Recyclable Material Collection 2016")), 3),
             streetTrees16.nn = nn_function(vars311_net_ccoid, st_c(long_16 %>% filter(Legend == "Street Trees 2016")), 3),
             vacant16.nn = nn_function(vars311_net_ccoid, st_c(long_16 %>% filter(Legend == "Vacant House or Commercial 2016")), 3),
             
             buildingCon17.nn = nn_function(vars311_net_ccoid, st_c(long_17 %>% filter(Legend == "Building Construction 2017")), 3),
             dangerSide17.nn = nn_function(vars311_net_ccoid, st_c(long_17 %>% filter(Legend == "Dangerous Sidewalk 2017")), 3),
             illegalDump17.nn = nn_function(vars311_net_ccoid, st_c(long_17 %>% filter(Legend == "Illegal Dumping 2017")), 3),
             parksNrec17.nn = nn_function(vars311_net_ccoid, st_c(long_17 %>% filter(Legend == "Parks and Rec Safety and Maintenance 2017")), 3),
             materialColl17.nn = nn_function(vars311_net_ccoid, st_c(long_17 %>% filter(Legend == "Rubbish/Recyclable Material Collection 2017")), 3),
             streetTrees17.nn = nn_function(vars311_net_ccoid, st_c(long_17 %>% filter(Legend == "Street Trees 2017")), 3),
             vacant17.nn = nn_function(vars311_net_ccoid, st_c(long_17 %>% filter(Legend == "Vacant House or Commercial 2017")), 3),
             
             buildingCon18.nn = nn_function(vars311_net_ccoid, st_c(long_18 %>% filter(Legend == "Building Construction 2018")), 3),
             dangerSide18.nn = nn_function(vars311_net_ccoid, st_c(long_18 %>% filter(Legend == "Dangerous Sidewalk 2018")), 3),
             illegalDump18.nn = nn_function(vars311_net_ccoid, st_c(long_18 %>% filter(Legend == "Illegal Dumping 2018")), 3),
             parksNrec18.nn = nn_function(vars311_net_ccoid, st_c(long_18 %>% filter(Legend == "Parks and Rec Safety and Maintenance 2018")), 3),
             materialColl18.nn = nn_function(vars311_net_ccoid, st_c(long_18 %>% filter(Legend == "Rubbish/Recyclable Material Collection 2018")), 3),
             streetTrees18.nn = nn_function(vars311_net_ccoid, st_c(long_18 %>% filter(Legend == "Street Trees 2018")), 3),
             vacant18.nn = nn_function(vars311_net_ccoid, st_c(long_18 %>% filter(Legend == "Vacant House or Commercial 2018")), 3),
             
             buildingCon19.nn = nn_function(vars311_net_ccoid, st_c(long_19 %>% filter(Legend == "Building Construction 2019")), 3),
             dangerSide19.nn = nn_function(vars311_net_ccoid, st_c(long_19 %>% filter(Legend == "Dangerous Sidewalk 2019")), 3),
             illegalDump19.nn = nn_function(vars311_net_ccoid, st_c(long_19 %>% filter(Legend == "Illegal Dumping 2019")), 3),
             parksNrec19.nn = nn_function(vars311_net_ccoid, st_c(long_19 %>% filter(Legend == "Parks and Rec Safety and Maintenance 2019")), 3),
             materialColl19.nn = nn_function(vars311_net_ccoid, st_c(long_19 %>% filter(Legend == "Rubbish/Recyclable Material Collection 2019")), 3),
             streetTrees19.nn = nn_function(vars311_net_ccoid, st_c(long_19 %>% filter(Legend == "Street Trees 2019")), 3),
             vacant19.nn = nn_function(vars311_net_ccoid, st_c(long_19 %>% filter(Legend == "Vacant House or Commercial 2019")), 3)) 
  }

}


# Adding census variables to fishnet
{
  # function to make fishnet from acs dataframe (dat_tract), for variable (var_name), giving it the legend (legend_name), aggregating with function (sum_or_mean)
  # for example, to make a fishnet summing all total_pop variables for 2013, one would define the variables as follows:
  # dat_tract <- tracts13; var_name <- "total_pop"; legend_name <- "Total Population"; sum_or_mean <- sum
  makenet <- function(dat_tract, var_name, legend_name, sum_or_mean){
    
    net_tract <- dat_tract %>% 
      .[philly,] %>% 
      dplyr::select(matches(var_name)) %>% 
      mutate(Legend = legend_name) %>% 
      st_join(fishnet, join=st_within) %>%
      st_drop_geometry() %>%
      group_by(uniqueID, Legend) %>%
      summarize(count = sum_or_mean(get(var_name), na.rm = T)) %>%
      left_join(fishnet, ., by = "uniqueID") %>%  # add geometry back in
      spread(Legend, count, fill=0) %>%  # fill in ones where fishnet was missing, count was NA with 0
      dplyr::select(-`<NA>`) %>%
      ungroup()
    
    return(net_tract)
  }
  
  var_names <- names(tracts13)[3:14]
  legend_names <- c("Total Population", "Median HH Income", "Median Rent", "Rent (as %age of Income)", "mobility_tot_metro",
                    "samehouse1yr_metro","owner_occ","renter_occ","vehicles_avail","private_vehicle_occ","children","pct_white")
  
  dat_tracts <- paste0("tracts",13:19)
  
  # for loop to create fish nets for all ACS variables for all years
  for (i in 1:length(dat_tracts)) {
    dat_tract <- get(dat_tracts[i])
    
    for (j in 1:length(var_names)){
      # select year, variable, and legend name
      yr <- i+12
      var_name <- var_names[j]
      legend_name <- paste(legend_names[j],yr)
      
      if (j %in% c(2:4,18)){
        sum_or_mean <- mean
      } else {
        sum_or_mean <- sum
      }
      
      # run function to make fishnet
      net_tract <- makenet(dat_tract,var_name,legend_name,sum_or_mean)
      
      # save as individual fishnet, uncomment if neededd
      # net_tract_name <- paste0("net_",var_name,yr)
      # assign(net_tract_name, net_tract)
      
      # save all variables into one big net
      if (i == 1 & j == 1) {
        census_net <- net_tract %>% 
          st_drop_geometry()
      } else {
        net_tract_tojoin <- net_tract %>% 
          st_drop_geometry() %>% 
          dplyr::select(1:2)
        census_net <- left_join(census_net, net_tract_tojoin, by = "uniqueID")
      }
    }
  }
  
}

# join all nets together
{
  # join census vars fishnet to permit count net + 311 net
  all_net <- left_join(permit_net_allyrs, census_net, by = "uniqueID") %>% 
    left_join(vars311_net %>% st_drop_geometry(), by = "uniqueID") %>% 
    st_as_sf()
  
  all_net15 <- all_net %>% 
    dplyr::select(matches("uniqueID|cvID|geometry|15"))
  
  all_net16 <- all_net %>% 
    dplyr::select(matches("uniqueID|cvID|geometry|16"))
  
  all_net17 <- all_net %>% 
    dplyr::select(matches("uniqueID|cvID|geometry|17"))
  
  all_net18 <- all_net %>% 
    dplyr::select(matches("uniqueID|cvID|geometry|18|count_permits15|count_permits16|count_permits17"))
  
  all_net19 <- all_net %>% 
    dplyr::select(matches("uniqueID|cvID|geometry|19|count_permits16|count_permits17|count_permits18"))
  
}

# correlation of census variables to count permit
{
  # 2015 net
  {
    for_cormat15 <- all_net15 %>% 
      st_drop_geometry() %>% 
      dplyr::select(-c(uniqueID,cvID))
    
    ggcorrplot(
      round(cor(for_cormat15), 1), 
      # method = "circle",
      p.mat = cor_pmat(for_cormat15),
      colors = c("#4b2875", "white", "#9c1339"),
      type="lower",
      insig = "blank",
      digits = 4,
      lab = T, lab_size = 2) +  
      labs(title = "Correlation Matrix for 2015 fishnet",
           caption = "Figure x.") 
    
    cor_mat15 <- data.frame(cor(for_cormat15)) %>% 
      dplyr::select(count_permits15) %>% 
      # arrange(desc(count_permits15)) %>% 
      tibble::rownames_to_column("feature")
  }
  
  # 2016 net
  {
    for_cormat16 <- all_net16 %>% 
      st_drop_geometry() %>% 
      dplyr::select(-c(uniqueID,cvID))
    
    ggcorrplot(
      round(cor(for_cormat16), 1), 
      # method = "circle",
      p.mat = cor_pmat(for_cormat16),
      colors = c("#4b2875", "white", "#9c1339"),
      type="lower",
      insig = "blank",
      digits = 4,
      lab = T, lab_size = 2) +  
      labs(title = "Correlation Matrix for 2016 fishnet",
           caption = "Figure x.") 
    
    cor_mat16 <- data.frame(cor(for_cormat16)) %>% 
      dplyr::select(count_permits16) %>% 
      # arrange(desc(count_permits16)) %>%
      tibble::rownames_to_column("feature")
  }
  
  # 2017 net
  {
    for_cormat17 <- all_net17 %>% 
      st_drop_geometry() %>% 
      dplyr::select(-c(uniqueID,cvID))
    
    ggcorrplot(
      round(cor(for_cormat17), 1), 
      # method = "circle",
      p.mat = cor_pmat(for_cormat17),
      colors = c("#4b2875", "white", "#9c1339"),
      type="lower",
      insig = "blank",
      digits = 4,
      lab = T, lab_size = 2) +  
      labs(title = "Correlation Matrix for 2017 fishnet",
           caption = "Figure x.") 
    
    cor_mat17 <- data.frame(cor(for_cormat17)) %>% 
      dplyr::select(count_permits17) %>% 
      # arrange(desc(count_permits17)) %>% 
      tibble::rownames_to_column("feature")
  }
  
  # 2018 net
  {
    for_cormat18 <- all_net18 %>% 
      st_drop_geometry() %>% 
      dplyr::select(-c(uniqueID,cvID))
    
    ggcorrplot(
      round(cor(for_cormat18), 1), 
      # method = "circle",
      p.mat = cor_pmat(for_cormat18),
      colors = c("#4b2875", "white", "#9c1339"),
      type="lower",
      insig = "blank",
      digits = 4,
      lab = T, lab_size = 2) +  
      labs(title = "Correlation Matrix for 2018 fishnet",
           caption = "Figure x.") 
    
    cor_mat18 <- data.frame(cor(for_cormat18)) %>% 
      dplyr::select(count_permits18) %>% 
      # arrange(desc(count_permits18)) %>% 
      tibble::rownames_to_column("feature")
  }
  
  # 2019 net
  {
    for_cormat19 <- all_net19 %>% 
      st_drop_geometry() %>% 
      dplyr::select(-c(uniqueID,cvID))
    
    ggcorrplot(
      round(cor(for_cormat19), 1), 
      # method = "circle",
      p.mat = cor_pmat(for_cormat19),
      colors = c("#4b2875", "white", "#9c1339"),
      type="lower",
      insig = "blank",
      digits = 4,
      lab = T, lab_size = 2) +  
      labs(title = "Correlation Matrix for 2019 fishnet",
           caption = "Figure x.") 
    
    cor_mat19 <- data.frame(cor(for_cormat19)) %>% 
      dplyr::select(count_permits19) %>% 
      # arrange(desc(count_permits19)) %>% 
      tibble::rownames_to_column("feature")
  }
  
  # weird way of looking at which predictors are the most accurate
  {
    
  }
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




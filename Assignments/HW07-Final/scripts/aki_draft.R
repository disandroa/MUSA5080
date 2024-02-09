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
  library(stats)
  
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
    st_transform(crs = 2272) %>% 
    mutate(
      type = str_replace_all(service_name, "[^[:alnum:]]", ""),
      Legend = "311") %>% 
    dplyr::select(Legend, type, geometry, Year)
  
  # make fishnet of all reports across all years
  {
    long_15 <- reports311 %>% 
      filter(Year == 2015) %>% 
      arrange(type) %>%  
      mutate(Legend = paste0(type, Year), .keep = "unused")
    
    long_16 <- reports311 %>% 
      filter(Year == 2016) %>% 
      arrange(type) %>%  
      mutate(Legend = paste0(type, Year), .keep = "unused")
    
    long_17 <- reports311 %>% 
      filter(Year == 2017) %>% 
      arrange(type) %>%  
      mutate(Legend = paste0(type, Year), .keep = "unused")
    
    long_18 <- reports311 %>% 
      filter(Year == 2018) %>% 
      arrange(type) %>%  
      mutate(Legend = paste0(type, Year), .keep = "unused")
    
    long_19 <- reports311 %>% 
      filter(Year == 2019) %>% 
      arrange(type) %>%  
      mutate(Legend = paste0(type, Year), .keep = "unused")
    
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
      mutate(buildingCon15.dist = nn_function(vars311_net_ccoid, st_c(long_15 %>% filter(Legend == "BuildingConstruction2015")), 3),
             dangerSide15.dist = nn_function(vars311_net_ccoid, st_c(long_15 %>% filter(Legend == "DangerousSidewalk2015")), 3),
             illegalDump15.dist = nn_function(vars311_net_ccoid, st_c(long_15 %>% filter(Legend == "IllegalDumping2015")), 3),
             parksNrec15.dist = nn_function(vars311_net_ccoid, st_c(long_15 %>% filter(Legend == "ParksandRecSafetyandMaintenance2015")), 3),
             materialColl15.dist = nn_function(vars311_net_ccoid, st_c(long_15 %>% filter(Legend == "RubbishRecyclableMaterialCollection2015")), 3),
             streetTrees15.dist = nn_function(vars311_net_ccoid, st_c(long_15 %>% filter(Legend == "StreetTrees2015")), 3),
             vacant15.dist = nn_function(vars311_net_ccoid, st_c(long_15 %>% filter(Legend == "VacantHouseorCommercial2015")), 3),
             
             buildingCon16.dist = nn_function(vars311_net_ccoid, st_c(long_16 %>% filter(Legend == "BuildingConstruction2016")), 3),
             dangerSide16.dist = nn_function(vars311_net_ccoid, st_c(long_16 %>% filter(Legend == "DangerousSidewalk2016")), 3),
             illegalDump16.dist = nn_function(vars311_net_ccoid, st_c(long_16 %>% filter(Legend == "IllegalDumping2016")), 3),
             parksNrec16.dist = nn_function(vars311_net_ccoid, st_c(long_16 %>% filter(Legend == "ParksandRecSafetyandMaintenance2016")), 3),
             materialColl16.dist = nn_function(vars311_net_ccoid, st_c(long_16 %>% filter(Legend == "RubbishRecyclableMaterialCollection2016")), 3),
             streetTrees16.dist = nn_function(vars311_net_ccoid, st_c(long_16 %>% filter(Legend == "StreetTrees2016")), 3),
             vacant16.dist = nn_function(vars311_net_ccoid, st_c(long_16 %>% filter(Legend == "VacantHouseorCommercial2016")), 3),
             
             buildingCon17.dist = nn_function(vars311_net_ccoid, st_c(long_17 %>% filter(Legend == "BuildingConstruction2017")), 3),
             dangerSide17.dist = nn_function(vars311_net_ccoid, st_c(long_17 %>% filter(Legend == "DangerousSidewalk2017")), 3),
             illegalDump17.dist = nn_function(vars311_net_ccoid, st_c(long_17 %>% filter(Legend == "IllegalDumping2017")), 3),
             parksNrec17.dist = nn_function(vars311_net_ccoid, st_c(long_17 %>% filter(Legend == "ParksandRecSafetyandMaintenance2017")), 3),
             materialColl17.dist = nn_function(vars311_net_ccoid, st_c(long_17 %>% filter(Legend == "RubbishRecyclableMaterialCollection2017")), 3),
             streetTrees17.dist = nn_function(vars311_net_ccoid, st_c(long_17 %>% filter(Legend == "StreetTrees2017")), 3),
             vacant17.dist = nn_function(vars311_net_ccoid, st_c(long_17 %>% filter(Legend == "VacantHouseorCommercial2017")), 3),
             
             buildingCon18.dist = nn_function(vars311_net_ccoid, st_c(long_18 %>% filter(Legend == "BuildingConstruction2018")), 3),
             dangerSide18.dist = nn_function(vars311_net_ccoid, st_c(long_18 %>% filter(Legend == "DangerousSidewalk2018")), 3),
             illegalDump18.dist = nn_function(vars311_net_ccoid, st_c(long_18 %>% filter(Legend == "IllegalDumping2018")), 3),
             parksNrec18.dist = nn_function(vars311_net_ccoid, st_c(long_18 %>% filter(Legend == "ParksandRecSafetyandMaintenance2018")), 3),
             materialColl18.dist = nn_function(vars311_net_ccoid, st_c(long_18 %>% filter(Legend == "RubbishRecyclableMaterialCollection2018")), 3),
             streetTrees18.dist = nn_function(vars311_net_ccoid, st_c(long_18 %>% filter(Legend == "StreetTrees2018")), 3),
             vacant18.dist = nn_function(vars311_net_ccoid, st_c(long_18 %>% filter(Legend == "VacantHouseorCommercial2018")), 3),
             
             buildingCon19.dist = nn_function(vars311_net_ccoid, st_c(long_19 %>% filter(Legend == "BuildingConstruction2019")), 3),
             dangerSide19.dist = nn_function(vars311_net_ccoid, st_c(long_19 %>% filter(Legend == "DangerousSidewalk2019")), 3),
             illegalDump19.dist = nn_function(vars311_net_ccoid, st_c(long_19 %>% filter(Legend == "IllegalDumping2019")), 3),
             parksNrec19.dist = nn_function(vars311_net_ccoid, st_c(long_19 %>% filter(Legend == "ParksandRecSafetyandMaintenance2019")), 3),
             materialColl19.dist = nn_function(vars311_net_ccoid, st_c(long_19 %>% filter(Legend == "RubbishRecyclableMaterialCollection2019")), 3),
             streetTrees19.dist = nn_function(vars311_net_ccoid, st_c(long_19 %>% filter(Legend == "StreetTrees2019")), 3),
             vacant19.dist = nn_function(vars311_net_ccoid, st_c(long_19 %>% filter(Legend == "VacantHouseorCommercial2019")), 3)) 
  }

}




# Adding census variables to fishnet
{
  # function to make fishnet from acs dataframe (dat_tract), for variable (var_name), giving it the legend (legend_name), aggregating with function (sum_or_mean)
  # for example, to make a fishnet summing all total_pop variables for 2013, one would define the variables as follows:
  # dat_tract <- tracts13; var_name <- "total_pop"; legend_name <- "total_pop13"; sum_or_mean <- sum
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
  
  dat_tracts <- paste0("tracts",13:19)
  
  # for loop to create fish nets for all ACS variables for all years
  for (i in 1:length(dat_tracts)) {
    dat_tract <- get(dat_tracts[i])
    
    for (j in 1:length(var_names)){
      # select year, variable, and legend name
      yr <- i+12
      var_name <- var_names[j]
      legend_name <- paste(var_name,yr)
      
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
    dplyr::select(matches("uniqueID|cvID|geometry|19|count_permits15|count_permits16|count_permits17|count_permits18"))
  
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
    # cor_mats <- cbind(cor_mat15,cor_mat16$count_permits16,cor_mat17$count_permits17) %>% 
    #   rename(count_permits16 = `cor_mat16$count_permits16`,
    #          count_permits17 = `cor_mat17$count_permits17`) %>% 
    #   rbind(data.frame(feature = c("count_permits_3","count_permits_2","count_permits_1"),
    #                    count_permits15 = rep(0,3), 
    #                    count_permits16 = rep(0,3),
    #                    count_permits17 = rep(0,3)), .) %>% 
    #   cbind(cor_mat18$count_permits18,cor_mat19$count_permits19) %>% 
    #   rename(count_permits18 = `cor_mat18$count_permits18`,
    #          count_permits19 = `cor_mat19$count_permits19`)
    # cor_mats$average <- abs(rowMeans(cor_mats[,2:6]))
    # cor_mats <- cor_mats %>% 
    #   arrange(desc(average))
    # # fixing some stuff
    # cor_mats[5,7] <- mean(c(cor_mat18 %>% filter(feature == "count_permits17") %>% pull(count_permits18),
    #                       cor_mat19 %>% filter(feature == "count_permits18") %>% pull(count_permits19)))
    # cor_mats[7,7] <- mean(c(cor_mat17 %>% filter(feature == "count_permits15") %>% pull(count_permits17),
    #                         cor_mat18 %>% filter(feature == "count_permits16") %>% pull(count_permits18),
    #                         cor_mat19 %>% filter(feature == "count_permits17") %>% pull(count_permits19)))
    # cor_mats[10,7] <- mean(c(cor_mat16 %>% filter(feature == "count_permits13") %>% pull(count_permits16),
    #                          cor_mat17 %>% filter(feature == "count_permits14") %>% pull(count_permits17),
    #                          cor_mat18 %>% filter(feature == "count_permits15") %>% pull(count_permits18),
    #                          cor_mat19 %>% filter(feature == "count_permits16") %>% pull(count_permits19)))
    # cor_mats <- cor_mats %>% 
    #   arrange(desc(average))
  }
}

# Data Exploration ----
{
  # moving forward with all_net19 now
  # define vars to keepf or model
  var_for_model <- c("uniqueID","cvID","count_permits19","count_permits18","count_permits17","count_permits16",
                     "count_permits15","BuildingConstruction2019","IllegalDumping2019","RubbishRecyclableMaterialCollection2019",
                     "DangerousSidewalk2019","private_vehicle_occ 19","VacantHouseorCommercial2019","renter_occ 19",
                     "vehicles_avail 19","med_rent 19","pct_white 19","total_pop 19","mobility_tot_metro 19",
                     "samehouse1yr_metro 19","dangerSide19.dist","buildingCon19.dist","med_hh_inc 19","pct_rent_hhinc 19",
                     "illegalDump19.dist","StreetTrees2019","vacant19.dist","owner_occ 19","parksNrec19.dist","streetTrees19.dist",
                     "materialColl19.dist","children 19","ParksandRecSafetyandMaintenance2019","mapname")
  
  # add neighborhood names
  allnet19_formodel <- all_net19 %>% 
    st_centroid() %>%
    st_join(dplyr::select(nhoods, mapname)) %>%
    st_drop_geometry() %>%
    left_join(dplyr::select(all_net19, geometry, uniqueID), by = "uniqueID") %>%
    st_sf()
  
  # only keep years for count_permit columns
  names(allnet19_formodel) <- c(str_replace_all(var_for_model, c(" 19" = "","2019" = "","19." = ".")), "geometry")
  
  # all predictors
  {
    vars_net.long <- gather(allnet19_formodel %>% dplyr::select(-count_permits19),
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
    
    # too many variables, have to split into three plots
    do.call(grid.arrange,c(varList[1:10], ncol = 4, top = "Predictors of Permit Count (on fishnet)", bottom = "Figure x."))
    do.call(grid.arrange,c(varList[11:20], ncol = 4, top = "Predictors of Permit Count (on fishnet)", bottom = "Figure x."))
    do.call(grid.arrange,c(varList[21:30], ncol = 4, top = "Predictors of Permit Count (on fishnet)", bottom = "Figure x."))
    
  }
  
  # spatial features
  {
    final_net.nb <- poly2nb(as_Spatial(allnet19_formodel), queen=TRUE)
    final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE) # turn neighborhood weights into list of weights
    
    local_morans <- localmoran(allnet19_formodel$count_permits19, final_net.weights, zero.policy=TRUE) %>%
      as.data.frame() # Ii moran's I at ith cell, Ei expected/mean from neighbors
    
    # join local Moran's I results to fishnet
    final_net.localMorans <- 
      cbind(local_morans, as.data.frame(allnet19_formodel)) %>% 
      st_sf() %>%
      dplyr::select(`Permit Count` = count_permits19, 
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
      allnet19_formodel %>% 
      mutate(permitct.isSig = 
               ifelse(localmoran(allnet19_formodel$count_permits19, 
                                 final_net.weights)[,5] <= 0.001, 1, 0)) %>%
      mutate(permitct.isSig.dist = 
               nn_function(st_coordinates(st_centroid(allnet19_formodel)),
                           st_coordinates(st_centroid(
                             filter(allnet19_formodel, permitct.isSig == 1))), 1))
    
  }
  
  # scatter plots of predictors
  {
    correlation.long <-
      st_drop_geometry(final_net) %>%
      dplyr::select(-uniqueID, -cvID, -mapname) %>%
      gather(variable, value, -count_permits19)
    
    correlation.cor <-
      correlation.long %>%
      group_by(variable) %>%
      summarize(correlation = cor(value, count_permits19, use = "complete.obs"))
    
    # multiple correlation plots
    var_group1 <- c("count_permits18","count_permits17","count_permits16","count_permits15",
                    "total_pop","med_hh_inc","med_rent","pct_rent_hhinc",
                    "mobility_tot_metro","samehouse1yr_metro","owner_occ","renter_occ",
                    "vehicles_avail","private_vehicle_occ","children","pct_white")
    
    var_group2 <- c("BuildingConstruction", "buildingCon.dist","IllegalDumping","illegalDump.dist",
                    "RubbishRecyclableMaterialCollection","materialColl.dist","DangerousSidewalk","dangerSide.dist",
                    "VacantHouseorCommercial","vacant.dist","StreetTrees","streetTrees.dist",
                    "ParksandRecSafetyandMaintenance","parksNrec.dist","permitct.isSig","permitct.isSig.dist")
    
    ggplot(correlation.long %>% 
             filter(variable %in% var_group1) %>% 
             mutate(variable = factor(variable, levels = var_group1)), 
           aes(value, count_permits19)) +
      geom_point(size = 0.1) +
      geom_text(data = correlation.cor %>% 
                  filter(variable %in% var_group1) %>% 
                  mutate(variable = factor(variable, levels = var_group1)), 
                aes(label = paste("r =", round(correlation, 2))),
                x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
      geom_smooth(method = "lm", se = FALSE, colour = "black") +
      facet_wrap(~variable, ncol = 4, scales = "free") +
      labs(title = "Permit count as a function of predictors (Group 1)",
           subtitle = "Group 1: Previous years' permit counts and census data",
           caption = "Figure x.") +
      plotTheme(title_size = 14)
    
    ggplot(correlation.long %>% 
             filter(variable %in% var_group2) %>% 
             mutate(variable = factor(variable, levels = var_group2)), 
           aes(value, count_permits19)) +
      geom_point(size = 0.1) +
      geom_text(data = correlation.cor %>% 
                  filter(variable %in% var_group2) %>% 
                  mutate(variable = factor(variable, levels = var_group2)), 
                aes(label = paste("r =", round(correlation, 2))),
                x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
      geom_smooth(method = "lm", se = FALSE, colour = "black") +
      facet_wrap(~variable, ncol = 4, scales = "free") +
      labs(title = "Permit count as a function of predictors (Group 2)",
           subtitle = "Group 2: 311 incident report data and spatial process",
           caption = "Figure x.") +
      plotTheme(title_size = 14)
  }
  
  # Poisson regression
  {
    final_net %>% ggplot(aes(x = count_permits19)) +
      geom_histogram(bins = 66, fill = viridis::cividis(1)) +
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
    reg.vars <- c("count_permits18","count_permits17","count_permits16","count_permits15","BuildingConstruction",
                  "IllegalDumping","DangerousSidewalk","RubbishRecyclableMaterialCollection","vehicles_avail",
                  "med_rent","renter_occ","VacantHouseorCommercial","total_pop",
                  "samehouse1yr_metro","StreetTrees","children","parksNrec.dist") # this row includes lower correlations that can be taken out
    
    # used the following to help decide reg.vars
    # temp <- correlation.cor %>% mutate(abs_cor = abs(correlation)) %>% arrange(desc(abs_cor))
    # rm(temp)
    
    # things to note:
    # Building Construction is highly correlated to dangerous sidewalk and illegal dumping, might change reg.vars
    
    ## RUN REGRESSIONS
    reg.CV <- crossValidate(dataset = final_net,
                            id = "cvID",
                            dependentVariable = "count_permits19",
                            indVariables = reg.vars) %>%
      dplyr::select(cvID, count_permits19, Prediction, geometry) %>% 
      mutate(error = count_permits19 - Prediction)
    
    # MAE
    reg.MAE <- mean(abs(reg.CV$error)) # ~7.558753
    
    # with local Moran's I spatial process features
    reg.sp.vars <- c(reg.vars,"permitct.isSig","permitct.isSig.dist")
    
    ## RUN REGRESSIONS
    reg.spatialCV <- crossValidate(
      dataset = final_net,
      id = "cvID",                           
      dependentVariable = "count_permits19",
      indVariables = reg.sp.vars) %>% 
      dplyr::select(cvID, count_permits19, Prediction, geometry) %>% 
      mutate(error = count_permits19 - Prediction)
    
    # MAE
    reg.spatial.MAE <- mean(abs(reg.spatialCV$error)) # ~5.589619
    
    # LOGO-CV
    # redefining crossValidate() to make sure NAs are removed from id list
    
    crossValidate <- function(dataset, id, dependentVariable, indVariables) {
      
      allPredictions <- data.frame()
      # cvID_list <- unique(dataset[[id]])
      cvID_list <- as.character(na.omit(unique(dataset[[id]])))
      
      for (i in cvID_list) {
        
        thisFold <- i
        cat("This hold out fold is", thisFold, "\n")
        
        fold.train <- filter(dataset, dataset[[id]] != thisFold) %>% as.data.frame() %>% 
          dplyr::select(id, geometry, all_of(indVariables),
                        all_of(dependentVariable))
        fold.test  <- filter(dataset, dataset[[id]] == thisFold) %>% as.data.frame() %>% 
          dplyr::select(id, geometry, all_of(indVariables),
                        all_of(dependentVariable))
        
        form_parts <- paste0(dependentVariable, " ~ ", paste0(indVariables, collapse = "+"))
        form <- as.formula(form_parts)
        regression <- glm(form, family = "poisson",
                          data = fold.train %>%
                            dplyr::select(-geometry, -id))
        
        thisPrediction <-
          mutate(fold.test, Prediction = predict(regression, fold.test, type = "response"))
        
        allPredictions <-
          rbind(allPredictions, thisPrediction)
        
      }
      return(st_sf(allPredictions))
    }
    
    # adding neighborhood for LOGO CV, risk factors only
    reg.logoCV <- crossValidate(
      dataset = final_net,
      id = "mapname",
      dependentVariable = "count_permits19",
      indVariables = reg.vars) %>%
      mutate(error = count_permits19 - Prediction)
    
    # MAE
    reg.logo.MAE <- mean(abs(reg.logoCV$error)) # 8.290278
    
    
    # adding neighborhood for LOGO CV, risk factors + spatial process
    reg.logo.spatialCV <- crossValidate(
      dataset = final_net,
      id = "mapname",                           
      dependentVariable = "count_permits19",
      indVariables = reg.sp.vars) %>% 
      dplyr::select(cvID = mapname, count_permits19, Prediction, geometry) %>% 
      mutate(error = count_permits19 - Prediction)
    
    # MAE
    reg.logo.spatial.MAE <- mean(abs(reg.logo.spatialCV$error)) # 6.158161
    
  }
  
  # map of MAE
  # TO DO: add observed count of permits as well
  {
    reg.summary <- rbind(
      mutate(reg.CV,
             Error = Prediction - count_permits19,
             Regression = "Random k-fold CV: Predictors"),
      mutate(reg.spatialCV,
             Error = Prediction - count_permits19,
             Regression = "Random k-fold CV: Predictors with Spatial Process"),
      mutate(reg.logoCV, 
             Error = Prediction - count_permits19,
             Regression = "Spatial LOGO-CV: Predictors"),
      mutate(reg.logo.spatialCV, 
             Error = Prediction - count_permits19,
             Regression = "Spatial LOGO-CV: Predictors with Spatial Process")) %>%
      st_sf() 
    
    error_by_reg_and_fold <- 
      reg.summary %>%
      group_by(Regression, cvID) %>% 
      summarize(Mean_Error = mean(Prediction - count_permits19, na.rm = T),
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
      row_spec(2, bold = T, color = "white", background = viridis::cividis(1)) %>% 
      kable_classic(full_width = F, html_font = "Cambria")
    
  }
  
  # mean error by neighborhood racial context
  {
    RaceContext <- tracts19 %>% 
      dplyr::select(GEOID,total_pop,RaceContext,geometry) %>% 
      .[nhoods,]
    
    reg.summary %>% 
      st_centroid() %>%
      st_join(tracts19 %>% dplyr::select(RaceContext, geometry)) %>%
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


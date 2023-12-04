# Data Exploration for Final project (MUSA5080)

# Benjamin Myers, 11/28/23

# Load packages
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

# Permit Reading
{
permits = st_read("https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+permits&filename=permits&format=geojson&skipfields=cartodb_id")
}
# Neighborhood Reading
{
phl.nh <- st_read("https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson") %>%
  st_transform(crs=2272) 
}

# Split by date
{
  for 
  
  
  
}
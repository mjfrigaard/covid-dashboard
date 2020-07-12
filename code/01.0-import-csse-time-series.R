#=====================================================================#
# This is code to create: mjfrigaard
# Authored by and feedback to:
# MIT License
# Version: 01.0
#=====================================================================#

# ‹(•_•)› PACKAGES ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#                   _
#                  | |
#  _ __   __ _  ___| | ____ _  __ _  ___  ___
# | '_ \ / _` |/ __| |/ / _` |/ _` |/ _ \/ __|
# | |_) | (_| | (__|   < (_| | (_| |  __/\__ \
# | .__/ \__,_|\___|_|\_\__,_|\__, |\___||___/
# | |                          __/ |
# |_|                         |___/

library(flexdashboard)
library(readr)
library(leaflet)
library(DT)
library(tidyverse)
library(lubridate)
library(plotly)
library(usmap)
library(spData)
library(ggmap)
library(hrbrthemes)
library(geofacet)
library(socviz)


# ‹(•_•)› IMPORT ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#  _                            _
# (_)_ __ ___  _ __   ___  _ __| |_
# | | '_ ` _ \| '_ \ / _ \| '__| __|
# | | | | | | | |_) | (_) | |  | |_
# |_|_| |_| |_| .__/ \___/|_|   \__|
#             |_|

# import TSConfirmedRaw ----
TSConfirmedRaw <- readr::read_csv(
  file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

# import TSRecoveredRaw ----
TSRecoveredRaw <- readr::read_csv(
  file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

# import TSDeathsRaw ----
TSDeathsRaw <- readr::read_csv(
  file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

# import TSConfirmedUSRaw ----
TSConfirmedUSRaw <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

# export TSDeathsUSRaw  ----
TSDeathsUSRaw <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

# import GDPRaw ----
# import gdp from https://github.com/datasets/gdp
GDPRaw <- readr::read_csv("https://raw.githubusercontent.com/datasets/gdp/master/data/gdp.csv")


# ‹(•_•)› EXPORT ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#                             _
#   _____  ___ __   ___  _ __| |_
#  / _ \ \/ / '_ \ / _ \| '__| __|
# |  __/>  <| |_) | (_) | |  | |_
#  \___/_/\_\ .__/ \___/|_|   \__|
#           |_|

tday <- lubridate::today()
# create a folder for today's data
raw_data_path_tday <- paste0("data/raw/", base::noquote(tday), "/")
# raw_data_path_tday
# create data path
fs::dir_create(raw_data_path_tday)
# verify
# fs::dir_tree("data/raw/", recurse = FALSE)

# create a list of raw data files
covid_raw_data_files <- list(
    "GDPRaw" = GDPRaw,
    "TSConfirmedRaw" = TSConfirmedRaw,
    "TSConfirmedUSRaw" = TSConfirmedUSRaw,
    "TSDeathsRaw" = TSDeathsRaw,
    "TSDeathsUSRaw" = TSDeathsUSRaw,
    "TSRecoveredRaw" = TSRecoveredRaw)
# write function for exporting data
output_csv <- function(data, names){ 
  
    require(fs)
    require(readr)
    require(purrr)
  
    # today
    tday <- lubridate::today()
  
    # raw data path
    raw_data_path_tday <- paste0("data/raw/", base::noquote(tday), "/")
    
    # export the data, into the raw data folder, with a date stamp
    readr::write_csv(data, base::paste0(raw_data_path_tday, base::noquote(tday), "-", names, ".csv"))
    
  }

list(data = covid_raw_data_files,
     names = names(covid_raw_data_files)) %>% 
     purrr::pmap(output_csv) %>% 
     purrr::quietly()

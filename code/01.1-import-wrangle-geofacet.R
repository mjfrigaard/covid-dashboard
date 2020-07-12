#=====================================================================#
# This is code to create: 01.1-import-wrangle-geofacet.R
# Authored by and feedback to:
# MIT License
# Version: 01.1
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
library(covdata)


# ‹(•_•)› IMPORT ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#  _                            _
# (_)_ __ ___  _ __   ___  _ __| |_
# | | '_ ` _ \| '_ \ / _ \| '__| __|
# | | | | | | | |_) | (_) | |  | |_
# |_|_| |_| |_| .__/ \___/|_|   \__|
#             |_|
Covus <- covdata::covus

# ‹(•_•)› WRANGLE ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#                                 _
# __      ___ __ __ _ _ __   __ _| | ___
# \ \ /\ / / '__/ _` | '_ \ / _` | |/ _ \
#  \ V  V /| | | (_| | | | | (_| | |  __/
#   \_/\_/ |_|  \__,_|_| |_|\__, |_|\___|
#                           |___/

Covus <- Covus %>% 
  
    dplyr::group_by(state) %>% 
  
    dplyr::mutate(date = lubridate::date(date),
                  # day
                  day = lubridate::day(date),
                  # year 
                  yr = lubridate::year(date), 
                  # week_year (with floor_date)\
                  week = lubridate::week(date),
                  
                  week_year = lubridate::floor_date(date, 
                                                    unit = "week"),
                  # month with label
                  month_lbl = lubridate::month(date, label = TRUE),
                  # month no label
                  month = lubridate::month(date, label = FALSE),
                   # floor_month
                  floor_month = lubridate::floor_date(week_year, 
                                                    unit = "month"),
                  # quarter 
                  qtr = lubridate::quarter(date),
                  
                  # days elapsed
                  days_elapsed = date - min(date)) %>% 
  # ungroup
  dplyr::ungroup()

MapCovus <- Covus %>% 
  dplyr::filter(state %nin% c("AS", "GU", "MP", "PR", "VI")) %>% 
  dplyr::select(date,
                state, 
                fips,
                measure,
                data_quality_grade,
                count,
                day,
                days_elapsed,
                yr, 
                week,
                week_year,
                month_lbl,
                month,
                floor_month)

# PosMapCovus -------------------------------------------------------------
# create dataset with postitive tests from COVID tracking
# ‹(•_•)› WRANGLE ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#                                 _
# __      ___ __ __ _ _ __   __ _| | ___
# \ \ /\ / / '__/ _` | '_ \ / _` | |/ _ \
#  \ V  V /| | | (_| | | | | (_| | |  __/
#   \_/\_/ |_|  \__,_|_| |_|\__, |_|\___|
#                           |___/

PosMapCovus <- MapCovus %>% 
  dplyr::filter(measure == "positive") %>% 
  # rename the generic count var
  dplyr::rename(`positive tests` = count) %>% 
  # sort by state
  dplyr::arrange(desc(state)) %>% 
  # group by state
  dplyr::group_by(state) %>% 
  # create 7 day average
  dplyr::mutate(`positive tests (7-day-avg)` = zoo::rollmean(`positive tests`, 
                                                                 k = 7, 
                                                                 fill = NA)) %>% 
  # ungroup
  dplyr::ungroup() 


# TidyPosMapCovus ---------------------------------------------------------
# tidy positive test metrics
TidyPosMapCovus <- PosMapCovus %>% 
# ‹(•_•)› TIDY ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#  _   _     _
# | |_(_) __| |_   _
# | __| |/ _` | | | |
# | |_| | (_| | |_| |
#  \__|_|\__,_|\__, |
#              |___/

  # tidy
  tidyr::pivot_longer(names_to = "roll_avg_key", 
                      values_to = "roll_avg_value", 
                      cols = c(`positive tests`,
                               `positive tests (7-day-avg)`))
# rename the roll_avg_key 
TidyPosMapCovus <- TidyPosMapCovus %>% 
  dplyr::rename(`Positive Test Metric` = roll_avg_key,
                `Positive Test Value` = roll_avg_value)



# NYTCovState -------------------------------------------------------------
NYTCovState <- covdata::nytcovstate 
NYTCovState <- NYTCovState %>% 
  dplyr::select(
    date,
    fips,
    state_name = state,
    `NYT cases` = cases,
    `NYT deaths` = deaths)


# DeathsMapCovus ---------------------------------------------------------
# filter to deaths

DeathsMapCovus <- MapCovus %>% 
  dplyr::filter(measure == "death") %>% 
  dplyr::rename(`COVID tracking deaths` = count)

# ‹(•_•)› JOIN ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#    _       _
#   (_) ___ (_)_ __
#   | |/ _ \| | '_ \
#   | | (_) | | | | |
#  _/ |\___/|_|_| |_|
# |__/

TidyCovDeathData <- NYTCovState %>% 
  dplyr::inner_join(x = ., 
                    y = DeathsMapCovus, 
                    by = c("date", "fips")) %>% 

  # replace missing NAs with 0
  dplyr::mutate(`COVID tracking deaths` = replace_na(`COVID tracking deaths`, 
                                                     replace = 0)) %>% 
# ‹(•_•)› TIDY ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#  _   _     _
# | |_(_) __| |_   _
# | __| |/ _` | | | |
# | |_| | (_| | |_| |
#  \__|_|\__,_|\__, |
#              |___/

  # tidy
    tidyr::pivot_longer(names_to = 'Death Key', 
                        values_to = 'Death Value', 
                      cols = c(`NYT deaths`,
                               `COVID tracking deaths`)) %>% 
  
    dplyr::select(date, fips, 
                  dplyr::contains("state"),
                  dplyr::contains("Death"),
                  day, 
                  days_elapsed, 
                  yr, 
                  week,
                  week_year,
                  month_lbl, 
                  month,
                  floor_month)

TidyCovCaseData <- MapCovus %>% 
  dplyr::filter(measure == "positive") %>% 
  # rename the generic count var
  dplyr::rename(`COVID tracking positive tests` = count) %>% 
  dplyr::inner_join(x = ., 
                    y = NYTCovState, 
                    by = c("date", "fips")) %>% 
  tidyr::pivot_longer(names_to = 'Cases Key', 
                        values_to = 'Cases Value', 
                      cols = c(`NYT cases`,
                               `COVID tracking positive tests`)) %>% 
    dplyr::select(date,
                 state, 
                 fips,
                 `Cases Key`,
                 `Cases Value`,
                 day:state_name)
   


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

# create data path
fs::dir_create(raw_data_path_tday)
# verify
# fs::dir_tree("data/raw/", recurse = FALSE)

# create a list of raw data files
covidata_files <- list("Covus" = Covus, 
  "DeathsMapCovus" = DeathsMapCovus, 
  "MapCovus" = MapCovus, 
  "NYTCovState" = NYTCovState, 
  "PosMapCovus" = PosMapCovus, 
  "TidyCovDeathData" = TidyCovDeathData, 
  "TidyCovCaseData" = TidyCovCaseData,
  "TidyPosMapCovus" = TidyPosMapCovus)

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

list(data = covidata_files,
     names = names(covidata_files)) %>% 
     purrr::pmap(output_csv) %>% 
     purrr::quietly()



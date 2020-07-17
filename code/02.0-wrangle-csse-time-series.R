#=====================================================================#
# This is code to create: 02.0-wrangle-csse-time-series.R
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


# ‹(•_•)› TIDY ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#  _   _     _
# | |_(_) __| |_   _
# | __| |/ _` | | | |
# | |_| | (_| | |_| |
#  \__|_|\__,_|\__, |
#              |___/

# Convert wide to long (`Confirmed`, `Recovered`, `Deaths`)  --------------
# Confirmed ----
Confirmed <- TSConfirmedRaw %>%
  tidyr::pivot_longer(cols = -c(`Province/State`,
                                `Country/Region`,
                                Lat,
                                Long),
                      names_to = "Date",
                      values_to = "Confirmed") %>%
  dplyr::mutate(Date = lubridate::mdy(Date)) %>%
  janitor::clean_names(case = "snake")
rm(TSConfirmedRaw)
# Recovered ----
Recovered <- TSRecoveredRaw %>%
  tidyr::pivot_longer(cols = -c(`Province/State`,
                                `Country/Region`,
                                Lat,
                                Long),
                      names_to = "Date",
                      values_to = "Recovered") %>%
  dplyr::mutate(Date = mdy(Date)) %>%
  janitor::clean_names(case = "snake")
rm(TSRecoveredRaw)
# Deaths ----
Deaths <- TSDeathsRaw %>%
  tidyr::pivot_longer(cols = -c(`Province/State`,
                                `Country/Region`,
                                Lat,
                                Long),
               names_to = "Date",
               values_to = "Deaths") %>%
  dplyr::mutate(Date = mdy(Date)) %>%
  janitor::clean_names(case = "snake")

# ‹(•_•)› JOIN ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#    _       _
#   (_) ___ (_)_ __
#   | |/ _ \| | '_ \
#   | | (_) | | | | |
#  _/ |\___/|_|_| |_|
# |__/

# `WorldTSDataAll` = join `Confirmed`, `Recovered`, `Deaths` --------------
WorldTSDataAll <- Confirmed %>%
  dplyr::left_join(x = ., y = Recovered,
                   by = c("province_state",
                          "country_region",
                          "lat", "long",
                          "date")) %>%
  dplyr::mutate(recovered = replace_na(recovered,
                                       replace = 0)) %>%
  dplyr::left_join(x = ., y = Deaths,
                   by = c("province_state",
                          "country_region",
                          "lat", "long",
                          "date")) %>%
  dplyr::mutate(deaths = replace_na(deaths,
                                    replace = 0))


# ‹(•_•)› TIDY ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#  _   _     _
# | |_(_) __| |_   _
# | __| |/ _` | | | |
# | |_| | (_| | |_| |
#  \__|_|\__,_|\__, |
#              |___/
#  tidy US confirmed and deaths data ----
# ConfirmedUS ----
ConfirmedUS <- TSConfirmedUSRaw %>%
  dplyr::select(-c(UID, iso2, iso3, code3, Admin2),
                `Province_State`, `Country_Region`,
                Combined_Key, FIPS, Lat, Long_) %>%
  tidyr::pivot_longer(cols = -c(`Province_State`,
                                `Country_Region`,
                                Combined_Key,
                                FIPS,
                                Lat,
                                Long_),
                      names_to = "Date",
                      values_to = "Confirmed") %>%
  dplyr::mutate(Date = lubridate::mdy(Date)) %>%
  janitor::clean_names(case = "snake")
# DeathsUS ----
DeathsUS <- TSDeathsUSRaw %>%
  dplyr::select(-c(UID, iso2, iso3, code3, Admin2),
                `Province_State`, `Country_Region`,
                Combined_Key, FIPS, Population, Lat, Long_) %>%
  tidyr::pivot_longer(cols = -c(`Province_State`,
                                `Country_Region`,
                                Combined_Key,
                                FIPS,
                                Population,
                                Lat,
                                Long_),
                      names_to = "Date",
                      values_to = "Deaths") %>%
  dplyr::mutate(Date = lubridate::mdy(Date)) %>%
  janitor::clean_names(case = "snake")
# base::dput(lubridate::intersect(x = names(ConfirmedUS),
#                                 y = names(DeathsUS)))
# ‹(•_•)› JOIN ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#    _       _
#   (_) ___ (_)_ __
#   | |/ _ \| | '_ \
#   | | (_) | | | | |
#  _/ |\___/|_|_| |_|
# |__/
# `USTSDataAll` = join `ConfirmedUS` and `DeathsUS` -------------------
USTSDataAll <- ConfirmedUS %>%
  dplyr::left_join(x = ., 
                   y = DeathsUS, 
                   by = c("fips", "province_state",
                             "country_region", "lat",
                             "long", "combined_key",
                             "date")) %>% 
  # Create `New Case`
  dplyr::mutate("New Case" = confirmed - lag(confirmed, 1)) %>% 
  # We want `country_region` to just be named `country`, and `province_state`
  # to just be named `state`
  dplyr::rename(
    state = province_state,
    country = country_region)


# SumUSDataMap ------------------------------------------------------------
# this creates the most recent summary of the confirmed cases and deaths 
# in the US
SumUSDataMap <- USTSDataAll %>% 
  dplyr::group_by(state, date) %>% 
  dplyr::summarize(
    new_case_sum = sum(`New Case`),
    confirmed_sum = sum(confirmed),
    deaths_sum = sum(deaths)) %>% 
   dplyr::filter(date == max(date))
# dput(setdiff(x = SumUSDataMap$state, y = state.name))
SumUSDataMap <- SumUSDataMap %>% 
  dplyr::filter(state %nin% c("American Samoa", 
                              "Diamond Princess", 
                              "District of Columbia",
                              "Grand Princess", 
                              "Guam", 
                              "Northern Mariana Islands", 
                              "Puerto Rico", 
                              "Virgin Islands"))

# create named vector for confirmed ----
confirmed_us <- SumUSDataMap %>% 
  select(state, confirmed_sum) %>% 
  tibble::deframe()
# class(confirmed_us)
# length(confirmed_us)
# create named vector for deaths ----
deaths_us <- SumUSDataMap %>% 
  select(state, deaths_sum) %>% 
  tibble::deframe()
# class(deaths_us)
# length(deaths_us)

# create named vector for new cases ----
new_case_us <- SumUSDataMap %>% 
  select(state, new_case_sum) %>% 
  tibble::deframe()
# class(new_case_sum)
# length(new_case_sum)

state.name <- state.name
state.abb <- state.abb

# ‹(•_•)› EXPORT ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#                             _
#   _____  ___ __   ___  _ __| |_
#  / _ \ \/ / '_ \ / _ \| '__| __|
# |  __/>  <| |_) | (_) | |  | |_
#  \___/_/\_\ .__/ \___/|_|   \__|
#           |_|

tday <- lubridate::today()
# create a folder for today's data
processed_data_path_tday <- paste0("data/processed/", base::noquote(tday), "/")
processed_data_path_tday
# create data path
fs::dir_create(processed_data_path_tday)
# verify
# fs::dir_tree("data/processed/", recurse = FALSE)

# create a list of raw data files
covid_us_data_files <- list(
    "ConfirmedUS" = ConfirmedUS,
    "DeathsUS" = DeathsUS,
   "USTSDataAll" = USTSDataAll,
   "SumUSDataMap" = SumUSDataMap)

output_csv <- function(data, names){ 
  
    require(fs)
    require(readr)
    require(purrr)
  
    # today
    tday <- lubridate::today()
  
    # processed data path
    processed_data_path_tday <- paste0("data/processed/", base::noquote(tday), "/")
    
    write_csv(data, base::paste0(processed_data_path_tday, base::noquote(tday), "-", names, ".csv"))
    
  }

list(data = covid_us_data_files,
     names = names(covid_us_data_files)) %>% 
     purrr::pmap(output_csv) %>% 
     purrr::quietly()


# ‹(•_•)› WRANGLE ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#                                 _
# __      ___ __ __ _ _ __   __ _| | ___
# \ \ /\ / / '__/ _` | '_ \ / _` | |/ _ \
#  \ V  V /| | | (_| | | | | (_| | |  __/
#   \_/\_/ |_|  \__,_|_| |_|\__, |_|\___|
#                           |___/
# `SumRegionDate` = group by `country_region` and `date` -----------------------
# recent country_region by region
SumRegionDate <- WorldTSDataAll %>%
  # group by the region and date
  dplyr::group_by(country_region, date) %>%
  # summarize confirmed, recovered, and dead
  dplyr::summarize(
    confirmed_sum = sum(confirmed),
    recovered_sum = sum(recovered),
    deaths_sum = sum(deaths)) %>%
  # create new case
  dplyr::mutate("New Case" = confirmed_sum -
                  dplyr::lag(confirmed_sum, 1)) %>%
  # get max date
  dplyr::filter(date == max(date)) %>% 
  # rename USA
  dplyr::mutate(country_region = case_when(
  country_region == "US" ~ "USA",
  TRUE ~ country_region)) %>% 
   # remove non-physical locations
  dplyr::filter(country_region != "Diamond Princess") %>%
  dplyr::filter(country_region != "MS Zaandam")

# most recent day
recent_day <- max(SumRegionDate$date)
# recent_day

# ‹(•_•)› WRANGLE ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#                                 _
# __      ___ __ __ _ _ __   __ _| | ___
# \ \ /\ / / '__/ _` | '_ \ / _` | |/ _ \
#  \ V  V /| | | (_| | | | | (_| | |  __/
#   \_/\_/ |_|  \__,_|_| |_|\__, |_|\___|
#                           |___/
### GDP Country Codes ----------------------------------------------------------
# filter to 2016 data
Gdp2016 <- GDPRaw %>%
  dplyr::select(region = `Country Name`,
                code = `Country Code`,
                year = Year) %>%
  dplyr::filter(year == 2016) %>% 
  dplyr::mutate(country_region = case_when(
   stringr::str_detect(string = region, pattern = "United States") ~ "USA",
   stringr::str_detect(string = region, pattern = "Macedonia") ~ "North Macedonia",
   stringr::str_detect(string = region, pattern = "Czech Republic") ~ "Czechia",
   stringr::str_detect(string = region, pattern = "Congo, Dem. Rep.") ~ "Congo (Kinshasa)",
   stringr::str_detect(string = region, pattern = "Congo, Rep") ~ "Congo (Brazzaville)",
   stringr::str_detect(string = region, pattern = "Bahamas, The") ~ "Bahamas",
   stringr::str_detect(string = region, pattern = "Swaziland") ~ "Eswatini",
   stringr::str_detect(string = region, pattern = "Gambia, The") ~ "Gambia",
   TRUE ~ region))



# ‹(•_•)› JOIN ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#    _       _
#   (_) ___ (_)_ __
#   | |/ _ \| | '_ \
#   | | (_) | | | | |
#  _/ |\___/|_|_| |_|
# |__/
# `SumRegionDateCodes` = join `SumRegionDate` and `Gdp2016` -----------
# Join the `SumRegionDate` to the `Gdp2016` data country.
SumRegionDateCodes <- SumRegionDate %>%
  dplyr::left_join(x = ., y = Gdp2016,
                   by = "country_region") %>%
  dplyr::arrange(desc(confirmed_sum)) %>% 
  dplyr::select(-year)
SumRegionDateCodes <- SumRegionDateCodes %>% 
  dplyr::rename(`Confirmed` = confirmed_sum,
                `New Cases` = `New Case`,
                `Recovered` = recovered_sum,
                `Deaths` = deaths_sum)


# ‹(•_•)› EXPORT ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#                             _
#   _____  ___ __   ___  _ __| |_
#  / _ \ \/ / '_ \ / _ \| '__| __|
# |  __/>  <| |_) | (_) | |  | |_
#  \___/_/\_\ .__/ \___/|_|   \__|
#           |_|
# Export world map data  -----
covid_world_map_data_files <- list(
    "WorldTSDataAll" = WorldTSDataAll,
    "Gdp2016" = Gdp2016,
   "SumRegionDate" = SumRegionDate,
   "SumRegionDateCodes" = SumRegionDateCodes)

output_csv <- function(data, names){ 
  
    require(fs)
    require(readr)
    require(purrr)
  
    # today
    tday <- lubridate::today()
  
    # processed data path
    processed_data_path_tday <- paste0("data/processed/", base::noquote(tday), "/")
    
    readr::write_csv(data, base::paste0(processed_data_path_tday, base::noquote(tday), "-", names, ".csv"))
    
  }

purrr::quietly(list(data = covid_world_map_data_files, 
                    names = names(covid_world_map_data_files)) %>% 
                    purrr::pmap(output_csv))

# ‹(•_•)› WRANGLE ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#                                 _
# __      ___ __ __ _ _ __   __ _| | ___
# \ \ /\ / / '__/ _` | '_ \ / _` | |/ _ \
#  \ V  V /| | | (_| | | | | (_| | |  __/
#   \_/\_/ |_|  \__,_|_| |_|\__, |_|\___|
#                           |___/

# `WorldTSDataAllDate` = group `WorldTSDataAll` by `date` --------------------
# This groups by the `date` column, the summarized the `confirmed`, `deaths`, 
# and `recovered`.
WorldTSDataAllDate <- WorldTSDataAll %>%
  # change this to region
  dplyr::rename(region = country_region) %>%
  # group by dates
  dplyr::group_by(date) %>%
  # get summary variables
  dplyr::summarize(
    confirmed_sum = sum(confirmed),
    deaths_sum = sum(deaths),
    recovered_sum = sum(recovered)
  ) %>%
  # create new case with lag
  dplyr::mutate("New Case" = confirmed_sum - dplyr::lag(confirmed_sum, 1))

# ‹(•_•)› TIDY ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#  _   _     _
# | |_(_) __| |_   _
# | __| |/ _` | | | |
# | |_| | (_| | |_| |
#  \__|_|\__,_|\__, |
#              |___/

# Create `WorldTSDataAllDateLong` from `WorldTSDataAllDate` ` ------------------
# restructure (pivot) to create `WorldTSDataAllDateLong`.
WorldTSDataAllDateLong <- WorldTSDataAllDate %>%
  dplyr::select(-c(`New Case`),
         `Confirmed` = confirmed_sum,
         `Deaths` = deaths_sum,
         `Recovered` = recovered_sum) %>%
  tidyr::pivot_longer(cols = -date,
               names_to = "status",
               values_to = "cases")


# ‹(•_•)› EXPORT ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#                             _
#   _____  ___ __   ___  _ __| |_
#  / _ \ \/ / '_ \ / _ \| '__| __|
# |  __/>  <| |_) | (_) | |  | |_
#  \___/_/\_\ .__/ \___/|_|   \__|
#           |_|

# export these files to processed folder ----
world_cum_point_chart_files <- list(
"WorldTSDataAllDateLong" = WorldTSDataAllDateLong,
"WorldTSDataAllDate" = WorldTSDataAllDate)

# create function for exporting data
output_csv <- function(data, names){ 
  
    require(fs)
    require(readr)
    require(purrr)
  
    # today
    tday <- lubridate::today()
  
    # processed data path
    processed_data_path_tday <- paste0("data/processed/", base::noquote(tday), "/")
    
    readr::write_csv(data, base::paste0(processed_data_path_tday, base::noquote(tday), "-", names, ".csv"))
    
  }

purrr::quietly(list(data = world_cum_point_chart_files,
                    names = names(world_cum_point_chart_files)) %>% 
                 
                 purrr::pmap(output_csv))


# ‹(•_•)› TIDY ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#  _   _     _
# | |_(_) __| |_   _
# | __| |/ _` | | | |
# | |_| | (_| | |_| |
#  \__|_|\__,_|\__, |
#              |___/

# WorldTSIncrementLong -----
WorldTSIncrementLong <- WorldTSDataAllDate %>%
  dplyr::select(-`New Case`) %>%
  dplyr::mutate(
    `Confirmed` = confirmed_sum - lag(confirmed_sum, 1),
    `Deaths` = deaths_sum - lag(deaths_sum, 1),
    `Recovered` = recovered_sum - lag(recovered_sum, 1)) %>%
  dplyr::filter(date != min(date)) %>%
  tidyr::pivot_longer(cols = c(`Confirmed`, `Deaths`, `Recovered`),
               names_to = "case",
               values_to = "increment")

## USA data (animated) = `WorldTSDataUS`-----
# Here we filter the `WorldTSDataAll` to only the US (`WorldTSDataUS`) and 
# rename `province_state` and `country_region`.
# - only `country_region` == `"US"`

# WorldTSDataUS ----
WorldTSDataUS <- WorldTSDataAll %>%
  dplyr::filter(country_region == "US") %>%
  dplyr::rename(
    state = province_state,
    country = country_region) %>%
  dplyr::mutate("New Case" = confirmed - lag(confirmed, 1))

# ‹(•_•)› TIDY ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#  _   _     _
# | |_(_) __| |_   _
# | __| |/ _` | | | |
# | |_| | (_| | |_| |
#  \__|_|\__,_|\__, |
#              |___/

# USTSDataAllIncrementLong ----
# create an incremental dataset for US states by grouping by `state`, 
# calculating the `lag` (between `metric - dplyr::lag(metric)`), then 
# summarizing by `date`.
USTSDataAllIncrementLong <- WorldTSDataUS %>%
  dplyr::select(date, confirmed, recovered, deaths, state) %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(
    confirmed_lag = confirmed - lag(confirmed, 1),
    recovered_lag = recovered - lag(recovered, 1),
    deaths_lag = deaths - lag(deaths, 1)) %>%
  dplyr::filter(date != min(date)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(
    Confirmed = sum(confirmed_lag),
    Recovered = sum(recovered_lag),
    Deaths = sum(deaths_lag)) %>%
  tidyr::pivot_longer(-date,
               names_to = "case",
               values_to = "increment")


# Create valueBox data (WorldTSRecent & SumUSRecentCountry) -------------------
WorldTSRecent <- WorldTSDataAllDate %>%
  filter(date == max(date))

# these are for the valueBoxes
SumUSRecentCountry <- WorldTSDataUS %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::group_by(country) %>%
  dplyr::summarize(
    date_max = max(date),
    confirmed_sum = sum(confirmed),
    recovered_sum = sum(recovered),
    new_case_sum = sum(`New Case`),
    deaths_sum = sum(deaths))

# valueBox()s (Tab 1) ----
# create new cases from 'confirmed' 
new_case <- WorldTSDataAll %>% dplyr::filter(confirmed > 0)
# get the first day of the new cases
case_no1 <- base::min(new_case$date)
# create today
tday <- base::Sys.Date()
# get the difference between today and date of first case
days_passed <- tday - case_no1
# the new cases for US from 'confirmed'
us_new_conf <- WorldTSDataUS %>% dplyr::filter(confirmed > 0)
# get the first day of new cases (US)
us_first_case_day <- base::min(us_new_conf$date)
# get difference between today and date of first case
us_days_passed <- tday - us_first_case_day

# ‹(•_•)› EXPORT ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#                             _
#   _____  ___ __   ___  _ __| |_
#  / _ \ \/ / '_ \ / _ \| '__| __|
# |  __/>  <| |_) | (_) | |  | |_
#  \___/_/\_\ .__/ \___/|_|   \__|
#           |_|
# export the animated data files

covid_animated_data_files <- list(
    "WorldTSIncrementLong" = WorldTSIncrementLong,
    "WorldTSDataUS" = WorldTSDataUS,
    "WorldTSDataAll" = WorldTSDataAll,
    "USTSDataAllIncrementLong" = USTSDataAllIncrementLong)

output_csv <- function(data, names){ 
  
    require(fs)
    require(readr)
    require(purrr)
  
    # today
    tday <- lubridate::today()
  
    # processed data path
    processed_data_path_tday <- paste0("data/processed/", base::noquote(tday), "/")
    
    readr::write_csv(data, base::paste0(processed_data_path_tday, base::noquote(tday), "-", names, ".csv"))
    
  }

purrr::quietly(list(data = covid_animated_data_files, 
                    names = names(covid_animated_data_files)) %>% 
                 purrr::pmap(output_csv))


# CLEAN UP DATA FILES  ----------------------------------------------------

rm(TSDeathsRaw)
# remove tidy datasets
rm(list = c("Confirmed", "Recovered", "Deaths"))
# remove raw us data
rm(list = c("TSConfirmedUSRaw", "TSDeathsUSRaw"))
# export GDPRaw
rm(GDPRaw)


# Old map data  -----------------------------------------------------------
# Here we remove the non-physical locations, and any weird `lat` and `long`
# USMapData
# library(socviz) # for %nin%
# USMapData <- USTSDataAll %>%
#   dplyr::filter(state %nin% c("American Samoa", "Diamond Princess",
#                        "Grand Princess", "Guam",
#                        "Northern Mariana Islands", "Puerto Rico",
#                        "Virgin Islands")) %>%
#   dplyr::filter(lat != 0.00000 & long != 0.00000)

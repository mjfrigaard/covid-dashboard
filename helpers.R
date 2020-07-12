#=====================================================================#
# This is code to create: helpers.R
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



# create tday! -------------------------------------------------------------
# this is new for each day
tday <- lubridate::today()

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


# ‹(•_•)› TIDY ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#  _   _     _
# | |_(_) __| |_   _
# | __| |/ _` | | | |
# | |_| | (_| | |_| |
#  \__|_|\__,_|\__, |
#              |___/

# Convert wide to long (`Confirmed`, `Recovered`, `Deaths`)  --------------
# Confirmed dataset ----
Confirmed <- TSConfirmedRaw %>%
  tidyr::pivot_longer(cols = -c(`Province/State`,
                                `Country/Region`,
                                Lat,
                                Long),
                      names_to = "Date",
                      values_to = "Confirmed") %>%
  dplyr::mutate(Date = lubridate::mdy(Date)) %>%
  janitor::clean_names(case = "snake")


# Recovered dataset ----
Recovered <- TSRecoveredRaw %>%
  tidyr::pivot_longer(cols = -c(`Province/State`,
                                `Country/Region`,
                                Lat,
                                Long),
                      names_to = "Date",
                      values_to = "Recovered") %>%
  dplyr::mutate(Date = mdy(Date)) %>%
  janitor::clean_names(case = "snake")

# Deaths dataset ----
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

# ‹(•_•)› WRANGLE ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#                                 _
# __      ___ __ __ _ _ __   __ _| | ___
# \ \ /\ / / '__/ _` | '_ \ / _` | |/ _ \
#  \ V  V /| | | (_| | | | | (_| | |  __/
#   \_/\_/ |_|  \__,_|_| |_|\__, |_|\___|
#                           |___/
# SumUSDataMap ------------------------------------------------------------
# this creates the most recent summary of the confirmed cases and deaths 
# in the US
SumUSDataMap <- USTSDataAll %>% 
  dplyr::group_by(state, date) %>% 
  dplyr::summarize(
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


# confirmed_us ------------------------------------------------------------
# create named vector for confirmed 
confirmed_us <- SumUSDataMap %>% 
  select(state, confirmed_sum) %>% 
  tibble::deframe()
# class(confirmed_us)
# length(confirmed_us)
# create named vector for deaths 

# deaths_us ---------------------------------------------------------------

deaths_us <- SumUSDataMap %>% 
  select(state, deaths_sum) %>% 
  tibble::deframe()
# class(deaths_us)
# length(deaths_us)
state.name <- state.name
state.abb <- state.abb


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
# glimpse(SumRegionDateCodes)
SumRegionDateCodes <- SumRegionDateCodes %>% 
  dplyr::rename(Confirmed = confirmed_sum,
                `New Cases` = `New Case`,
                Recovered = recovered_sum,
                Deaths = deaths_sum)




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

# CLEAN UP DATA FILES  ----------------------------------------------------
# remove the raw data files
rm(list = c("TSConfirmedRaw", "TSRecoveredRaw", "TSDeathsRaw", "GDPRaw"))
# remove tidy datasets
rm(list = c("Confirmed", "Recovered", "Deaths"))


# ‹(•_•)› PACKAGES ––•––•––√\/––√\/––•––•––√\/––√\/––•––•––√\/––√\/  ----
#                   _
#                  | |
#  _ __   __ _  ___| | ____ _  __ _  ___  ___
# | '_ \ / _` |/ __| |/ / _` |/ _` |/ _ \/ __|
# | |_) | (_| | (__|   < (_| | (_| |  __/\__ \
# | .__/ \__,_|\___|_|\_\__,_|\__, |\___||___/
# | |                          __/ |
# |_|                         |___/

library(tidyverse)
library(lubridate)
library(plotly)
library(usmap)
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
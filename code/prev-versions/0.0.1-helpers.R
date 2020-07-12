#=====================================================================#
# This is code to create: helpers.R
# Authored by and feedback to:
# MIT License
# Version: 00.1
#=====================================================================#




## ----packages---------------------------------------------------------------------------------
library(knitr)
library(rmdformats)

# dashboard
library(flexdashboard)
library(knitr)
library(DT)

# theme
library(ggthemes)
library(brotools)

# data wrangling
library(tidyverse)
library(lubridate)

# data visualization
library(plotly)
library(gganimate)

# map
library(rmapshaper)
library(sugarbag)


# create raw data folder --------------------------------------------------
fs::dir_create("data/raw/")


# create processed data folder --------------------------------------------
# create processed data folder for today
fs::dir_create(paste0("data/processed/",
                      base::noquote(lubridate::today())))

##  download csse_covid_19_time_series -------------------------------------------------------
# import TSConfirmedRaw ----
TSConfirmedRaw <- readr::read_csv(
  file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

# export TSConfirmedRaw ----
fs::dir_create("data/raw/")
write_csv(x = as.data.frame(TSConfirmedRaw),  
          path = paste0("data/raw/",
                        base::noquote(lubridate::today()),
                            "-TSConfirmedRaw.csv"))

# import TSRecoveredRaw ----
TSRecoveredRaw <- readr::read_csv(
  file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

# export TSRecoveredRaw ----
fs::dir_create("data/raw/")
write_csv(x = as.data.frame(TSRecoveredRaw),  
          path = paste0("data/raw/",
                        base::noquote(lubridate::today()),
                            "-TSRecoveredRaw.csv"))



# import TSDeathsRaw ----
TSDeathsRaw <- readr::read_csv(
  file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

# export TSDeathsRaw ----
# fs::dir_create("data/raw/")
write_csv(x = as.data.frame(TSDeathsRaw),  
          path = paste0("data/raw/",
                        base::noquote(lubridate::today()),
                            "-TSDeathsRaw.csv"))


## ----TSConfirmedUSRaw-------------------------------------------------------------------------
# import TSConfirmedUSRaw ----
TSConfirmedUSRaw <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
# export TSConfirmedUSRaw  ----

write_csv(x = as.data.frame(TSConfirmedUSRaw),  
          path = paste0("data/raw/",
                        base::noquote(lubridate::today()),
                            "-TSConfirmedUSRaw.csv"))
# export TSDeathsUSRaw  ----
TSDeathsUSRaw <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
# export TSDeathsUSRaw  ----
fs::dir_create("data/raw/")
write_csv(x = as.data.frame(TSDeathsUSRaw),  
          path = paste0("data/raw/",
                        base::noquote(lubridate::today()),
                            "-TSDeathsUSRaw.csv"))


## create -> GDPRaw ---------------------------------------------------------------------------
# import gdp from https://github.com/datasets/gdp
GDPRaw <- read_csv("https://raw.githubusercontent.com/datasets/gdp/master/data/gdp.csv")
# export GDPRaw ----
# fs::dir_create("data/raw/")
write_csv(x = as.data.frame(GDPRaw),  
          path = paste0("data/raw/",
                        base::noquote(lubridate::today()),
                            "-GDPRaw.csv"))


## create -> Confirmed--------------------------------------------------------------------------------
Confirmed <- TSConfirmedRaw %>%
  tidyr::pivot_longer(cols = -c(`Province/State`,
                                `Country/Region`,
                                Lat,
                                Long),
                      names_to = "Date",
                      values_to = "Confirmed") %>%
  dplyr::mutate(Date = lubridate::mdy(Date)) %>%
  janitor::clean_names(case = "snake")


## create -> Recovered--------------------------------------------------------------------------------
Recovered <- TSRecoveredRaw %>%
  tidyr::pivot_longer(cols = -c(`Province/State`,
                                `Country/Region`,
                                Lat,
                                Long),
                      names_to = "Date",
                      values_to = "Recovered") %>%
  dplyr::mutate(Date = mdy(Date)) %>%
  janitor::clean_names(case = "snake")


## create -> Deaths-----------------------------------------------------------------------------------
Deaths <- TSDeathsRaw %>%
  tidyr::pivot_longer(cols = -c(`Province/State`,
                                `Country/Region`,
                                Lat,
                                Long),
               names_to = "Date",
               values_to = "Deaths") %>%
  dplyr::mutate(Date = mdy(Date)) %>%
  janitor::clean_names(case = "snake")


## create -> WorldTSDataAll ---------------------------------------------------------------------------
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

# export WorldTSDataAll ----
write_csv(x = as.data.frame(WorldTSDataAll),  
          path = paste0("data/processed/",
                        base::noquote(lubridate::today()),
                        "/",
                        base::noquote(lubridate::today()),
                            "-WorldTSDataAll.csv"))


## create -> SumRegionDate----------------------------------------------------------------------------
# recent country_region by region
SumRegionDate <- WorldTSDataAll %>%
  # group by the region and date
  dplyr::group_by(country_region, date) %>%
  dplyr::summarize(
    confirmed_sum = sum(confirmed),
    recovered_sum = sum(recovered),
    deaths_sum = sum(deaths)) %>%
  dplyr::mutate("New Case" = confirmed_sum -
                  dplyr::lag(confirmed_sum, 1)) %>%
  dplyr::filter(date == max(date))
# rename US to USA
SumRegionDate <- SumRegionDate %>%
  dplyr::mutate(country_region = case_when(
  country_region == "US" ~ "USA",
  TRUE ~ country_region))

# remove non-physical locations
SumRegionDate <- SumRegionDate %>%
  dplyr::filter(country_region != "Diamond Princess") %>%
  dplyr::filter(country_region != "MS Zaandam")


## create -> recent_day-------------------------------------------------------------------------------
# most recent day
recent_day <- max(SumRegionDate$date)


## create -> Gdp2016----------------------------------------------------------------------------------
# filter to 2016 data
Gdp2016 <- GDPRaw %>%
  dplyr::select(region = `Country Name`,
                code = `Country Code`,
                year = Year) %>%
  dplyr::filter(year == 2016)


## create -> country_region ----------------------------------------------------
Gdp2016 <- Gdp2016 %>%
  dplyr::mutate(country_region = case_when(
   stringr::str_detect(string = region, pattern = "United States") ~ "USA",
   stringr::str_detect(string = region, pattern = "Macedonia") ~ "North Macedonia",
   stringr::str_detect(string = region, pattern = "Czech Republic") ~ "Czechia",
   stringr::str_detect(string = region, pattern = "Taiwan") ~ "Taiwan*",
   stringr::str_detect(string = region, pattern = "West Bank") ~ "West Bank and Gaza",
   stringr::str_detect(string = region, pattern = "Congo, Dem. Rep.") ~ "Congo (Kinshasa)",
   stringr::str_detect(string = region, pattern = "Congo, Rep") ~ "Congo (Brazzaville)",
   stringr::str_detect(string = region, pattern = "Bahamas, The") ~ "Bahamas",
   stringr::str_detect(string = region, pattern = "Swaziland") ~ "Eswatini",
   stringr::str_detect(string = region, pattern = "Gambia, The") ~ "Gambia",
   TRUE ~ region))
# check USA
# Gdp2016 %>%
#   dplyr::filter(str_detect(string = region, pattern = "United States"))


## create -> SumRecentRegionCodes---------------------------------------------------------------------
SumRegionDateCodes <- SumRegionDate %>%
  dplyr::left_join(x = ., y = Gdp2016,
                   by = "country_region") %>%
  dplyr::arrange(desc(confirmed_sum))


# export-SumRegionDateCodes-----------------------------------------------------
write_csv(x = as.data.frame(SumRegionDateCodes),  
          path = paste0("data/processed/",
                        base::noquote(lubridate::today()),
                        "/",
                        base::noquote(lubridate::today()),
                            "-SumRegionDateCodes.csv"))

## create -> WorldTSDataAllDate ----------------------------------------------
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
# export WorldTSDataAllDate ----
write_csv(x = as.data.frame(WorldTSDataAllDate),  
          path = paste0("data/processed/",
                        base::noquote(lubridate::today()),
                        "/",
                        base::noquote(lubridate::today()),
                            "-WorldTSDataAllDate.csv"))


## create -> WorldTSDataAllDateLong --------------------------------------------
WorldTSDataAllDateLong <- WorldTSDataAllDate %>%
  select(-c(`New Case`),
         `Confirmed` = confirmed_sum,
         `Deaths` = deaths_sum,
         `Recovered` = recovered_sum) %>%
  pivot_longer(cols = -date,
               names_to = "status",
               values_to = "cases")
# export WorldTSDataAllDateLong ----
write_csv(x = as.data.frame(WorldTSDataAllDateLong),  
          path = paste0("data/processed/",
                        base::noquote(lubridate::today()),
                        "/",
                        base::noquote(lubridate::today()),
                            "-WorldTSDataAllDateLong.csv"))





## create -> WorldTSIncrementLong---------------------------------------------------------------------
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
# WorldTSIncrementLong
# export WorldTSIncrementLong ----
write_csv(x = as.data.frame(WorldTSIncrementLong),  
          path = paste0("data/processed/",
                        base::noquote(lubridate::today()),
                        "/",
                        base::noquote(lubridate::today()),
                            "-WorldTSIncrementLong.csv"))


## create -> WorldTSDataUS -----------------------------------------------------
WorldTSDataUS <- WorldTSDataAll %>%
  dplyr::filter(country_region == "US") %>%
  dplyr::rename(
    state = province_state,
    country = country_region) %>%
  dplyr::mutate("New Case" = confirmed - lag(confirmed, 1))
# export WorldTSDataUS ----
write_csv(x = as.data.frame(WorldTSDataUS),  
          path = paste0("data/processed/",
                        base::noquote(lubridate::today()),
                        "/",
                        base::noquote(lubridate::today()),
                            "-WorldTSDataUS.csv"))


## create -> USTSDataAllIncrementLong ------------------------------------------
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

# export USTSDataAllIncrementLong ----
write_csv(x = as.data.frame(USTSDataAllIncrementLong),  
          path = paste0("data/processed/",
                        base::noquote(lubridate::today()),
                        "/",
                        base::noquote(lubridate::today()),
                            "-USTSDataAllIncrementLong.csv"))
fs::dir_ls(paste0("data/processed/",
                        base::noquote(lubridate::today()),
                        "/"))


## ----ConfirmedUS-DeathsUS-----------------------------------------------------
# create -> ConfirmedUS ----
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

# create -> DeathsUS  ----
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
# create -> USTSDataAll -----
USTSDataAll <- ConfirmedUS %>%
  dplyr::left_join(DeathsUS, by = c("fips", "province_state",
                             "country_region", "lat",
                             "long", "combined_key",
                             "date"))
## ----USTSDataAll new case ----------------------------------------------------
USTSDataAll <- USTSDataAll %>%
  dplyr::mutate("New Case" = confirmed - lag(confirmed, 1))


## ----USTSDataAll rename state country----------------------------------------
USTSDataAll <- USTSDataAll %>%
  dplyr::rename(
    state = province_state,
    country = country_region)

# export USTSDataAll ----
write_csv(x = as.data.frame(USTSDataAll),  
          path = paste0("data/processed/",
                        base::noquote(lubridate::today()),
                        "/",
                        base::noquote(lubridate::today()),
                            "-USTSDataAll.csv"))


## create -> USMapData---------------------------------------------------------
library(socviz) # for %nin%
USMapData <- USTSDataAll %>%
  filter(state %nin% c("American Samoa", "Diamond Princess",
                       "Grand Princess", "Guam",
                       "Northern Mariana Islands", "Puerto Rico",
                       "Virgin Islands")) %>%
  filter(lat != 0.00000 & long != 0.00000)

# export USMapData ----
write_csv(x = as.data.frame(USMapData),  
          path = paste0("data/processed/",
                        base::noquote(lubridate::today()),
                        "/",
                        base::noquote(lubridate::today()),
                            "-USMapData.csv"))
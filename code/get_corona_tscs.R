## code to prepare covdata datasets

library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(socviz)
library(ggrepel)
library(paletteer)

### --------------------------------------------------------------------------------------
### CoronaNet Data
### --------------------------------------------------------------------------------------

## See https://github.com/saudiwin/corona_tscs
## and https://osf.io/preprints/socarxiv/jp4wk

# https://raw.githubusercontent.com/saudiwin/corona_tscs/master/data/CoronaNet/coronanet_release.csv
get_corona_tscs <- function(url = "https://raw.githubusercontent.com/saudiwin/corona_tscs/master/data/CoronaNet",
                            fname = "coronanet_release",
                            date = lubridate::today(),
                            ext = "csv",
                            dest = "data-raw/data",
                            save_file = c("n", "y")) {

  save_file <- match.arg(save_file)
  target <-  paste0(url, "/", fname, ".", ext)
  message("target: ", target)

  destination <- fs::path(here::here("data-raw/data"),
                          paste0("", date), ext = ext)

  tf <- tempfile(fileext = ext)
  curl::curl_download(target, tf)

  switch(save_file,
         y = fs::file_copy(tf, destination),
         n = NULL)

  cn_spec <- cols(
    record_id = col_character(),
    policy_id = col_character(),
    recorded_date = col_datetime(),
    date_announced = col_datetime(),
    date_start = col_date(),
    date_end = col_date(),
    entry_type = col_character(),
    event_description = col_character(),
    domestic_policy = col_integer(),
    type = col_character(),
    type_sub_cat = col_character(),
    type_text = col_integer(),
    index_high_est = col_double(),
    index_med_est = col_double(),
    index_low_est  = col_double(),
    index_country_rank = col_double(),
    country = col_character(),
    init_country_level = col_character(),
    province = col_character(),
    source_corr_type = col_character(),
    target_country = col_character(),
    target_geog_level = col_character(),
    target_region = col_character(),
    target_province = col_character(),
    target_city = col_character(),
    target_other = col_character(),
    target_other = col_logical(),
    target_direction = col_character(),
    travel_mechanism = col_character(),
    compliance = col_character(),
    enforcer = col_character(),
    link = col_character()
  )


  janitor::clean_names(read_csv(tf, col_types = cn_spec))

}
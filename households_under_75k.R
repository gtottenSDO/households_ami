library(ipumsr)
library(srvyr)
library(tidycensus)
library(duckplyr)
library(hudr)
library(googledrive)
library(sdotools)
acs_extract <- define_extract_micro(
  collection = "usa",
  case_select_who = "households",
  description = "ACS 1 year samples in Colorado of income variables, a2024",
  samples = "us2024a",
  variables = list(
    var_spec("STATEFIP", case_selections = "08"),
    "HHINCOME",
    "REPWT"
  )
) |>
  submit_extract() |>
  wait_for_extract() |>
  download_extract() |>
  read_ipums_micro()

acs_srvy <- acs_extract |>
  rename_with(tolower) |>
  mutate(flag71 = if_else(hhincome <= 71000, TRUE, FALSE)) |>
  filter(
    gq %in% c(1, 2) # households and group quarters
  ) |>
  as_survey_design(
    weight = hhwt,
    repweights = matches("repwt[0-9]+"),
    type = "acs",
    mse = true
  )

acs_srvy |>
  group_by(flag71) |>
  summarize(prop = survey_prop())

acs_srvy |>
  survey_count(flag71)

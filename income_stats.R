srvy <- acs_amis_region |>
  filter(
    year == 2024, # remove years without puma data, skip 2000
    pernum == 1, # one record per household
    gq %in% c(1, 2) # households and group quarters
  ) |>
  mutate(high_inc = if_else(hhincome >= 175000, 1, 0)) |>
  select(
    year,
    region,
    pernum,
    gq,
    hhincome,
    high_inc,
    age,
    ami_group,
    tenure,
    hhwt,
    rent,
    owncost,
    housing_cost,
    cost_burdened_30,
    cost_burdened_50,
    matches("repwt[0-9]+")
  ) |>
  mutate(
    tenure = str_to_lower(str_extract(tenure, "Own|Rent"))
  ) |>
  as_survey_design(
    weight = hhwt,
    repweights = matches("repwt[0-9]+"),
    type = "ACS",
    mse = TRUE
  )


srvy |>
  filter(age >= 24, age <= 65) |>
  summarize(
    ratio = survey_mean(high_inc, vartype = "ci")
  )

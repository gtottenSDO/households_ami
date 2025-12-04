library(tidyverse)
library(readxl)

# create list of excel_files
file_list <- list.files(
    recursive = TRUE,
    pattern = ".xls*",
    include.dirs = TRUE,
    full.names = TRUE
)

income_limits <- map(
    file_list,
    \(x) {
        df <- read_excel(x)

        # Extract year from filename (e.g., "Section8-FY00.xls" -> "2000")
        year_match <- str_extract(basename(x), "FY(\\d{2})")
        year <- as.numeric(paste0("20", str_extract(year_match, "\\d{2}")))

        # Get column names
        cols <- names(df)

        # Find state FIPS code (numeric) - "FIPS STATE CODE", "State", or "state"
        state_fips_col <- cols[grep(
            "(?i)^fips.*state|^state$(?!.*alpha)",
            cols,
            perl = TRUE
        )]
        if (length(state_fips_col) == 0) {
            state_fips_col <- cols[1]
        }
        state_fips_col <- state_fips_col[1]

        # Find state abbreviation (alpha) - "STATE ABREV.", "state_alpha", "StateAlpha"
        state_col <- cols[grep(
            "(?i)state.*abrev|state.*alpha|statealpha",
            cols,
            perl = TRUE
        )]
        if (length(state_col) == 0) {
            state_col <- cols[2]
        }
        state_col <- state_col[1]

        # Find county FIPS - "FIPS COUNTY CODE", "County", "county" (but NOT "countyname" or "county name")
        county_fips_col <- cols[grep(
            "(?i)^fips.*county|^county$(?!.*name)",
            cols,
            perl = TRUE
        )]
        if (length(county_fips_col) == 0) {
            county_fips_col <- cols[3]
        }
        county_fips_col <- county_fips_col[1]

        # Find county name - "COUNTY NAME", "County Name", "countyname", "County_Town_Name"
        county_name_col <- cols[grep(
            "(?i)county.*name|countyname",
            cols,
            perl = TRUE
        )]
        county_name_col <- county_name_col[1]

        # Find MFI column - contains "MEDIAN" or starts with "median"
        mfi_col <- cols[grep("(?i)median|median2", cols, perl = TRUE)]
        mfi_col <- mfi_col[1]

        # Select and rename columns
        df |>
            select(
                state_fips = all_of(state_fips_col),
                state = all_of(state_col),
                county_fips = all_of(county_fips_col),
                county_name = all_of(county_name_col),
                mfi = all_of(mfi_col)
            ) |>
            mutate(
                year = !!year,
                state_fips = str_pad(as.character(state_fips), 2, pad = "0"),
                state = as.character(state),
                county_fips = str_pad(as.character(county_fips), 3, pad = "0"),
                .before = 1
            )
    }
)
hud_il <- bind_rows(income_limits) |>
    arrange(year, state_fips, county_fips)
co_mfi <- hud_il |>
    filter(state_fips == "08")

duckplyr::compute_parquet(hud_il, "hud_income_limits.parquet")

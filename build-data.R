# Build pre-aggregated casualty data from raw STATS19 CSV
# This avoids re-reading the 921 MB file on every render or in CI.
# Output: data/casualties_by_year_type.csv (small, ~200 bytes)

library(tidyverse)
library(readr)
library(stats19)

years <- seq(1979, 2022, by = 5)
src <- "data/dft-road-casualty-statistics-casualty-1979-latest-published-year.csv"

if (!file.exists(src)) {
  stop("Raw STATS19 CSV not found at ", src,
       "\nDownload it with: get_stats19(year = 1979, type = \"cas\", data_dir = \"data\")")
}

# Read once
message("Reading ", src, " ...")
cas <- read_csv(src, show_col_types = FALSE)

# Filter to sample years
cas <- cas |> filter(collision_year %in% years)

# Map casualty_type codes to labels using stats19 schema
cas_map <- stats19_schema |>
  filter(variable == "casualty_type") |>
  mutate(code_dbl = as.double(code))

cas <- cas |>
  left_join(cas_map |> select(code_dbl, label), by = c("casualty_type" = "code_dbl")) |>
  mutate(type = case_when(
    label == "Pedestrian" ~ "Pedestrian",
    label == "Cyclist" ~ "Cyclist",
    label %in% c("Car occupant", "Taxi/Private hire car occupant",
                 "Car (including private hire cars) (1979-2004)",
                 "Minibus/Motor caravan (1979-1998)") ~ "Car occupant",
    TRUE ~ "Other"
  ))

# Save aggregates
by_year_type <- cas |> count(collision_year, type, name = "n")
write_csv(by_year_type, "data/casualties_by_year_type.csv")
message("Saved data/casualties_by_year_type.csv (", nrow(by_year_type), " rows, ",
        file.size("data/casualties_by_year_type.csv"), " bytes)")

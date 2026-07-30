# Script to download STATS19 1979-latest datasets and build aggregated data CSVs

library(stats19)
library(tidyverse)

dir.create("data", showWarnings = FALSE)

cat("Downloading STATS19 casualty and vehicle tables...\n")
cas <- get_stats19(year = 1979, type = "cas")
veh <- get_stats19(year = 1979, type = "veh")

# Cars in collision
cars_in_col <- veh |>
  filter(str_detect(vehicle_type, "(?i)Car")) |>
  distinct(collision_index)

# 1. Overall casualties by year and mode
cas_by_year_type <- cas |>
  mutate(type = case_when(
    str_detect(casualty_type, "(?i)Cyclist") ~ "Cyclist",
    str_detect(casualty_type, "(?i)Pedestrian") ~ "Pedestrian",
    str_detect(casualty_type, "(?i)Car") ~ "Car occupant",
    TRUE ~ "Other"
  )) |>
  count(collision_year, type, name = "n")

write_csv(cas_by_year_type, "data/casualties_by_year_type.csv")

# 2. Vulnerable road users hit by cars (Hurt vs Killed)
vru_car_harm <- cas |>
  mutate(
    vru_mode = case_when(
      str_detect(casualty_type, "(?i)Pedestrian") ~ "Pedestrians",
      str_detect(casualty_type, "(?i)Cyclist") ~ "Cyclists",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(vru_mode)) |>
  inner_join(cars_in_col, by = "collision_index") |>
  mutate(
    outcome = if_else(casualty_severity %in% c("Fatal", "1"), "Killed", "Hurt (Injured)")
  ) |>
  count(collision_year, vru_mode, outcome, name = "n")

write_csv(vru_car_harm, "data/vru_car_harm.csv")

# 3. Car engine capacity over time in VRU collisions
car_engine_vru <- veh |>
  filter(str_detect(vehicle_type, "(?i)Car")) |>
  inner_join(
    cas |> filter(str_detect(casualty_type, "(?i)Pedestrian|Cyclist")) |> distinct(collision_index),
    by = "collision_index"
  ) |>
  filter(!is.na(engine_capacity_cc), engine_capacity_cc > 0, engine_capacity_cc < 8000) |>
  group_by(collision_year) |>
  summarise(
    mean_cc = mean(engine_capacity_cc, na.rm = TRUE),
    median_cc = median(engine_capacity_cc, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(car_engine_vru, "data/car_engine_vru.csv")

# 4. Driver sex in car-VRU collisions
driver_sex_vru <- veh |>
  filter(str_detect(vehicle_type, "(?i)Car")) |>
  inner_join(
    cas |> filter(str_detect(casualty_type, "(?i)Pedestrian|Cyclist")) |> distinct(collision_index),
    by = "collision_index"
  ) |>
  filter(str_detect(sex_of_driver, "(?i)Male|Female")) |>
  count(collision_year, sex_of_driver, name = "n")

write_csv(driver_sex_vru, "data/driver_sex_vru.csv")

cat("Aggregated datasets built successfully in data/\n")

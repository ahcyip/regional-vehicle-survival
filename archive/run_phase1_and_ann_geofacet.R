library(tidyverse)
library(patchwork)

# ========== PHASE 1 ==========
message("=== Phase 1: Loading Experian data ===")

process_consecutive_years <- function(year1, year2) {
  data1 <- data_list[[as.character(year1)]] %>% mutate(CY = year1, CY_index = 1)
  data2 <- data_list[[as.character(year2)]] %>% mutate(CY = year2, CY_index = 2)
  bind_rows(data1, data2) %>%
    mutate(Age = CY - vehicle_model_year,
           Age1 = year1 - vehicle_model_year,
           Age2 = year2 - vehicle_model_year)
}

calculate_survival <- function(pair_of_years, region = "natl", fleet_sep = FALSE, tech_sep = FALSE) {
  data <- processed_data_list[[as.character(pair_of_years)]]
  if (fleet_sep & region == "natl" & !tech_sep) {
    data <- data %>% mutate(state_name = "USA", county_name = "USA", Tech = "ALL_TECHS")
  } else if (fleet_sep & region == "state" & !tech_sep) {
    data <- data %>% mutate(county_name = "STATE", Tech = "ALL_TECHS")
  } else if (!fleet_sep & region == "natl" & !tech_sep) {
    data <- data %>% mutate(regtype = "ALL_OWNERS", state_name = "USA", county_name = "USA", Tech = "ALL_TECHS")
  } else if (!fleet_sep & region == "state" & !tech_sep) {
    data <- data %>% mutate(regtype = "ALL_OWNERS", county_name = "STATE", Tech = "ALL_TECHS")
  }
  data %>%
    filter(!is.na(VehicleClass)) %>%
    group_by(regtype, Tech, state_name, county_name, Age1, VehicleClass, CY_index) %>%
    summarize(stock = sum(stock, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = "CY_index", values_from = "stock", names_prefix = "CY") %>%
    mutate(survival = CY2 / CY1)
}

directory <- "c:/users/ayip/OneDrive - NREL (1)/from eagle/experian/"
data_list <- list()
for (year in 2014:2025) {
  message("  Loading ", year, "...")
  file_path <- paste0(directory, "exp_", year, "_stock_interim.csv")
  data_list[[as.character(year)]] <- read_csv(file_path, show_col_types = FALSE)
}

data_list <- data_list %>% map(~ .x %>% mutate(
  VehicleClass = case_when(
    yip_tempo_class %in% c("Compact", "Midsize") ~ "Car",
    yip_tempo_class %in% c("SUV") ~ "SUV",
    yip_tempo_class %in% c("Pickup") ~ "Pickup"),
  state_name = if_else(state_name == "Dist. Of Columbia", "District of Columbia", state_name),
  state_name = if_else(state_name == "District Of Columbia", "District of Columbia", state_name)) %>%
  filter(state_name != "(293838699 rows)" & !is.na(state_name)))

processed_data_list <- list()
for (year in 2014:2024) {
  year_next <- year + 1
  message("  Processing ", year, "-", year_next, "...")
  processed_data_list[[paste0(year, year_next)]] <- process_consecutive_years(year, year_next)
}

year_pairs <- lapply(2014:2024, function(year) paste0(year, year + 1))

message("  Computing survival rates...")
national_survival_list <- year_pairs %>% setNames(year_pairs) %>% map(calculate_survival, region = "natl", fleet_sep = FALSE)
national_survival_list_fleetsep <- year_pairs %>% setNames(year_pairs) %>% map(calculate_survival, region = "natl", fleet_sep = TRUE)
state_survival_list <- year_pairs %>% setNames(year_pairs) %>% map(calculate_survival, region = "state", fleet_sep = FALSE)
state_survival_list_fleetsep <- year_pairs %>% setNames(year_pairs) %>% map(calculate_survival, region = "state", fleet_sep = TRUE)

message("  Saving CSVs...")
surv_data_dir <- "c:/users/ayip/OneDrive - NREL (1)/survival and uvm/data"
dir.create(surv_data_dir, showWarnings = FALSE, recursive = TRUE)

national_survival_list |> bind_rows(.id = "year_pair") |> ungroup() |>
  write_csv(file.path(surv_data_dir, "pipeline_national_survival_14to25.csv"))
national_survival_list_fleetsep |> bind_rows(.id = "year_pair") |> ungroup() |>
  write_csv(file.path(surv_data_dir, "pipeline_national_survival_fleetsep_14to25.csv"))
state_survival_list |> bind_rows(.id = "year_pair") |> ungroup() |>
  write_csv(file.path(surv_data_dir, "pipeline_state_survival_14to25.csv"))
state_survival_list_fleetsep |> bind_rows(.id = "year_pair") |> ungroup() |>
  write_csv(file.path(surv_data_dir, "pipeline_state_survival_fleetsep_14to25.csv"))

national_survival_list |> bind_rows(.id = "yearpair") |> ungroup() |>
  select(yearpair, Age1, VehicleClass, survival) |>
  write_csv(file.path(surv_data_dir, "national_14to25_survival.csv"))
state_survival_list |> bind_rows(.id = "yearpair") |> ungroup() |>
  select(yearpair, state_name, Age1, VehicleClass, survival) |>
  write_csv(file.path(surv_data_dir, "state_14to25_survival.csv"))

message("=== Phase 1 complete ===")

# free memory from raw data
rm(data_list, processed_data_list)
gc()

# ========== PHASE 2 (partial) ==========
message("=== Phase 2: Deriving objects ===")

compute_national_aggregate <- function(df) {
  df |>
    filter(year_pair != "20202021", year_pair != "20222023") |>
    group_by(regtype, Age1, VehicleClass) |>
    summarize(CY2 = sum(CY2, na.rm = TRUE), CY1 = sum(CY1, na.rm = TRUE), .groups = "drop") |>
    mutate(survival = CY2 / CY1) |>
    filter(Age1 >= 1) |>
    group_by(regtype, VehicleClass) |>
    arrange(regtype, VehicleClass, Age1) |>
    mutate(cumulative_survival = cumprod(survival))
}

national_surv <- read_csv(file.path(surv_data_dir, "pipeline_national_survival_14to25.csv"), show_col_types = FALSE)
state_surv <- read_csv(file.path(surv_data_dir, "pipeline_state_survival_14to25.csv"), show_col_types = FALSE)

year_pair_levels <- national_surv |> distinct(year_pair) |> pull(year_pair) |> sort()
national_aggregate_allyears <- compute_national_aggregate(national_surv)

n_pairs <- length(year_pair_levels)
year_pair_palette <- scales::viridis_pal(option = "turbo")(n_pairs) |>
  setNames(year_pair_levels)

message("=== Phase 2 complete ===")

# ========== ANNUAL GEOFACET PLOTS ==========
message("=== Generating annual conditional survival geofacets ===")

for (vc in c("Car", "SUV", "Pickup")) {
  message("  Plotting ", vc, "...")
  p <- state_surv |>
    filter(VehicleClass == vc, Age1 >= 1) |>
    mutate(year_pair = factor(year_pair, levels = year_pair_levels)) |>
    ggplot(aes(x = Age1, y = survival, color = year_pair, group = year_pair)) +
    geom_line(alpha = 0.6, linewidth = 0.7) +
    geom_line(
      data = national_aggregate_allyears |> filter(VehicleClass == vc),
      aes(x = Age1, y = survival, group = 1),
      col = "black", linewidth = 1, linetype = "dashed", inherit.aes = FALSE
    ) +
    scale_color_manual(values = year_pair_palette, name = "Year pair") +
    scale_x_continuous(limits = c(NA, 30)) +
    geofacet::facet_geo(facets = "state_name", grid = geofacet::us_state_grid3, label = "name") +
    scale_y_continuous(limits = c(0.7, 1.2)) +
    guides(color = guide_legend(nrow = 1)) +
    labs(
      y = "Annual conditional net survival rate",
      x = "Vehicle age",
      title = paste0("Annual conditional net survival rates for ", vc, "s by state (colored) and national long-term average (excl 20-21 and 22-23) (black dashes)")
      # caption = "Source: Derived from Experian registration data, by Arthur Yip, NLR"
    ) +
    theme(legend.position = "bottom")

  out_path <- paste0("c:/users/ayip/OneDrive - NREL (1)/survival and uvm/ann_surv_", str_to_lower(vc), "_bystate_14to25_allyears.png")
  ggsave(out_path, plot = p, width = 14, height = 10)
  message("  Saved: ", out_path)
}

message("=== All done! ===")

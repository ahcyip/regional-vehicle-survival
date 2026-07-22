library(tidyverse)
library(patchwork)

surv_data_dir <- "c:/users/ayip/OneDrive - NREL (1)/survival and uvm/data"

calculate_cumulative_survival <- function(survival) {
  survival %>%
    filter(Age1 >= 1) %>%
    group_by(regtype, Tech, state_name, county_name, VehicleClass) %>%
    arrange(regtype, Tech, state_name, county_name, VehicleClass, Age1) %>%
    mutate(cumulative_survival = cumprod(survival))
}

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

split_by_year_pair <- function(df) {
  df |> group_split(year_pair) |>
    setNames(map_chr(group_split(df, year_pair), ~ unique(.x$year_pair)))
}

national_surv <- read_csv(file.path(surv_data_dir, "pipeline_national_survival_14to25.csv"))
national_surv_fleetsep <- read_csv(file.path(surv_data_dir, "pipeline_national_survival_fleetsep_14to25.csv"))
state_surv <- read_csv(file.path(surv_data_dir, "pipeline_state_survival_14to25.csv"))
state_surv_fleetsep <- read_csv(file.path(surv_data_dir, "pipeline_state_survival_fleetsep_14to25.csv"))

year_pair_levels <- national_surv |> distinct(year_pair) |> pull(year_pair) |> sort()
national_aggregate_allyears <- compute_national_aggregate(national_surv)
state_survival_list <- split_by_year_pair(state_surv)

n_pairs <- length(year_pair_levels)
year_pair_palette <- scales::viridis_pal(option = "turbo")(n_pairs) |>
  setNames(year_pair_levels)

message("Data loaded, generating plots...")

for (vc in c("Car", "SUV", "Pickup")) {
  message("Plotting ", vc, "...")
  p <- state_survival_list |>
    bind_rows(.id = "year_pair") |>
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

  ggsave(
    paste0("c:/users/ayip/OneDrive - NREL (1)/survival and uvm/ann_surv_", str_to_lower(vc), "_bystate_14to25_allyears.png"),
    plot = p, width = 14, height = 10
  )
  message("Saved ", vc)
}

message("Done!")

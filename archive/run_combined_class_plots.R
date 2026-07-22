Sys.setenv(PATH = paste("C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools", Sys.getenv("PATH"), sep = ";"))
library(tidyverse)
library(patchwork)
library(geofacet)

surv_data_dir <- "c:/users/ayip/OneDrive - NREL (1)/survival and uvm/data"
out_dir <- "c:/users/ayip/OneDrive - NREL (1)/survival and uvm"

message("=== Loading pipeline CSVs ===")
national_surv <- read_csv(file.path(surv_data_dir, "pipeline_national_survival_14to25.csv"), show_col_types = FALSE)
national_surv_fleetsep <- read_csv(file.path(surv_data_dir, "pipeline_national_survival_fleetsep_14to25.csv"), show_col_types = FALSE)
state_surv <- read_csv(file.path(surv_data_dir, "pipeline_state_survival_14to25.csv"), show_col_types = FALSE)
state_surv_fleetsep <- read_csv(file.path(surv_data_dir, "pipeline_state_survival_fleetsep_14to25.csv"), show_col_types = FALSE)

national_surv <- national_surv |> mutate(year_pair = as.character(year_pair))
national_surv_fleetsep <- national_surv_fleetsep |> mutate(year_pair = as.character(year_pair))
state_surv <- state_surv |> mutate(year_pair = as.character(year_pair))
state_surv_fleetsep <- state_surv_fleetsep |> mutate(year_pair = as.character(year_pair))

year_pair_levels <- national_surv |> distinct(year_pair) |> pull(year_pair) |> sort()
n_pairs <- length(year_pair_levels)
year_pair_palette <- scales::viridis_pal(option = "turbo")(n_pairs) |> setNames(year_pair_levels)

# --- Combine vehicle classes by summing CY1/CY2, then recompute survival ---

combine_classes <- function(df) {
  group_cols <- setdiff(names(df), c("VehicleClass", "CY1", "CY2", "survival"))
  df |>
    filter(!is.na(VehicleClass)) |>
    group_by(across(all_of(group_cols))) |>
    summarize(CY1 = sum(CY1, na.rm = TRUE), CY2 = sum(CY2, na.rm = TRUE), .groups = "drop") |>
    mutate(survival = CY2 / CY1)
}

state_combined <- combine_classes(state_surv)
state_combined_fleetsep <- combine_classes(state_surv_fleetsep)
national_combined <- combine_classes(national_surv)
national_combined_fleetsep <- combine_classes(national_surv_fleetsep)

# --- National aggregates (excl 20-21 and 22-23) ---

compute_national_aggregate_combined <- function(df) {
  df |>
    filter(year_pair != "20202021", year_pair != "20222023") |>
    group_by(regtype, Age1) |>
    summarize(CY2 = sum(CY2, na.rm = TRUE), CY1 = sum(CY1, na.rm = TRUE), .groups = "drop") |>
    mutate(survival = CY2 / CY1) |>
    filter(Age1 >= 1) |>
    group_by(regtype) |>
    arrange(regtype, Age1) |>
    mutate(cumulative_survival = cumprod(survival))
}

natl_agg_combined <- compute_national_aggregate_combined(national_combined)
natl_agg_combined_fleetsep <- compute_national_aggregate_combined(national_combined_fleetsep)

# --- Cumulative survival for state combined ---

state_cum_combined <- state_combined |>
  filter(Age1 >= 1) |>
  group_by(year_pair, regtype, state_name, county_name, Tech) |>
  arrange(Age1, .by_group = TRUE) |>
  mutate(cumulative_survival = cumprod(survival)) |>
  ungroup()

state_cum_combined_fleetsep <- state_combined_fleetsep |>
  filter(Age1 >= 1) |>
  group_by(year_pair, regtype, state_name, county_name, Tech) |>
  arrange(Age1, .by_group = TRUE) |>
  mutate(cumulative_survival = cumprod(survival)) |>
  ungroup()

# ========== 1. ANNUAL CONDITIONAL SURVIVAL GEOFACET ==========
message("=== Annual conditional survival geofacet (combined) ===")

p_ann_geo <- state_combined |>
  filter(Age1 >= 1) |>
  mutate(year_pair = factor(year_pair, levels = year_pair_levels)) |>
  ggplot(aes(x = Age1, y = survival, color = year_pair, group = year_pair)) +
  geom_line(alpha = 0.6, linewidth = 0.7) +
  geom_line(
    data = natl_agg_combined,
    aes(x = Age1, y = survival, group = 1),
    col = "black", linewidth = 1, linetype = "dashed", inherit.aes = FALSE
  ) +
  scale_color_manual(values = year_pair_palette, name = "Year pair") +
  scale_x_continuous(limits = c(NA, 30)) +
  facet_geo(facets = "state_name", grid = us_state_grid3, label = "name") +
  scale_y_continuous(limits = c(0.7, 1.2)) +
  guides(color = guide_legend(nrow = 1)) +
  labs(
    y = "Annual conditional net survival rate",
    x = "Vehicle age",
    title = "Annual conditional net survival rates for all LDVs combined by state (colored) and national long-term average (excl 20-21 and 22-23) (black dashes)"
    # caption = "Source: Derived from Experian registration data, by Arthur Yip, NLR"
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "ann_surv_combined_bystate_14to25_allyears.png"),
       plot = p_ann_geo, width = 14, height = 10)
message("  Saved annual geofacet")

# ========== 2. CUMULATIVE SURVIVAL GEOFACET ==========
message("=== Cumulative survival geofacet (combined) ===")

p_cum_geo <- state_cum_combined |>
  mutate(year_pair = factor(year_pair, levels = year_pair_levels)) |>
  ggplot(aes(x = Age1, y = cumulative_survival, color = year_pair, group = year_pair)) +
  geom_line(alpha = 0.6, linewidth = 0.7) +
  geom_line(
    data = natl_agg_combined,
    aes(x = Age1, y = cumulative_survival, group = 1),
    col = "black", linewidth = 1, linetype = "dashed", inherit.aes = FALSE
  ) +
  scale_color_manual(values = year_pair_palette, name = "Year pair") +
  scale_x_continuous(limits = c(NA, 30)) +
  facet_geo(facets = "state_name", grid = us_state_grid3, label = "name") +
  scale_y_continuous(limits = c(0, 1.5)) +
  guides(color = guide_legend(nrow = 1)) +
  labs(
    y = "Cumulative net survival rate",
    x = "Vehicle age",
    title = "Cumulative net survival rates for all LDVs combined by state (colored) and national long-term average (excl 20-21 and 22-23) (black dashes)"
    # caption = "Source: Derived from Experian registration data, by Arthur Yip, NLR"
  ) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "cum_surv_combined_bystate_14to25_allyears.png"),
       plot = p_cum_geo, width = 14, height = 10)
message("  Saved cumulative geofacet")

# ========== 3. STATE-BY-STATE CUMULATIVE ==========
message("=== State-by-state cumulative (combined) ===")

state_ann_combined <- state_combined |> filter(Age1 >= 1)
state_ann_combined_fleetsep <- state_combined_fleetsep |> filter(Age1 >= 1)

cum_dir <- file.path(out_dir, "cum cond surv state by state")
dir.create(cum_dir, showWarnings = FALSE, recursive = TRUE)

plot_state_cum_combined <- function(st) {
  safe_name <- st |> str_replace_all("\\s+", "_") |> str_replace_all("[^A-Za-z0-9_]", "")
  out_file <- file.path(cum_dir, paste0("cumcond_combinedclasses_", safe_name, "_14to25.png"))

  p <- (state_cum_combined |>
    filter(state_name == st) |>
    ggplot() +
    geom_line(aes(x = Age1, y = cumulative_survival, group = year_pair, color = year_pair)) +
    geom_line(
      data = natl_agg_combined,
      aes(x = Age1, y = cumulative_survival, group = 1),
      linetype = "dashed", linewidth = 1.5, color = "black"
    ) +
    scale_color_manual(values = year_pair_palette, name = "Year pair") +
    facet_grid(rows = vars(regtype)) +
    scale_y_continuous(limits = c(0, 1.5)) +
    scale_x_continuous(limits = c(0, 30)) +
    guides(color = guide_legend(nrow = 1)) +
    labs(
      y = "Cumulative net survival rate",
      title = paste0("Empirical ", st, " LDV (combined) cumulative net survival for 2014-2025"),
      subtitle = "National long-term average in dashes (excl 20-21 and 22-23)",
      x = "Age in year 1"
    ) +
    theme(legend.position = "bottom")) /
  (state_cum_combined_fleetsep |>
    filter(state_name == st) |>
    ggplot() +
    geom_line(aes(x = Age1, y = cumulative_survival, group = interaction(year_pair, regtype), color = year_pair)) +
    geom_line(
      data = natl_agg_combined_fleetsep,
      aes(x = Age1, y = cumulative_survival, group = regtype),
      linetype = "dashed", linewidth = 1.5, color = "black"
    ) +
    scale_color_manual(values = year_pair_palette, name = "Year pair") +
    facet_grid(rows = vars(regtype)) +
    scale_y_continuous(limits = c(0, 1.5)) +
    scale_x_continuous(limits = c(0, 30)) +
    guides(color = guide_legend(nrow = 1)) +
    labs(
      y = "Cumulative net survival rate",
      # caption = "Source: Derived from Experian registration data, by Arthur Yip, NLR",
      x = "Age in year 1"
    ) +
    theme(legend.position = "bottom"))

  ggsave(out_file, plot = p, width = 14, height = 10)
  out_file
}

states_to_plot <- state_cum_combined |> distinct(state_name) |> pull(state_name) |> na.omit()
out_files_cum <- map_chr(states_to_plot, plot_state_cum_combined)
message("  Saved ", length(out_files_cum), " state cumulative plots")

# ========== 4. STATE-BY-STATE ANNUAL CONDITIONAL ==========
message("=== State-by-state annual conditional (combined) ===")

ann_dir <- file.path(out_dir, "ann cond surv state by state")
dir.create(ann_dir, showWarnings = FALSE, recursive = TRUE)

plot_state_ann_combined <- function(st) {
  safe_name <- st |> str_replace_all("\\s+", "_") |> str_replace_all("[^A-Za-z0-9_]", "")
  out_file <- file.path(ann_dir, paste0("anncond_combinedclasses_", safe_name, "_14to25.png"))

  p <- (state_ann_combined |>
    filter(state_name == st) |>
    ggplot() +
    geom_line(aes(x = Age1, y = survival, group = year_pair, color = year_pair)) +
    geom_line(
      data = natl_agg_combined,
      aes(x = Age1, y = survival, group = 1),
      linetype = "dashed", linewidth = 1.5, color = "black"
    ) +
    scale_color_manual(values = year_pair_palette, name = "Year pair") +
    facet_grid(rows = vars(regtype)) +
    scale_y_continuous(limits = c(0.7, 1.2)) +
    scale_x_continuous(limits = c(0, 30)) +
    guides(color = guide_legend(nrow = 1)) +
    labs(
      y = "Annual conditional net survival rate",
      title = paste0("Empirical ", st, " LDV (combined) annual conditional net survival for 2014-2025"),
      subtitle = "National long-term average in dashes (excl 20-21 and 22-23)",
      x = "Age in year 1"
    ) +
    theme(legend.position = "bottom")) /
  (state_ann_combined_fleetsep |>
    filter(state_name == st) |>
    ggplot() +
    geom_line(aes(x = Age1, y = survival, group = interaction(year_pair, regtype), color = year_pair)) +
    geom_line(
      data = natl_agg_combined_fleetsep,
      aes(x = Age1, y = survival, group = regtype),
      linetype = "dashed", linewidth = 1.5, color = "black"
    ) +
    scale_color_manual(values = year_pair_palette, name = "Year pair") +
    facet_grid(rows = vars(regtype)) +
    scale_y_continuous(limits = c(0.7, 1.2)) +
    scale_x_continuous(limits = c(0, 30)) +
    guides(color = guide_legend(nrow = 1)) +
    labs(
      y = "Annual conditional net survival rate",
      # caption = "Source: Derived from Experian registration data, by Arthur Yip, NLR",
      x = "Age in year 1"
    ) +
    theme(legend.position = "bottom"))

  ggsave(out_file, plot = p, width = 14, height = 10)
  out_file
}

states_to_plot_ann <- state_ann_combined |> distinct(state_name) |> pull(state_name) |> na.omit()
out_files_ann <- map_chr(states_to_plot_ann, plot_state_ann_combined)
message("  Saved ", length(out_files_ann), " state annual plots")

message("=== All done! ===")

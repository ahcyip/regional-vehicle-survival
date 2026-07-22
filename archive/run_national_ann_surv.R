library(tidyverse)

surv_data_dir <- "c:/users/ayip/OneDrive - NREL (1)/survival and uvm/data"
out_dir <- "c:/users/ayip/OneDrive - NREL (1)/survival and uvm"

national_surv <- read_csv(file.path(surv_data_dir, "pipeline_national_survival_14to25.csv"), show_col_types = FALSE) |>
  mutate(year_pair = as.character(year_pair))

year_pair_levels <- national_surv |> distinct(year_pair) |> pull(year_pair) |> sort()
outlier_pairs <- c("20202021", "20222023")
normal_pairs <- setdiff(year_pair_levels, outlier_pairs)

n_normal <- length(normal_pairs)
normal_colors <- scales::viridis_pal(option = "turbo")(n_normal) |> setNames(normal_pairs)
outlier_colors <- c("20202021" = "black", "20222023" = "grey40")
all_colors <- c(normal_colors, outlier_colors)[year_pair_levels]

normal_linetypes <- rep("solid", n_normal) |> setNames(normal_pairs)
outlier_linetypes <- c("20202021" = "dashed", "20222023" = "dotted")
all_linetypes <- c(normal_linetypes, outlier_linetypes)[year_pair_levels]

plot_national_survival <- function(df, title_label, filename) {
  plot_df <- df |>
    filter(Age1 >= 1) |>
    mutate(year_pair = factor(year_pair, levels = year_pair_levels))

  p <- ggplot(plot_df, aes(x = Age1, y = survival, color = year_pair, linetype = year_pair, group = year_pair)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = all_colors, name = "Year pair") +
    scale_linetype_manual(values = all_linetypes, name = "Year pair") +
    scale_x_continuous(breaks = seq(0, 50, 5), limits = c(0, 50)) +
    scale_y_continuous(breaks = seq(0.70, 1.20, 0.05), limits = c(0.70, 1.20)) +
    labs(
      title = paste0("National ", title_label, " Net Survival Rates: 2015-2025"),
      x = "Vehicle Age",
      y = "Year-to-Year Net Survival Rate"
      # caption = "Source: Derived from Experian registration data, by Arthur Yip, NLR"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 13),
      legend.position = "right"
    )

  ggsave(file.path(out_dir, filename), plot = p, width = 10, height = 7)
  message("  Saved: ", filename)
}

message("=== National annual survival by year pair ===")

for (vc in c("Car", "SUV", "Pickup")) {
  vc_label <- case_when(vc == "Car" ~ "Passenger Car", vc == "SUV" ~ "SUV", vc == "Pickup" ~ "Pickup Truck")
  df <- national_surv |> filter(VehicleClass == vc)
  plot_national_survival(df, vc_label, paste0("national_ann_surv_", str_to_lower(vc), "_byyearpair.png"))
}

national_combined <- national_surv |>
  filter(!is.na(VehicleClass)) |>
  group_by(year_pair, Age1) |>
  summarize(CY1 = sum(CY1, na.rm = TRUE), CY2 = sum(CY2, na.rm = TRUE), .groups = "drop") |>
  mutate(survival = CY2 / CY1)

plot_national_survival(national_combined, "Light-duty Vehicle (Combined)", "national_ann_surv_combined_byyearpair.png")

message("=== Done! ===")

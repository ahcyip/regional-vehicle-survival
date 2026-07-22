Sys.setenv(PATH = paste("C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools", Sys.getenv("PATH"), sep = ";"))
library(tidyverse)
library(plotly)
library(htmlwidgets)

surv_data_dir <- "c:/users/ayip/OneDrive - NREL (1)/survival and uvm/data"
out_dir <- "c:/users/ayip/OneDrive - NREL (1)/survival and uvm"

national_surv <- read_csv(file.path(surv_data_dir, "pipeline_national_survival_14to25.csv"), show_col_types = FALSE)
state_surv <- read_csv(file.path(surv_data_dir, "pipeline_state_survival_14to25.csv"), show_col_types = FALSE)

state_plot_df <- state_surv |>
  filter(Age1 >= 1, !is.na(VehicleClass)) |>
  mutate(label = paste0(state_name, " | ", year_pair, " | ", VehicleClass,
                        "\nAge: ", Age1, " | Net Survival: ", round(survival, 3)))

national_plot_df <- national_surv |>
  filter(Age1 >= 1, !is.na(VehicleClass)) |>
  mutate(label = paste0("USA | ", year_pair, " | ", VehicleClass,
                        "\nAge: ", Age1, " | Net Survival: ", round(survival, 3)))

p <- ggplot() +
  geom_point(
    data = state_plot_df,
    aes(x = Age1, y = survival, shape = VehicleClass, text = label),
    color = "grey70", size = 1, alpha = 0.5
  ) +
  geom_point(
    data = national_plot_df,
    aes(x = Age1, y = survival, shape = VehicleClass, text = label),
    color = "black", size = 1.5
  ) +
  scale_shape_manual(values = c("Car" = 16, "SUV" = 17, "Pickup" = 15)) +
  scale_x_continuous(breaks = seq(0, 55, 10), limits = c(0, 55)) +
  scale_y_continuous(breaks = seq(0, 2, 0.2), limits = c(0, 2)) +
  labs(
    title = "National and State Conditional Annual Net Survival Rates: 2014-2025, Light-duty Vehicles",
    x = "Age",
    y = "Net Survival Rate",
    shape = "Vehicle Class"
    # caption = "Source: Derived from Experian registration data, by Arthur Yip, NLR"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)
  )

message("Saving static PNG...")
ggsave(file.path(out_dir, "national_state_ann_surv_byclass.png"),
       plot = p, width = 10, height = 7)

message("Saving interactive HTML...")
interactive_p <- ggplotly(p, tooltip = "text")
saveWidget(interactive_p,
           file = file.path(out_dir, "national_state_ann_surv_byclass.html"),
           selfcontained = TRUE)

message("By-class plots done. Now generating combined (all classes) version...")

state_combined_df <- state_surv |>
  filter(Age1 >= 1, !is.na(VehicleClass)) |>
  group_by(year_pair, state_name, Age1) |>
  summarize(CY1 = sum(CY1, na.rm = TRUE), CY2 = sum(CY2, na.rm = TRUE), .groups = "drop") |>
  mutate(survival = CY2 / CY1,
         label = paste0(state_name, " | ", year_pair,
                        "\nAge: ", Age1, " | Net Survival: ", round(survival, 3)))

national_combined_df <- national_surv |>
  filter(Age1 >= 1, !is.na(VehicleClass)) |>
  group_by(year_pair, Age1) |>
  summarize(CY1 = sum(CY1, na.rm = TRUE), CY2 = sum(CY2, na.rm = TRUE), .groups = "drop") |>
  mutate(survival = CY2 / CY1,
         label = paste0("USA | ", year_pair,
                        "\nAge: ", Age1, " | Net Survival: ", round(survival, 3)))

p2 <- ggplot() +
  geom_point(
    data = state_combined_df,
    aes(x = Age1, y = survival, text = label),
    color = "grey70", size = 1, alpha = 0.5
  ) +
  geom_point(
    data = national_combined_df,
    aes(x = Age1, y = survival, text = label),
    color = "black", size = 1.5
  ) +
  scale_x_continuous(breaks = seq(0, 55, 10), limits = c(0, 55)) +
  scale_y_continuous(breaks = seq(0, 2, 0.2), limits = c(0, 2)) +
  labs(
    title = "National and State Conditional Annual Net Survival Rates: 2014-2025, All Light-duty Vehicles Combined",
    x = "Age",
    y = "Net Survival Rate"
    # caption = "Source: Derived from Experian registration data, by Arthur Yip, NLR"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12)
  )

message("Saving combined static PNG...")
ggsave(file.path(out_dir, "national_state_ann_surv_combined.png"),
       plot = p2, width = 10, height = 7)

message("Saving combined interactive HTML...")
interactive_p2 <- ggplotly(p2, tooltip = "text")
saveWidget(interactive_p2,
           file = file.path(out_dir, "national_state_ann_surv_combined.html"),
           selfcontained = TRUE)

message("Done!")

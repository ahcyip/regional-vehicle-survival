#!/usr/bin/env Rscript
# Harmonization report for two Parquet snapshots of the Experian zip-level
# VIO + demographics data, plus first-cut national make/model/MY survival.
#
# RUN ON A KESTREL COMPUTE NODE (see submit_viod.sh).
#   Rscript viod_report.R 2024Q4 2025Q4
# Writes small CSVs (national aggregates only) to $VIOD_REPORTS/<a>_<b>/.
#
# The second snapshot is one year after the first for 2024Q4 -> 2025Q4, so a
# model-year cohort's national count ratio is its annual survival. Vehicles
# rarely leave the country, so that ratio reflects scrappage, plus any
# naming/classification drift between Experian vintages, which this report
# also flags.

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(readr)
  library(stringr); library(purrr); library(rlang)
})

args  <- commandArgs(trailingOnly = TRUE)
snaps <- args[1:2]
root  <- Sys.getenv("VIOD_PARQUET", "/scratch/ayip/viod_parquet")
out   <- file.path(Sys.getenv("VIOD_REPORTS", "~/viod_reports"), paste(snaps, collapse = "_"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
interim_dir <- "/projects/tempo/experian/experian"

ds <- open_dataset(root) |> filter(snapshot %in% !!snaps)
snap_year <- as.integer(str_sub(snaps, 1, 4))
log <- \(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")

## 1. totals ------------------------------------------------------------
totals <- ds |>
  group_by(snapshot) |>
  summarise(rows = n(), vehicles = sum(vehicle_count),
            zips = n_distinct(zip5), states = n_distinct(state_name)) |>
  collect() |> arrange(snapshot)
write_csv(totals, file.path(out, "totals.csv"))
log("totals done"); print(totals)

## 2. category levels, field by field ------------------------------------
# vehicle counts per level and snapshot; levels present in only one snapshot,
# or whose share moves a lot, point to coding changes rather than real change
fields <- c("category", "segmentation", "fuel_type_group", "fuel_type",
            "body_style", "drivewheel", "gvw_class", "import_domestic_sales",
            "fleet_type", "ind_org_type", "age_generation", "age_group",
            "number_of_children", "presence_of_children", "dwelling_type",
            "education_level", "estimated_home_value", "ethnicity", "gender",
            "household_income", "marital_status",
            "mosaic_lifestyle_segmentation", "occupation")

levels_long <- map(fields, \(f) {
  ds |>
    group_by(snapshot, level = !!sym(f)) |>
    summarise(vehicles = sum(vehicle_count)) |>
    collect() |>
    mutate(field = f)
}) |>
  list_rbind() |>
  mutate(share = vehicles / sum(vehicles), .by = c(field, snapshot))

levels_wide <- levels_long |>
  select(field, level, snapshot, share) |>
  pivot_wider(names_from = snapshot, values_from = share, names_prefix = "share_") |>
  mutate(in_both = if_all(starts_with("share_"), \(x) !is.na(x)),
         share_change = .data[[paste0("share_", snaps[2])]] - .data[[paste0("share_", snaps[1])]]) |>
  arrange(field, desc(abs(share_change)))
write_csv(levels_long, file.path(out, "category_levels_long.csv"))
write_csv(levels_wide, file.path(out, "category_levels_compare.csv"))
log("category levels done")

## 3. national counts by make / model / model year -----------------------
mm <- ds |>
  group_by(snapshot, category, make, model, model_year) |>
  summarise(vehicles = sum(vehicle_count)) |>
  collect()
write_csv(mm, file.path(out, "national_make_model_my_counts.csv"))

mm_wide <- mm |>
  pivot_wider(names_from = snapshot, values_from = vehicles,
              names_prefix = "n_", values_fill = 0) |>
  rename(n1 = !!paste0("n_", snaps[1]), n2 = !!paste0("n_", snaps[2])) |>
  mutate(age1 = snap_year[1] - model_year,
         survival = if_else(n1 > 0, n2 / n1, NA_real_))
write_csv(mm_wide, file.path(out, "national_make_model_my_survival.csv"))

# cohorts that grow, or appear or vanish, at age >= 2 are naming or
# classification drift, not survival
drift <- mm_wide |>
  filter(age1 >= 2, pmax(n1, n2) >= 1000, (survival > 1.02 | n1 == 0 | n2 == 0)) |>
  arrange(desc(abs(n2 - n1)))
write_csv(drift, file.path(out, "make_model_drift_candidates.csv"))
log("make/model done; drift candidates:", nrow(drift))

## 4. survival summaries --------------------------------------------------
by_age <- mm_wide |>
  filter(age1 >= 0, age1 <= 30) |>
  summarise(n1 = sum(n1), n2 = sum(n2), .by = c(category, age1)) |>
  mutate(survival = n2 / n1) |>
  arrange(category, age1)
write_csv(by_age, file.path(out, "national_survival_by_category_age.csv"))

by_make_age <- mm_wide |>
  filter(age1 >= 1, age1 <= 25) |>
  summarise(n1 = sum(n1), n2 = sum(n2), .by = c(category, make, age1)) |>
  mutate(survival = n2 / n1) |>
  arrange(category, make, age1)
write_csv(by_make_age, file.path(out, "national_survival_by_make_age.csv"))
log("survival summaries done")

## 5. comparison with the interim files used by survival.qmd ------------
# the interim exp_YYYY_stock_interim.csv files feed the existing pipeline;
# matching their totals by model year shows whether these snapshots count
# the same fleet
interim <- map(snap_year, \(y) {
  read_csv(file.path(interim_dir, sprintf("exp_%d_stock_interim.csv", y)),
           show_col_types = FALSE,
           col_select = c(vehicle_model_year, stock)) |>
    summarise(interim = sum(stock), .by = vehicle_model_year) |>
    mutate(year = y)
}) |> list_rbind()

my_tot <- mm |>
  summarise(viod = sum(vehicles), .by = c(snapshot, model_year)) |>
  mutate(year = as.integer(str_sub(snapshot, 1, 4))) |>
  full_join(interim, by = join_by(year, model_year == vehicle_model_year)) |>
  mutate(ratio = viod / interim) |>
  arrange(year, model_year)
write_csv(my_tot, file.path(out, "totals_by_my_vs_interim.csv"))
log("interim comparison done")

## 6. state totals --------------------------------------------------------
state_tot <- ds |>
  group_by(snapshot, state_name) |>
  summarise(vehicles = sum(vehicle_count)) |>
  collect() |>
  pivot_wider(names_from = snapshot, values_from = vehicles)
write_csv(state_tot, file.path(out, "state_totals.csv"))
log("report complete:", out)

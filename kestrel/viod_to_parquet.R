#!/usr/bin/env Rscript
# Convert one Experian zip-level VIO + demographics snapshot (~110 GB CSV) to
# harmonized Parquet, partitioned by snapshot and state.
#
# RUN ON A KESTREL COMPUTE NODE (see submit_viod.sh), never the login node.
#   Rscript viod_to_parquet.R 2024Q4 [path/to/sample.csv]
# The optional second argument overrides the source file (for testing on a
# small head -n sample).
#
# Harmonization:
# - Names: all snapshots get the 2024Q4 file's snake_case names. The other
#   snapshots use the same 46 columns, in the same order, in CamelCase; the
#   header is checked before renaming. 2024Q4's extra fuel-economy columns
#   are dropped.
# - Text: whitespace is squished ("45-54     " -> "45-54",
#   "$1,000  - $14,999" -> "$1,000 - $14,999") and empty strings become NA.
#   Dollar ranges are normalized to "a - b" (2026 files write "a-b").
# - Types: zip5 is left-padded to 5 digits, model_year is integer, and
#   vehicle_count is int64. Every other field stays a string, so nothing is
#   lost.
# Category *meanings* (e.g. "Unmatched" vs NA) are not recoded here.
# viod_report.R compares them across snapshots first.

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(stringr); library(purrr); library(rlang)
})

args     <- commandArgs(trailingOnly = TRUE)
snapshot <- args[1]
out_root <- Sys.getenv("VIOD_PARQUET", "/scratch/ayip/viod_parquet")

sources <- c(
  "2024Q2" = "/scratch/ayip/NREL_EVD+Demograhpic_US_ZipCode_2024Q2.csv",
  "2024Q4" = "/scratch/ayip/NREL_EVD+Demograhpic_US_ZipCode_2024Q4_with_fuel_economy_data.csv",
  "2025Q4" = "/scratch/ayip/NLR_EVD+Demograhpic_US_ZipCode_2025Q4.csv",
  "2026Q1" = "/scratch/ayip/NLR_EVD+Demograhpic_US_ZipCode_2026Q1.csv",
  "2026Q2" = "/scratch/ayip/NRL_EVD_Demographic_US_ZipCode_2026Q2.csv"
)
stopifnot(snapshot %in% names(sources))
src <- if (length(args) >= 2) args[2] else sources[[snapshot]]

harmonized <- c(
  "state_name", "county_name", "city", "zip_code", "category", "segmentation",
  "manufacturer", "make", "model", "model_year", "series", "body_style",
  "door_count", "drivewheel", "fuel_type", "fuel_type_group", "liters",
  "cylinders", "performance_option", "fuel_delivery", "wheelbase", "gvw_class",
  "weight", "transmission_type", "min_hp", "max_hp", "min_kw", "max_kw",
  "import_domestic_hq", "import_domestic_sales", "fleet_type", "ind_org_type",
  "age_generation", "age_group", "number_of_children", "presence_of_children",
  "dwelling_type", "education_level", "estimated_home_value", "ethnicity",
  "gender", "household_income", "marital_status",
  "mosaic_lifestyle_segmentation", "occupation", "vehicle_count")

camel <- c(
  "StateName", "CountyName", "ZipCityName", "ZIPCode", "VehicleCategory",
  "VehicleSegmentation", "VehicleManufacturer", "VehicleMake", "VehicleModel",
  "VehicleModelYear", "VehicleSeries", "VehicleBodyStyle", "VehicleDoorCount",
  "VehicleDrivewheel", "VehicleFuelType", "VehicleFuelTypeGroup",
  "VehicleEngineLiters", "VehicleEngineCylinders", "VehiclePerformanceOption",
  "VehicleFuelDelivery", "VehicleWheelbase", "VehicleGVWClass", "VehicleWeight",
  "VehicleTransmissionType", "VehicleMinHP", "VehicleMaxHP", "VehicleMinkW",
  "VehicleMaxkW", "VehicleImportDomesticHQ", "VehicleImportDomesticSales",
  "FleetFlag", "IndOrgFlag", "AgeGeneration", "AgeGroup", "NumberofChildren",
  "PresenceofChildren", "DwellingType", "EducationLevel", "EstimatedHomeValue",
  "Ethnicity", "Gender", "HouseholdIncome", "MaritalStatus",
  "MosaicLifestyleSegmentation", "Occupation", "VehicleCount")

# detect delimiter and naming from the header line
header_line <- readLines(src, n = 1)
delim <- if (str_count(header_line, fixed("|")) > str_count(header_line, fixed(","))) "|" else ","
header <- str_split_1(header_line, fixed(delim))

if (identical(header[seq_along(camel)], camel)) {
  rename_map <- set_names(camel, harmonized)
} else if (identical(header[seq_along(harmonized)], harmonized)) {
  rename_map <- set_names(harmonized, harmonized)
} else {
  stop("unexpected header in ", src, ":\n", header_line)
}

# read everything as text; types are set after cleaning
sch <- schema(map(set_names(header), \(x) utf8()))
ds  <- open_dataset(src, format = "csv", delimiter = delim, schema = sch, skip = 1)

text_cols  <- setdiff(harmonized, c("model_year", "vehicle_count"))
money_cols <- c("household_income", "estimated_home_value")

squish <- map(set_names(text_cols),
              \(cl) expr(str_trim(str_replace_all(!!sym(cl), "\\s+", " "))))
blank_na <- map(set_names(text_cols),
                \(cl) expr(if_else(!!sym(cl) == "", NA_character_, !!sym(cl))))
money <- map(set_names(money_cols),
             \(cl) expr(str_replace_all(!!sym(cl), " ?- ?", " - ")))

t0 <- Sys.time()
ds |>
  select(all_of(rename_map)) |>
  mutate(!!!squish) |>
  mutate(!!!blank_na) |>
  mutate(!!!money,
         zip5          = str_pad(zip_code, 5, side = "left", pad = "0"),
         # a few rows carry an empty model year, which a direct cast rejects
         model_year    = cast(if_else(str_trim(model_year) == "", NA_character_,
                                      str_trim(model_year)), int32()),
         vehicle_count = cast(str_trim(vehicle_count), int64()),
         snapshot      = !!snapshot) |>
  select(-zip_code) |>
  write_dataset(out_root, format = "parquet",
                partitioning = c("snapshot", "state_name"),
                existing_data_behavior = "delete_matching",
                compression = "zstd")
cat(snapshot, "written in", format(round(Sys.time() - t0, 1)), "\n")

# check: every CSV data row arrived, and no counts were lost to NA casts
pq <- open_dataset(out_root) |> filter(snapshot == !!snapshot)
chk <- pq |>
  summarise(rows = n(), vehicles = sum(vehicle_count, na.rm = TRUE),
            na_count = sum(is.na(vehicle_count)), na_my = sum(is.na(model_year))) |>
  collect()
csv_rows <- as.numeric(system2("bash", c("-c", shQuote(paste("tail -n +2", shQuote(src), "| wc -l"))),
                               stdout = TRUE))
cat(sprintf("%s: parquet rows %s vs csv data rows %s; vehicles %s; NA counts %s; NA model_year rows %s\n",
            snapshot, format(chk$rows, big.mark = ","), format(csv_rows, big.mark = ","),
            format(chk$vehicles, big.mark = ","), chk$na_count, chk$na_my))
stopifnot(chk$rows == csv_rows, chk$na_count == 0)

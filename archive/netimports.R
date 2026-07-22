# =====================================================================
# SUPERSEDED: this analysis is now integrated into survival.qmd
# (section "Phase 1b: Net imports of used vehicles"), extended to old +
# new methods for all yearpairs, NA handling for flagged cells, and
# multiple averaging windows. Kept for reference only.
# ---------------------------------------------------------------------
# Net imports of used vehicles, by state and vehicle type
# ---------------------------------------------------------------------
# R / tidyverse replication of the NetImports tab of
#   StateNationalSurvivalRateDifferences12Dec25NewMethod.xlsx,
# generalized to use ALL yearpairs available in the data files.
#
# Method (mirrors the spreadsheet tab chain):
#   raw tabs -> trim (NatSurv Trim / ByVType) -> state-minus-national
#   differences (SND) -> per-state net-import rates (NetImports).
#
# Two sets of numbers, matching the two halves of the NetImports tab:
#   1. Simple   (sheet cols A:N)   - unweighted mean of the survival
#      difference over ages 1-7, one value per state x yearpair.
#   2. Weighted (sheet cols AF:BB) - weighted mean of the survival
#      difference over ages 2-8. Weighting scheme per yearpair:
#        - "cohort" (the workbook's New Method) wherever the yearpair
#          has the 6 prior consecutive yearpairs needed: the weight
#          for age a follows the cohort backwards through the STATE's
#          own survival, w(a) = s(yp,a)*s(yp-1,a-1)*...*s(yp-(a-2),2).
#        - "national" (old method) for the early yearpairs without
#          enough history: w(a) = cumulative product of NATIONAL
#          survival, ages 2..a, within the same yearpair.
#      (The workbook applied "cohort" only to its latest yearpair,
#       2021/22; here it is applied to every yearpair that can
#       support it. The long output flags the method used.)
#
# Known, deliberate differences from the workbook (Excel formula
# quirks found while reverse-engineering; this code produces the
# clean version of the intended calculation):
#   * Idaho, 2014/15, all 3 vehicle types: the SND formulas for Idaho
#     subtract NATIONAL survival of the WRONG yearpair (2015/16).
#   * Pickup / Alabama / 2015/16: one SND cell (O2399, the age-1
#     difference) is a hard-coded literal 1, not a formula.
#   * Pickup / District of Columbia / 2016/17: the ByVType block is
#     one row short (DC has 4 junk rows, not 5), so the age-7 entry
#     is blank. (DC pickup data is extremely noisy here either way:
#     age-7 survival = 2.38 -> trimmed to the 999 flag.)
# One quirk that IS replicated because it looks intentional:
#   * Tennessee: its outlier first-yearpair (2014/15) weighted value
#     is excluded from Average / Std.Dev (the sheet uses
#     AVERAGE(AQ:AW), not AP:AW, in all three vehicle blocks).
# =====================================================================

library(tidyverse)

# ---- 0. Parameters --------------------------------------------------

data_dir     <- "data"            # relative to survival.qmd
n_ages       <- 7                 # ages 1-7 (simple) / 2-8 (weighted)
cohort_depth <- n_ages - 1        # prior yearpairs needed for cohort wts

# Excel helpers
trim999     <- function(x) if_else(is.na(x) | abs(x) >= 2, 999, x)
sd_pop      <- function(x) sqrt(mean((x - mean(x))^2))     # STDEV.P
var_s_excel <- function(x) var(c(x, mean(x)))  # VAR.S(D:L): the sheet
                                               # includes the mean itself

# ---- 1. Read the two data files -------------------------------------
# state file: yearpair, statename, Age, VehicleClass, Survival
# national file: yearpair, Age1, VehicleClass, survival
# (columns renamed by position; the national header has trailing
#  spaces in "yearpair  ", and "NA" strings are parsed as NA)

state_surv <- read_csv(file.path(data_dir, "state_14to25_survival.csv"),
                       show_col_types = FALSE) |>
  select(1:5) |>
  rename(yearpair = 1, statename = 2, Age = 3,
         VehicleClass = 4, Survival = 5)

national_surv <- read_csv(file.path(data_dir, "national_14to25_survival.csv"),
                          show_col_types = FALSE) |>
  select(1:4) |>
  rename(yearpair = 1, Age = 2, VehicleClass = 3, Survival = 4) |>
  mutate(Age = suppressWarnings(as.numeric(Age)))

# All yearpairs present in BOTH files, in order
yearpairs <- sort(intersect(unique(state_surv$yearpair),
                            unique(national_surv$yearpair)))
n_yp      <- length(yearpairs)

# Cohort weighting needs consecutive yearpairs (20142015, 20152016, ...)
start_yrs <- as.integer(str_sub(as.character(yearpairs), 1, 4))
stopifnot(all(diff(start_yrs) == 1))

# Yearpairs with enough history for cohort weights vs not
cohort_yps   <- yearpairs[seq_along(yearpairs) > cohort_depth]
national_yps <- setdiff(yearpairs, cohort_yps)

# ---- 2. Trim and difference  (NatSurv Trim / ByVType / SND tabs) ----
# Keep ages >= 1; flag missing (-99) or implausible (|s| >= 2)
# survival as 999, exactly as the IF(...,999) formulas do.

state_t <- state_surv |>
  filter(yearpair %in% yearpairs, Age >= 1) |>
  mutate(surv_state = trim999(as.numeric(Survival)))

nat_t <- national_surv |>
  filter(yearpair %in% yearpairs, !is.na(Age), Age >= 1) |>
  mutate(surv_nat = trim999(as.numeric(Survival)))

# SND: state minus national, matched on yearpair x vehicle type x age
snd <- state_t |>
  select(yearpair, statename, VehicleClass, Age, surv_state) |>
  left_join(nat_t |> select(yearpair, VehicleClass, Age, surv_nat),
            by = c("yearpair", "VehicleClass", "Age")) |>
  mutate(surv_diff = surv_state - surv_nat)

# ---- 3. Simple net-import rates  (NetImports cols A:N) --------------
# Mean survival difference over ages 1-7 (sum / 7, as in
# SUM(SND!E..)/$C$2 with C2 = 7).

simple_ni <- snd |>
  filter(Age <= n_ages) |>
  group_by(VehicleClass, statename, yearpair) |>
  summarise(NI = sum(surv_diff) / n_ages, n = n(), .groups = "drop")

stopifnot(all(simple_ni$n == n_ages))     # every block has all 7 ages
simple_ni <- simple_ni |> select(-n)      # tidy/long version

net_imports_simple <- simple_ni |>
  arrange(VehicleClass, statename, yearpair) |>
  mutate(year = str_sub(as.character(yearpair), 1, 4)) |>
  select(-yearpair) |>
  pivot_wider(names_from = year, values_from = NI, names_prefix = "NI_") |>
  rowwise() |>
  mutate(
    Mean    = mean(c_across(starts_with("NI_"))),
    # Car block, col M ("Within") = VAR.S over the yearpair values AND
    # their mean; col N ("Ratio") = Within/Mean when Mean > 0.099
    Within  = var_s_excel(c_across(starts_with("NI_"))),
    Ratio   = if_else(Mean > 0.099, Within / Mean, NA_real_),
    # Pickup / SUV blocks instead report col M = Mean * 7
    Mean_x7 = Mean * n_ages
  ) |>
  ungroup()

# ---- 4. Weighted net-import rates  (NetImports cols AF:BB) ----------

# 4a. national (old-method) weights, for early yearpairs only:
#     w(a) = cumprod of national survival, ages 2..a, within the same
#     yearpair x vehicle type (SND cols G/H: H_a = I3*I4*...*Ia)
w_national <- nat_t |>
  filter(Age >= 2, Age <= n_ages + 1) |>
  arrange(VehicleClass, yearpair, Age) |>
  group_by(VehicleClass, yearpair) |>
  mutate(w = cumprod(surv_nat)) |>
  ungroup() |>
  select(yearpair, VehicleClass, Age, w)

weighted_national <- snd |>
  filter(yearpair %in% national_yps, Age >= 2, Age <= n_ages + 1) |>
  inner_join(w_national, by = c("yearpair", "VehicleClass", "Age")) |>
  group_by(VehicleClass, statename, yearpair) |>
  summarise(WNI = sum(surv_diff * w) / sum(w), .groups = "drop") |>
  mutate(method = "national")

# 4b. cohort (new-method) weights, for every yearpair with full
#     history: w(a) = product of the STATE's own survival following
#     the cohort backwards: s(yp,a)*s(yp-1,a-1)*...*s(yp-(a-2),2)
cohort_steps <- crossing(end_yp  = cohort_yps,
                         age_end = 2:(n_ages + 1),
                         back    = 0:cohort_depth) |>   # deepest: age 8 -> back 6
  filter(back <= age_end - 2) |>
  mutate(yearpair = yearpairs[match(end_yp, yearpairs) - back],
         Age      = age_end - back)

w_cohort <- cohort_steps |>
  inner_join(state_t |>
               select(yearpair, statename, VehicleClass, Age, surv_state),
             by = join_by(yearpair, Age),
             relationship = "many-to-many") |>
  group_by(VehicleClass, statename, end_yp, age_end) |>
  summarise(w = prod(surv_state), n = n(), .groups = "drop")

stopifnot(all(w_cohort$n == w_cohort$age_end - 1))  # full cohort history

weighted_cohort <- snd |>
  filter(yearpair %in% cohort_yps, Age >= 2, Age <= n_ages + 1) |>
  inner_join(w_cohort |> select(-n),
             by = join_by(VehicleClass, statename,
                          yearpair == end_yp, Age == age_end)) |>
  group_by(VehicleClass, statename, yearpair) |>
  summarise(WNI = sum(surv_diff * w) / sum(w), .groups = "drop") |>
  mutate(method = "cohort")

# tidy/long version, with the weighting method flagged per yearpair
weighted_ni <- bind_rows(weighted_national, weighted_cohort) |>
  arrange(VehicleClass, statename, yearpair)

# 4c. per-state summary: Average, population SD, Average +/- 2 SD.
#     Tennessee's outlier first-yearpair value is excluded, as in
#     the spreadsheet.
weighted_summary <- weighted_ni |>
  filter(!(statename == "Tennessee" & yearpair == min(yearpairs))) |>
  group_by(VehicleClass, statename) |>
  summarise(Average = mean(WNI),
            Std_Dev = sd_pop(WNI),
            Lo      = Average - 2 * Std_Dev,    # col BA ("0.05")
            Hi      = Average + 2 * Std_Dev,    # col BB ("0.95")
            .groups = "drop")

net_imports_weighted <- weighted_ni |>
  mutate(year = str_sub(as.character(yearpair), 1, 4)) |>
  select(-yearpair, -method) |>
  pivot_wider(names_from = year, values_from = WNI,
              names_prefix = "WNI_") |>
  left_join(weighted_summary, by = c("VehicleClass", "statename"))

# ---- 5. Column sums (the SUM rows under each block) ------------------

sum_rows_simple <- simple_ni |>
  group_by(VehicleClass, yearpair) |>
  summarise(SUM = sum(NI), .groups = "drop")

sum_rows_weighted <- weighted_ni |>
  group_by(VehicleClass, yearpair, method) |>
  summarise(SUM = sum(WNI), .groups = "drop")

# ---- 6. Outputs -------------------------------------------------------
# long/tidy:  simple_ni (VehicleClass, statename, yearpair, NI)
#             weighted_ni (..., yearpair, WNI, method)
# wide/sheet-style: net_imports_simple, net_imports_weighted
#                   (one NI_/WNI_ column per available yearpair)

net_imports_simple
net_imports_weighted

# ---- 7. greene_intermediate ------------------------------------------
# Replaces the survival.qmd block that did
#   greene <- readxl::read_excel(...12Dec25NewMethod.xlsx, "NetImports")
#   greene_intermediate <- greene |> select(VehType...1, State...2,
#     Mean, `2019...9` .. `2015...5`) |> ...
# The Excel columns map to the simple (ages 1-7, unweighted) block:
#   Mean       -> mean of the simple NI over yearpairs 2014/15-2021/22
#   2015..2019 -> per-yearpair simple NI values
#
# Improvement over the sheet: NI cells contaminated by a 999 trim flag
# (missing / |survival| >= 2 at ages 1-7; currently only DC Pickup
# 2016/17) become NA instead of nonsense. NetImpExpMean1421 averages
# the remaining years (na.rm = TRUE); NetImpExpMean1519 keeps the
# qmd's plain 5-year formula, so it propagates NA for that one row.
#
# Requires vehstockdata_forweightinggreene (defined in survival.qmd
# just above the old block) for the stock-weighted columns.

flagged <- snd |>
  filter(Age <= n_ages) |>
  group_by(VehicleClass, statename, yearpair) |>
  summarise(n_flagged = sum(surv_state == 999 | surv_nat == 999),
            .groups = "drop")

simple_clean <- simple_ni |>
  left_join(flagged, by = c("VehicleClass", "statename", "yearpair")) |>
  mutate(NI = if_else(n_flagged > 0, NA_real_, NI),
         year = as.integer(str_sub(as.character(yearpair), 1, 4)))

greene_intermediate <- simple_clean |>
  filter(year %in% 2014:2021) |>
  group_by(VehType = VehicleClass, STNAME = statename) |>
  summarise(NetImpExpMean1421 = mean(NI, na.rm = TRUE), .groups = "drop") |>
  left_join(
    simple_clean |>
      filter(year %in% 2015:2019) |>
      mutate(yr2 = paste0("NetImpExp", str_sub(as.character(year), 3, 4))) |>
      select(VehType = VehicleClass, STNAME = statename, yr2, NI) |>
      pivot_wider(names_from = yr2, values_from = NI),
    by = c("VehType", "STNAME")
  )

if (exists("vehstockdata_forweightinggreene")) {
  # stock join and multipliers copied verbatim from survival.qmd
  greene_intermediate <- greene_intermediate |>
    left_join(vehstockdata_forweightinggreene,
              by = c("VehType", "STNAME")) |>
    mutate(NetImpExpMean1519 = (NetImpExp19 + NetImpExp18 + NetImpExp17 +
                                NetImpExp16 + NetImpExp15) / 5,
           NetImpExpMean1519_stock = (NetImpExpMean1519 * stock * 0.92) |> round(-2),
           NetImpExpMean1421_stock = (NetImpExpMean1421 * stock * 0.93) |> round(-2),
           NetImpExp19_stock = NetImpExp19 * stock * 0.95,
           NetImpExp18_stock = NetImpExp18 * stock * 0.94,
           NetImpExp17_stock = NetImpExp17 * stock * 0.93,
           NetImpExp16_stock = NetImpExp16 * stock * 0.92,
           NetImpExp15_stock = NetImpExp15 * stock * 0.91)
} else {
  message("vehstockdata_forweightinggreene not found - ",
          "greene_intermediate has rate columns only (no *_stock).")
}

# in survival.qmd, keep the existing save line:
# greene_intermediate |> write_csv(file.path(surv_data_dir, "greene_intermediate.csv"))

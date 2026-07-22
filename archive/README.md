# Archive — superseded, kept for reference

These files are **not** part of the active pipeline. The current, canonical pipeline
and all plotting live in [`../survival.qmd`](../survival.qmd). Nothing here should be
run as the source of truth; it's retained for history and reference only.

| File | Why archived |
|------|--------------|
| `netimports.R` | Self-declared superseded: the net-imports analysis is now integrated into `survival.qmd` (section "Phase 1b: Net imports of used vehicles"), extended to old + new methods, all yearpairs, NA handling, and multiple averaging windows. |
| `run_phase1_and_ann_geofacet.R` | "Phase 1" (Experian → survival rates) is now done inside `survival.qmd`; this standalone runner duplicates it. |
| `run_ann_geofacet.R` | Geofacet annual-survival plotting — duplicated by chunks in `survival.qmd`. |
| `run_combined_class_plots.R` | Combined-class geofacet plotting — duplicated by `survival.qmd`. |
| `run_national_ann_surv.R` | National annual-survival plotting — duplicated by `survival.qmd`. |
| `run_scatter_plot.R` | National-vs-state scatter (plotly) — duplicated by `survival.qmd`. |
| `survival_v1_old.qmd` | Older, smaller version of `survival.qmd` (was `survival (2).qmd`). |
| `survival-analysis.qmd` | Earlier, shorter analysis approach, superseded by `survival.qmd`. |

Dropped entirely (not archived): `survival-analysis (2).qmd` (byte-identical duplicate of
`survival-analysis.qmd`) and `survival-comparison.R` (a 27-byte pointer note).

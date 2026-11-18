#!/bin/bash
# Submit Parquet conversion of Experian zip-level VIO + demographics snapshots
# (one array task per snapshot), then the harmonization report once both are
# done. Run from the Kestrel login node:
#   bash ~/regional-vehicle-survival/kestrel/submit_viod.sh 2024Q4 2025Q4
set -euo pipefail

a=$1; b=$2
here=$(cd "$(dirname "$0")" && pwd)
logs=~/viod_logs; mkdir -p "$logs"
R="R_LIBS_USER=$HOME/R/tempo-r-new-extra $HOME/.conda-envs/tempo-r-new/bin/Rscript"

conv=$(sbatch --parsable --account=tempo --partition=short --time=04:00:00 \
  --nodes=1 --cpus-per-task=52 --mem=240G --array=0-1 \
  --job-name=viod_pq --output="$logs/viod_pq_%A_%a.log" \
  --wrap="snaps=($a $b); $R $here/viod_to_parquet.R \${snaps[\$SLURM_ARRAY_TASK_ID]}")

rep=$(sbatch --parsable --account=tempo --partition=short --time=02:00:00 \
  --nodes=1 --cpus-per-task=52 --mem=240G --dependency=afterok:$conv \
  --job-name=viod_rep --output="$logs/viod_report_${a}_${b}_%j.log" \
  --wrap="$R $here/viod_report.R $a $b")

echo "conversion array: $conv   report: $rep   logs: $logs"

# Regional Vehicle Survival

Data processing and analysis for regional vehicle survival work at NREL.

This project analyzes vehicle survival (time-to-event / age-at-retirement) patterns across U.S. regions, combining survival analysis methods with vehicle usage and mobility modeling (UVM).

---

## Project Structure

```
regional-vehicle-survival/
├── data/
│   ├── raw/          # Immutable raw input data — do not edit these files
│   └── processed/    # Cleaned and transformed data ready for analysis
├── notebooks/        # Jupyter or R notebooks for exploration and analysis
├── src/              # Reusable Python/R source modules and helper functions
├── results/
│   ├── figures/      # Generated plots and visualizations (output, not tracked)
│   └── tables/       # Generated summary tables and statistics (output, not tracked)
├── environment.yml   # Conda environment specification
└── README.md
```

See [`data/README.md`](data/README.md) for a description of data sources and file formats.

---

## Setup

### Prerequisites

- [Anaconda](https://www.anaconda.com/download) or [Miniconda](https://docs.conda.io/en/latest/miniconda.html)

### Create and activate the environment

```bash
conda env create -f environment.yml
conda activate regional-vehicle-survival
```

### Run analysis notebooks

Open notebooks from the `notebooks/` directory in Jupyter:

```bash
jupyter lab
```

---

## Workflow

1. **Raw data** lives in `data/raw/` — never modified directly.
2. **Data processing** scripts/notebooks in `notebooks/` or `src/` clean and transform raw data into `data/processed/`.
3. **Analysis** notebooks in `notebooks/` consume processed data and write outputs to `results/`.
4. **Figures and tables** in `results/` are generated outputs and are excluded from version control by default (see `.gitignore`).

---

## Data

Raw data files are stored in `data/raw/` and are listed in [`data/README.md`](data/README.md). Large data files may be excluded from the repository via `.gitignore`; see that file for details and instructions on obtaining the data.

---

## Contributing

- Keep raw data immutable — never overwrite files in `data/raw/`.
- Write notebooks with clear, sequential cell execution (restart kernel and run all before committing).
- Store reusable logic in `src/` rather than duplicating code across notebooks.
- Commit processed data and results only when they are small and reproducible output is impractical to regenerate.

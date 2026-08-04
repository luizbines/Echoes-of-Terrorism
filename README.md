# Echoes of Terrorism — README

**Purpose**: This README maps the project structure, briefly documents every script group and the main outputs, and explains how to reproduce results. It is written to support economic research best-practices: reproducibility, clear execution order, and quick understanding for collaborators and reviewers.

**Quick Start**
- From the repository root run the master runner in one of three modes:
  - `simple` (default): runs the main processing pipelines (raw → cleaning → treating) without performing any web scraping
  - `extraction`: it is necessary to use a VPN connected to Israel. Runs Extraction first, which scrapes the necessary raw data, then raw → cleaning → treating
  - `dry-run` (alias `dry`): lists the scripts that would run (safe preview)

Usage examples:

```bash
Rscript master.R simple
Rscript master.R extraction
Rscript master.R dry-run
```

**Dynamic Paths**
- All scripts use **dynamic paths** — no hardcoded directory paths. The `master.R` script automatically detects the project root and passes it to all child scripts via the `R_PROJECT_DIR` environment variable.
- You can run the master script from any working directory:

```bash
# From project root:
cd /path/to/Echoes-of-Terrorism && Rscript master.R simple

# Or from anywhere:
Rscript /path/to/Echoes-of-Terrorism/master.R simple
```

- Individual scripts also work standalone and auto-detect the project structure.

**High-level execution order**
- `master.R` (root) — runs `Voting/main_Voting.R` then `Trends/main_Trends.R` in the same mode
- Each module main script executes these categories (in order): `raw`, `cleaning`, `treating`.
- For `extraction` and `dry-run` the pipeline includes an `Extraction` stage before `raw`.
- Within each category the sub-folders are processed in the order: `Red Alerts`, `Israel`, `Elections` (if present).
- Inside each sub-folder the scripts are executed alphabetically; after those, any `Robustness/` scripts are executed (alphabetically).

**Repository map (short)**

- `master.R` — Master runner (root). Calls:
  - `Voting/main_Voting.R` — Voting pipeline runner (supports modes)
  - `Trends/main_Trends.R` — Trends pipeline runner (supports modes)

- `Voting/` — Voting data & analysis
  - `main_Voting.R` — orchestrates Voting scripts in modes (simple/extraction/dry-run)
  - `raw/` — raw data and supporting files (shapefiles, config, etc.)
    - `Red Alerts/Extraction/` (optional) — extraction scripts (e.g. `1_importing_red_alerts.py`)
  - `cleaning/`
    - `Red Alerts/Code/1_filtering_red_alerts.R`
    - `Israel/Code/1_israel_demographics.R`, `2_israel_night_light.R`, `3_detecting_west_bank.R`, `4_adding_SEI.R`
    - `Elections/Code/1_getting_all_parties_percentages.R`, `2_merging_parties_datasets.R`
  - `treating/`
    - `Red Alerts/Code/` — regressions, plotting and `Robustness/` variants
  - `Output/` folders inside submodules hold produced CSVs, tables, and figures

- `Trends/` — Google Trends processing & analysis
  - `main_Trends.R` — orchestrates Trends scripts in modes
  - `raw/Google Trends/Extraction/gtrends_v2.py` — extraction script (requires Python and `pytrends`)
  - `cleaning/Red Alerts/Code/adding_districts_v2.R` — data preparation
  - `treating/Israel/trend_regressions.R` — regressions and tables
  - `Output/` and `cleaning/output/` contain CSVs and derived datasets

**Scripts & languages**
- R scripts: executed with `Rscript` via `source()` when run inside a runner
- Python scripts: executed with `python3` (runner will call `python3 <script>`) — ensure required Python packages are installed

**Outputs**
- Outputs are saved inside module `Output/` directories (or submodule `Output/` path), for example:
  - `Voting/cleaning/Elections/Output/parties_percentages.csv`
  - `Voting/treating/Red Alerts/Output/` — regression outputs, maps, figures
  - `Trends/cleaning/output/` — cleaned trends datasets
  - `Trends/raw/Google Trends/Output/trends_israel.csv`

**Reproducibility & Environment**
- R: use R >= 4.0 and install required packages listed at the top of scripts (commonly `dplyr`, `data.table`, `sf`, `lfe`, `sandwich`, `stargazer` or `broom` depending on scripts). Consider using `renv` or `packrat` to snapshot package state.
- Python: `python3` with `pytrends` (for gtrends), and usual data libs if scripts use them. Example:

```bash
python3 -m pip install pytrends pandas numpy
```

- Run everything from the repository root to preserve relative paths.
- Use `dry-run` to review the exact execution order before running.

**Logging & warnings**
- The master `master.R` suppresses child-process stderr by default to avoid flooding the terminal with child warnings. Individual scripts may still print key messages to stdout. If you want full logs, edit `master.R` to redirect `stderr` to a file instead of `/dev/null`.

**Notes for reviewers**
- The repository is organized to make replication straightforward: master runner → two module runners → category/subfolder → scripts. Output locations are local to module subfolders, simplifying artifact inspection.
- The extraction stage requires a VPN connected to Israel, network access and Python dependencies; run this stage only when you can obtain remote data.
- The file `/Voting/raw/Red Alerts/area_codes.csv` is a dictionary that translates the old identification system used by the Home Front Command into Israeli localities. This was collected in July 2024 from their website before removal. An identical table can still be found at `https://www.mivzaklive.co.il/%D7%94%D7%AA%D7%A8%D7%90%D7%AA-%D7%A6%D7%91%D7%A2-%D7%90%D7%93%D7%95%D7%9D-%D7%9E%D7%A1%D7%A4%D7%A8%D7%99-%D7%A4%D7%95%D7%9C%D7%99%D7%92%D7%95%D7%A0%D7%99%D7%9D-%D7%95%D7%96%D7%9E%D7%A0%D7%99-%D7%94`.
- The file `/Voting/raw/Israel/all_cities_coordinates.csv` contains all relevant Israeli locality coordinates (manually collected)

**Contact / Maintenance**
- Maintainer: Luiz Bines (github repository: `https://github.com/luizbines/Echoes-of-Terrorism`).
---
_Last updated: 2026_

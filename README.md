# Swiss hotel demand: where should a tourism agency spend?

**Authors:** <Name 1>, <Name 2>, <Name 3>
**Course:** Applied Machine Learning and Predictive Modelling 1 (HSLU, HS26)

## Context
A tourism agency wants to know where to spend money and effort to attract visitors to Switzerland:
which origin markets matter in each canton, how sensitive they are to exchange rates, and where there
is room for growth. We use hotel statistics from the Swiss Federal Statistical Office (BFS) combined
with exchange rates, income, COVID restrictions, distance, population and visa rules of the origin markets.

## Dataset
`data/processed/swiss_hotel_panel.csv`: **37,440 rows** (26 cantons × 120 months × 12 markets) and 19 columns.
- One row per month, canton and origin market, January 2016 to December 2025 (the last 10 years).
  2015 is downloaded only to compute the "vs. last year" columns (`growth`, `bed_occupancy_lag12`).
- **Near markets:** Germany, France, Italy, United Kingdom, Netherlands, Austria.
- **Far markets:** United States, China, India, Japan, South Korea, Gulf states (GCC = Bahrain, Qatar, Kuwait, Oman, Saudi Arabia, UAE).
- `data/processed/data_dictionary.csv` gives each column's definition and source, and its role (identifier, response or predictor).

## Data sources
| Data | Source |
|---|---|
| Arrivals and overnight stays by year, month, canton, country of residence | BFS, HESTA, table px-x-1003020000_102 |
| Beds and bed occupancy by year, month, canton | BFS, HESTA, table px-x-1003020000_202 |
| Monthly CHF exchange rates (EUR, GBP, USD, CNY, JPY, KRW) | Swiss National Bank data portal, cube `devkum` |
| Indian rupees per US dollar | FRED, series `EXINUS` |
| Inflation, GDP per capita (PPP), real GDP growth, population | IMF World Economic Outlook (DataMapper API) |
| COVID-19 stringency index | Oxford COVID-19 Government Response Tracker (OxCGRT) |
| Distance between Switzerland and the origin country | CEPII GeoDist (Mayer & Zignago 2011), R package `cepiigeodist` |
| Schengen visa requirement | EU Regulation 2018/1806 (coded by hand in `data/raw/manual/origin_lookup.csv`) |

## Folder structure
```
data/raw/                      downloaded source files (not edited by hand)
  bfs_demand.csv, bfs_supply.csv        from R/01_get_bfs.R
  fx_monthly.csv, fred_EXINUS.csv,      from R/02_get_fx_macro.R
  macro_yearly.csv, covid_monthly.csv,
  origin_static.csv
data/raw/manual/origin_lookup.csv  visa rules per origin (coded by hand)
data/processed/                master panel + data dictionary
R/01_get_bfs.R                 BFS demand and capacity tables (PxWeb API)
R/02_get_fx_macro.R            exchange rates, IMF indicators, COVID stringency, distance
R/03_build_panel.R             joins everything and derives the columns
```

## Reproduce
From this folder (needs internet access):
```
Rscript R/01_get_bfs.R
Rscript R/02_get_fx_macro.R      # also downloads the OxCGRT file (44 MB) if it is missing
Rscript R/03_build_panel.R
```
R packages: tidyverse (dplyr, tidyr, readr), httr, jsonlite, cepiigeodist.

## Notes
- **Missing values:**
  - 97 cells are hidden by the BFS (small values in small cantons). The columns derived from them are NA too.
  - `avg_stay` is NA when arrivals are 0.
- **Period:** the whole period is after the SNB dropped the EUR/CHF floor (Jan 2015). COVID (2020-2022) covers 3 of the 10 years.
- **Nesting:** `origin_group`, `distance_km` and `visa_required` are fixed per origin (`population_origin_m` almost), so they duplicate `origin_country`. A model uses either the country or these features, not both.

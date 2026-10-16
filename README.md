# Swiss hotel demand: where should a tourism agency spend?

**Authors:** <Name 1>, <Name 2>, <Name 3>
**Course:** Applied Machine Learning and Predictive Modelling 1 (HSLU, HS26)

## Context
A tourism agency wants to know where to spend money and effort to attract visitors to Switzerland:
which origin markets matter in each canton, how sensitive they are to exchange rates, and where there
is room for growth. We use hotel statistics from the Swiss Federal Statistical Office (BFS) combined
with exchange rates, income, population and visa rules of the origin markets.

## Dataset
`data/processed/swiss_hotel_panel.csv`: **3,120 rows** (26 cantons × 10 years × 12 markets) and 17 columns.
- One row per year, canton and origin market, 2016 to 2025 (the last 10 years).
- All values are yearly at the source (BFS "Total of the year", SNB and FRED annual averages, IMF yearly figures).
  2015 is downloaded only to compute `growth` (vs. last year).
- **Near markets:** Germany, France, Italy, United Kingdom, Netherlands, Austria.
- **Far markets:** United States, China, India, Japan, South Korea, Gulf states (GCC = Bahrain, Qatar, Kuwait, Oman, Saudi Arabia, UAE).
- `data/processed/data_dictionary.csv` gives each column's definition and source, and its role (identifier, response, predictor or descriptive only).

## Data sources
| Data | Source |
|---|---|
| Arrivals and overnight stays by year, canton, country of residence | BFS, HESTA, table px-x-1003020000_102 |
| Beds and bed occupancy by year, canton | BFS, HESTA, table px-x-1003020000_202 |
| Annual average CHF exchange rates (EUR, GBP, USD, CNY, JPY, KRW) | Swiss National Bank data portal, cube `devkua` |
| Indian rupees per US dollar, annual | FRED, series `AEXINUS` |
| Inflation, GDP per capita (PPP), real GDP growth, population | IMF World Economic Outlook (DataMapper API) |
| Schengen visa requirement | EU Regulation 2018/1806 (coded by hand in `data/raw/manual/origin_lookup.csv`) |

## Folder structure
```
data/raw/                          downloaded source files (not edited by hand)
  bfs_demand.csv, bfs_supply.csv     from R/01_get_bfs.R
  fx_yearly.csv, fred_AEXINUS.csv,   from R/02_get_fx_macro.R
  macro_yearly.csv, origin_static.csv
data/raw/manual/origin_lookup.csv  visa rules per origin (coded by hand)
data/processed/                    master panel + data dictionary
R/01_get_bfs.R                     BFS demand and capacity tables (PxWeb API)
R/02_get_fx_macro.R                exchange rates, IMF indicators, population
R/03_build_panel.R                 joins everything and derives the columns
```

## Reproduce
From this folder (needs internet access):
```
Rscript R/01_get_bfs.R
Rscript R/02_get_fx_macro.R
Rscript R/03_build_panel.R
```
R packages: tidyverse (dplyr, tidyr, readr), httr, jsonlite.

## Notes
- **Missing values:**
  - 2 yearly cells are hidden by the BFS (Appenzell Innerrhoden: GCC and South Korea, 2022). The columns derived from them are NA too.
  - `avg_stay` is NA when arrivals are 0.
- **Period:** the whole period is after the SNB dropped the EUR/CHF floor (Jan 2015). `covid_period` is pre (2016-2019), during (2020-2021) and post (2022-2025).
- **Leakage:** `share_in_canton` and `bed_occupancy_pct` are computed from demand. Use them only for description (which markets matter, where there is spare capacity), not as predictors of arrivals or stays.
- **Nesting:** `origin_group` and `visa_required` are fixed per origin (`population_origin_m` almost), so they duplicate `origin_country`. A model uses either the country or these features, not both.

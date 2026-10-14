# 03_build_panel.R
# Builds the master panel: one row per month x canton x origin market, Jan 2016 - Dec 2025.
# 2015 is only used to compute the "vs. last year" columns.
# Run after 01_get_bfs.R and 02_get_fx_macro.R.
# Output: data/processed/swiss_hotel_panel.csv

library(dplyr)
library(tidyr)
library(readr)

near <- c("Germany", "France", "Italy", "United Kingdom", "Netherlands", "Austria")
iso  <- c(Germany = "DEU", France = "FRA", Italy = "ITA", "United Kingdom" = "GBR", Netherlands = "NLD",
          Austria = "AUT", "United States" = "USA", China = "CHN", India = "IND", Japan = "JPN",
          "South Korea" = "KOR", GCC = "GCC")
currency <- c(Germany = "EUR1", France = "EUR1", Italy = "EUR1", Netherlands = "EUR1", Austria = "EUR1",
              "United Kingdom" = "GBP1", "United States" = "USD1", China = "CNY100", India = "INR",
              Japan = "JPY100", "South Korea" = "KRW100", GCC = "USD1")
gulf_codes <- c("60", "68", "69", "81", "88", "89")   # BH, QA, KW, OM, SA, AE

# ---- Demand (BFS _102) ----
raw <- read_csv("data/raw/bfs_demand.csv", show_col_types = FALSE,
                col_types = cols(.default = "c", value = "d")) %>%
  transmute(year = as.integer(Jahr), month = as.integer(Monat), canton_code = Kanton,
            canton = Kanton_label, origin_code = Herkunftsland, origin = Herkunftsland_label,
            indicator = ifelse(Indikator == "1", "arrivals", "overnights"), value)

# GCC: sum of the six Gulf countries
gcc <- raw %>%
  filter(origin_code %in% gulf_codes) %>%
  group_by(year, month, canton_code, canton, indicator) %>%
  summarise(value = if (all(is.na(value))) NA_real_ else sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(origin = "GCC")

demand <- raw %>%
  filter(!origin_code %in% gulf_codes) %>%
  mutate(origin = ifelse(origin_code == "00", "TOTAL", origin)) %>%
  select(-origin_code) %>%
  bind_rows(gcc) %>%
  pivot_wider(names_from = indicator, values_from = value)

panel <- demand %>%
  filter(canton_code != "8100", origin != "TOTAL") %>%
  rename(origin_country = origin) %>%
  group_by(canton, origin_country, month) %>% arrange(year, .by_group = TRUE) %>%
  mutate(growth = as.integer(arrivals > lag(arrivals))) %>%   # vs. same month last year
  ungroup() %>%
  mutate(avg_stay = ifelse(arrivals > 0, overnights / arrivals, NA_real_))

# ---- Capacity (BFS _202) ----
supply <- read_csv("data/raw/bfs_supply.csv", show_col_types = FALSE,
                   col_types = cols(.default = "c", value = "d")) %>%
  filter(Kanton != "8100") %>%
  transmute(year = as.integer(Jahr), month = as.integer(Monat), canton = Kanton_label,
            indicator = ifelse(Indikator == "3", "beds", "bed_occupancy_pct"), value) %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  group_by(canton, month) %>% arrange(year, .by_group = TRUE) %>%
  mutate(bed_occupancy_lag12 = lag(bed_occupancy_pct)) %>%   # same month last year
  ungroup()

# ---- Real exchange rate ----
macro <- read_csv("data/raw/macro_yearly.csv", show_col_types = FALSE)
cpi_ch <- macro %>% filter(iso3 == "CHE") %>% select(year, cpi_ch = cpi, gdp_pc_ppp_ch = gdp_pc_ppp)

fx <- read_csv("data/raw/fx_monthly.csv", show_col_types = FALSE) %>%
  mutate(year = as.integer(format(date, "%Y")), month = as.integer(format(date, "%m")))
real_fx <- tibble(origin_country = names(iso), iso3 = unname(iso), currency = currency[names(iso)]) %>%
  inner_join(fx, by = "currency", relationship = "many-to-many") %>%
  left_join(macro %>% select(iso3, year, cpi), by = c("iso3", "year")) %>%
  left_join(cpi_ch, by = "year") %>%
  # CHF per unit of origin currency, adjusted for relative prices.
  # Higher = origin currency stronger in real terms = Switzerland cheaper for that market.
  mutate(real_fx = chf_per_unit * cpi / cpi_ch) %>%
  group_by(origin_country) %>%
  mutate(real_fx_index = 100 * real_fx / mean(real_fx[year == 2016])) %>%
  ungroup() %>%
  select(origin_country, year, month, real_fx_index)

# ---- Income, growth, COVID ----
income <- macro %>%
  left_join(cpi_ch %>% select(year, gdp_pc_ppp_ch), by = "year") %>%
  transmute(iso3, year, rel_income = gdp_pc_ppp / gdp_pc_ppp_ch, gdp_growth_origin = gdp_growth)
covid <- read_csv("data/raw/covid_monthly.csv", show_col_types = FALSE)

# ---- Origin features ----
origin_static <- read_csv("data/raw/origin_static.csv", show_col_types = FALSE)
origin_lookup <- read_csv("data/raw/manual/origin_lookup.csv", show_col_types = FALSE) %>%
  select(origin_country, visa_required)

# ---- Assemble ----
panel <- panel %>%
  mutate(iso3 = iso[origin_country]) %>%
  left_join(supply, by = c("year", "month", "canton")) %>%
  left_join(real_fx, by = c("origin_country", "year", "month")) %>%
  left_join(income, by = c("iso3", "year")) %>%
  left_join(covid, by = c("iso3", "year", "month")) %>%
  left_join(origin_static, by = c("iso3", "year")) %>%
  left_join(origin_lookup, by = "origin_country") %>%
  mutate(
    date = as.Date(sprintf("%d-%02d-01", year, month)),
    origin_group = ifelse(origin_country %in% near, "near", "far"),
    covid_stringency_origin = coalesce(covid_stringency, 0)
  ) %>%
  select(date, year, month, canton, origin_country, origin_group,
         arrivals, overnights, avg_stay, growth,
         beds, bed_occupancy_lag12,
         real_fx_index, rel_income, gdp_growth_origin,
         covid_stringency_origin,
         distance_km, population_origin_m, visa_required) %>%
  filter(year >= 2016) %>%   # lags above were computed with 2015 included
  arrange(origin_country, canton, date)

dir.create("data/processed", showWarnings = FALSE)
write_csv(panel, "data/processed/swiss_hotel_panel.csv")

# ---- Checks ----
stopifnot(!anyDuplicated(panel[c("date", "canton", "origin_country")]))
stopifnot(nrow(panel) == 26 * 120 * 12)
de_zh <- panel %>% filter(origin_country == "Germany", canton == "Zurich", year == 2019) %>% pull(arrivals) %>% sum()
stopifnot(de_zh == 379775)   # yearly value in nice_ish_format.csv
message("rows: ", nrow(panel), " | columns: ", ncol(panel))
na <- colSums(is.na(panel)); message("NAs: ", paste(names(na[na > 0]), na[na > 0], collapse = ", "))

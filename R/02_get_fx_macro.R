# 02_get_fx_macro.R
# Exchange rates, prices, income and COVID restrictions for the 12 origin markets.
#   - SNB data portal, cube "devkum": monthly average CHF exchange rates (EUR, GBP, USD, CNY, JPY, KRW)
#   - FRED "EXINUS": Indian rupees per USD (the SNB does not publish INR)
#   - IMF World Economic Outlook (DataMapper API): inflation (PCPIPCH), GDP per capita PPP (PPPPC),
#     real GDP growth (NGDP_RPCH), population (LP, used to weight the Gulf countries)
#   - OxCGRT: COVID-19 stringency index (download once, see below)
#   - CEPII GeoDist: distance from Switzerland to each origin country
# GCC currencies are pegged to the US dollar, so the GCC market uses USD.
# Output: data/raw/fx_monthly.csv, data/raw/macro_yearly.csv, data/raw/covid_monthly.csv,
#         data/raw/origin_static.csv

library(dplyr)
library(tidyr)
library(readr)
library(jsonlite)
library(cepiigeodist)

markets <- tribble(
  ~origin_country,  ~currency, ~iso3,
  "Germany",        "EUR1",    "DEU",
  "France",         "EUR1",    "FRA",
  "Italy",          "EUR1",    "ITA",
  "Netherlands",    "EUR1",    "NLD",
  "Austria",        "EUR1",    "AUT",
  "United Kingdom", "GBP1",    "GBR",
  "United States",  "USD1",    "USA",
  "China",          "CNY100",  "CHN",
  "India",          "INR",     "IND",
  "Japan",          "JPY100",  "JPN",
  "South Korea",    "KRW100",  "KOR",
  "GCC",            "USD1",    "GCC"
)
gcc_iso <- c("BHR", "KWT", "OMN", "QAT", "SAU", "ARE")

# ---- Nominal exchange rates: CHF per 1 unit of the origin currency, monthly average ----
snb <- read_delim(
  "https://data.snb.ch/api/cube/devkum/data/csv/en?fromDate=2015-01&toDate=2025-12&dimSel=D0(M0),D1(EUR1,GBP1,USD1,CNY100,JPY100,KRW100)",
  delim = ";", skip = 3, show_col_types = FALSE) %>%
  transmute(date = as.Date(paste0(Date, "-01")), currency = D1,
            chf_per_unit = Value / ifelse(grepl("100$", D1), 100, 1))

fred_file <- "data/raw/fred_EXINUS.csv"
download.file("https://fred.stlouisfed.org/graph/fredgraph.csv?id=EXINUS", fred_file, method = "curl", quiet = TRUE)
fred_inr <- read_csv(fred_file, show_col_types = FALSE) %>%
  transmute(date = observation_date, inr_per_usd = EXINUS)
inr <- snb %>% filter(currency == "USD1") %>%
  inner_join(fred_inr, by = "date") %>%
  transmute(date, currency = "INR", chf_per_unit = chf_per_unit / inr_per_usd)

fx <- bind_rows(snb, inr) %>% filter(date >= as.Date("2015-01-01"), date <= as.Date("2025-12-01"))
write_csv(fx, "data/raw/fx_monthly.csv")

# ---- IMF WEO yearly indicators ----
imf <- function(indicator) {
  vals <- fromJSON(paste0("https://www.imf.org/external/datamapper/api/v1/", indicator))$values[[indicator]]
  bind_rows(lapply(names(vals), function(iso) {
    v <- vals[[iso]]
    tibble(iso3 = iso, year = as.integer(names(v)), value = as.numeric(unlist(v)))
  })) %>% filter(year %in% 2015:2025) %>% rename(!!indicator := value)
}
weo <- imf("PCPIPCH") %>%
  full_join(imf("PPPPC"), by = c("iso3", "year")) %>%
  full_join(imf("NGDP_RPCH"), by = c("iso3", "year")) %>%
  full_join(imf("LP"), by = c("iso3", "year")) %>%
  filter(iso3 %in% c(markets$iso3, gcc_iso, "CHE"))

weo_countries <- weo   # keep the individual Gulf countries for the distance weights below

# GCC = population-weighted average of the six Gulf countries
gcc <- weo %>% filter(iso3 %in% gcc_iso) %>%
  group_by(year) %>%
  summarise(across(c(PCPIPCH, PPPPC, NGDP_RPCH), ~ weighted.mean(.x, LP, na.rm = TRUE)),
            LP = sum(LP), .groups = "drop") %>%
  mutate(iso3 = "GCC")
weo <- bind_rows(weo %>% filter(!iso3 %in% gcc_iso), gcc)

# Price index from annual inflation, 2016 = 100
macro <- weo %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(cpi = cumprod(1 + PCPIPCH / 100), cpi = 100 * cpi / cpi[year == 2016]) %>%
  ungroup() %>%
  transmute(iso3, year, cpi, inflation = PCPIPCH, gdp_pc_ppp = PPPPC, gdp_growth = NGDP_RPCH)
write_csv(macro, "data/raw/macro_yearly.csv")

# ---- Origin features: distance (CEPII) and population (IMF, millions) ----
pop2020 <- weo_countries %>% filter(year == 2020) %>% select(iso3, LP)
distance <- dist_cepii %>%
  filter(iso_o == "CHE", iso_d %in% c(markets$iso3, gcc_iso)) %>%
  transmute(iso3 = iso_d, distance_km = dist) %>%
  left_join(pop2020, by = "iso3") %>%
  mutate(iso3 = ifelse(iso3 %in% gcc_iso, "GCC", iso3)) %>%
  group_by(iso3) %>%
  summarise(distance_km = weighted.mean(distance_km, LP), .groups = "drop")   # GCC: population-weighted
origin_static <- weo %>%
  filter(iso3 %in% markets$iso3) %>%
  select(iso3, year, population_origin_m = LP) %>%   # GCC: sum of the six (see above)
  left_join(distance, by = "iso3")
write_csv(origin_static, "data/raw/origin_static.csv")

# ---- OxCGRT: monthly mean stringency (0-100), 2020-2022 ----
# Source file (44 MB, downloaded once):
# https://raw.githubusercontent.com/OxCGRT/covid-policy-dataset/main/data/OxCGRT_compact_national_v1.csv
ox_file <- "data/raw/OxCGRT_compact_national_v1.csv"
if (!file.exists(ox_file))
  download.file("https://raw.githubusercontent.com/OxCGRT/covid-policy-dataset/main/data/OxCGRT_compact_national_v1.csv", ox_file)
pop <- weo %>% filter(year == 2020) %>% select(iso3, LP)
covid <- read_csv(ox_file, col_select = c(CountryCode, Date, StringencyIndex_Average), show_col_types = FALSE) %>%
  filter(CountryCode %in% c(markets$iso3, gcc_iso)) %>%
  mutate(year = Date %/% 10000, month = (Date %/% 100) %% 100,
         iso3 = ifelse(CountryCode %in% gcc_iso, "GCC", CountryCode)) %>%
  group_by(iso3, CountryCode, year, month) %>%
  summarise(s = mean(StringencyIndex_Average, na.rm = TRUE), .groups = "drop") %>%
  left_join(imf("LP") %>% filter(year == 2020) %>% select(CountryCode = iso3, w = LP), by = "CountryCode") %>%
  group_by(iso3, year, month) %>%
  summarise(covid_stringency = weighted.mean(s, w, na.rm = TRUE), .groups = "drop")
write_csv(covid, "data/raw/covid_monthly.csv")

message("fx: ", nrow(fx), " rows (", n_distinct(fx$currency), " currencies) | macro: ", nrow(macro),
        " rows | covid: ", nrow(covid), " rows")
message("macro NAs: ", paste(names(macro), colSums(is.na(macro)), collapse = ", "))

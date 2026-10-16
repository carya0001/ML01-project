# 02_get_fx_macro.R
# Exchange rates, prices, income and population for the 12 origin markets (all yearly at the source).
#   - SNB data portal, cube "devkua": annual average CHF exchange rates (EUR, GBP, USD, CNY, JPY, KRW)
#   - FRED "AEXINUS": Indian rupees per USD, annual (the SNB does not publish INR)
#   - IMF World Economic Outlook (DataMapper API): inflation (PCPIPCH), GDP per capita PPP (PPPPC),
#     real GDP growth (NGDP_RPCH), population (LP, used to weight the Gulf countries)
# GCC currencies are pegged to the US dollar, so the GCC market uses USD.
# Output: data/raw/fx_yearly.csv, data/raw/fred_AEXINUS.csv, data/raw/macro_yearly.csv,
#         data/raw/origin_static.csv

library(dplyr)
library(tidyr)
library(readr)
library(jsonlite)

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

# ---- Nominal exchange rates: CHF per 1 unit of the origin currency, annual average ----
snb <- read_delim(
  "https://data.snb.ch/api/cube/devkua/data/csv/en?fromDate=2015&toDate=2025&dimSel=D1(EUR1,GBP1,USD1,CNY100,JPY100,KRW100)",
  delim = ";", skip = 3, show_col_types = FALSE) %>%
  transmute(year = as.integer(Date), currency = D1,
            chf_per_unit = Value / ifelse(grepl("100$", D1), 100, 1))

fred_file <- "data/raw/fred_AEXINUS.csv"
download.file("https://fred.stlouisfed.org/graph/fredgraph.csv?id=AEXINUS", fred_file, method = "curl", quiet = TRUE)
fred_inr <- read_csv(fred_file, show_col_types = FALSE) %>%
  transmute(year = as.integer(format(observation_date, "%Y")), inr_per_usd = AEXINUS)
inr <- snb %>% filter(currency == "USD1") %>%
  inner_join(fred_inr, by = "year") %>%
  transmute(year, currency = "INR", chf_per_unit = chf_per_unit / inr_per_usd)

fx <- bind_rows(snb, inr) %>% filter(year %in% 2015:2025)
write_csv(fx, "data/raw/fx_yearly.csv")

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

# ---- Origin features: population (IMF, millions) ----
origin_static <- weo %>%
  filter(iso3 %in% markets$iso3) %>%
  select(iso3, year, population_origin_m = LP)   # GCC: sum of the six (see above)
write_csv(origin_static, "data/raw/origin_static.csv")

message("fx: ", nrow(fx), " rows (", n_distinct(fx$currency), " currencies) | macro: ", nrow(macro), " rows")
message("macro NAs: ", paste(names(macro), colSums(is.na(macro)), collapse = ", "))

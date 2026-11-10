# ----packages-----------------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)

# ----load data----------------------------------------------------------------

hotel <- read_csv("data/processed/swiss_hotel_panel.csv", show_col_types = FALSE)

# ----check and filter data----------------------------------------------------

dim(hotel)                 # 3120 rows = 26 cantons x 12 markets x 10 years
str(hotel)                 # canton, origin_country, origin_group, covid_period are text
colSums(is.na(hotel))      # arrivals, overnights, share_in_canton: 2 NA (hidden by the BFS)

hotel |> filter(is.na(arrivals)) |> select(year, canton, origin_country)
# Appenzell Innerrhoden, GCC and South Korea, 2022: no response, so we drop them

hotel <- hotel |> filter(!is.na(arrivals))

# ----check response-----------------------------------------------------------

summary(hotel$arrivals)
ggplot(hotel, aes(arrivals)) + geom_histogram(bins = 40)
# very right-skewed amount (0 to ~500,000), so it needs a log

sum(hotel$arrivals == 0)
# 25 zeros: log(0) = -Inf, so we use log1p = log(arrivals + 1)

hotel <- hotel |> mutate(log_arrivals = log1p(arrivals))

ggplot(hotel, aes(log_arrivals)) + geom_histogram(bins = 40)
# more symmetric: transformation works

# ----check predictors---------------------------------------------------------

#check levels 
unique(hotel$origin_group)    # near, far
unique(hotel$covid_period)    # pre, during, post
unique(hotel$canton)          # 26 cantons
unique(hotel$origin_country)  # 12 markets

#factor text columns
hotel <- hotel |>
  mutate(
    canton         = factor(canton),
    origin_country = factor(origin_country),
    origin_group   = factor(origin_group, levels = c("near", "far")),
    covid_period   = factor(covid_period, levels = c("pre", "during", "post"))
  )

#check continuous predictor: beds
summary(hotel$beds)
ggplot(hotel, aes(beds)) + geom_histogram(bins = 40)
# right-skewed, no zeros, so we use log

hotel <- hotel |> mutate(log_beds = log(beds))

#check continuous predictor: real exchange-rate index
summary(hotel$real_fx_index)
ggplot(hotel, aes(real_fx_index)) + geom_histogram(bins = 40)
# left-skewed: few low values from weak currencies
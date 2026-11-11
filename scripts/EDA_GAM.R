# ----packages-----------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(
  theme_minimal(base_size = 11) +
    theme(plot.caption = element_text(hjust = 0.5))
)

# ----load data----------------------------------------------------------------

hotel <- read_csv("data/processed/swiss_hotel_panel.csv", show_col_types = FALSE)

#-----------------------------------EDA----------------------------------------

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

group_cols <- c(near = "#0072B2", far = "#E69F00")      # colorblind safe

#check continuous predictor: beds
summary(hotel$beds)
ggplot(hotel, aes(beds)) + geom_histogram(bins = 40)
# right-skewed, no zeros, so we use log

hotel <- hotel |> mutate(log_beds = log(beds))

#check continuous predictor: real exchange-rate index
summary(hotel$real_fx_index)
ggplot(hotel, aes(real_fx_index)) + geom_histogram(bins = 40)
# left-skewed: few low values from weak currencies

#-----------------------------------------------------------
#eda arrivals
hotel |>
  select(arrivals, log_arrivals) |>
  pivot_longer(everything()) |>
  ggplot(aes(value)) +
  geom_histogram(bins = 40) +
  facet_wrap(~ name, scales = "free") +
  labs(x = NULL, y = "Number of observations", 
       subtitle = "Each observation is one canton × source market × year.")

#eda origin
country_order <- hotel |>
  group_by(origin_group, origin_country) |>
  summarise(med = median(log_arrivals), .groups = "drop") |>
  arrange(desc(origin_group), med) |>   
  pull(origin_country)

hotel |>
  mutate(origin_country = factor(origin_country, levels = country_order)) |>
  ggplot(aes(log_arrivals, origin_country, fill = origin_group)) +
  geom_boxplot(outlier.size = 1) +
  scale_fill_manual(values = group_cols) +
  labs(x = "log(arrivals)", y = NULL, fill = "Market")

#eda canton

hotel |>
  group_by(canton, origin_group) |>
  summarise(med = median(log_arrivals), .groups = "drop") |>
  pivot_wider(names_from = origin_group, values_from = med) |>
  mutate(canton = reorder(canton, near)) |>
  ggplot(aes(y = canton)) +
  geom_segment(aes(x = far, xend = near, yend = canton), colour = "grey50") +
  geom_point(aes(x = near, colour = "near"), size = 2.5) +
  geom_point(aes(x = far,  colour = "far"),  size = 2.5) +
  scale_colour_manual(values = group_cols) +
  labs(x = "Median log(arrivals) per market", y = NULL, colour = "Market")

#eda time

hotel |>
  group_by(year, origin_country, origin_group) |>
  summarise(arrivals = sum(arrivals), .groups = "drop") |>
  group_by(origin_country) |>
  mutate(index = 100 * arrivals / arrivals[year == 2019]) |>
  ggplot(aes(year, index, group = origin_country, colour = origin_group)) +
  geom_hline(yintercept = 100, linetype = 2, colour = "grey50") +
  geom_line(alpha = 0.8) +
  geom_point(size = 0.8) +
  scale_colour_manual(values = group_cols) +
  scale_x_continuous(breaks = 2016:2025) +
  labs(x = NULL, y = "National arrivals (2019 = 100)", colour = "Market")

hotel |>
  group_by(year, origin_country, origin_group) |>
  summarise(arrivals = sum(arrivals), .groups = "drop") |>
  group_by(origin_country) |>
  mutate(index = 100 * arrivals / arrivals[year == 2019]) |>
  ungroup() |>
  ggplot(aes(year, index, colour = origin_group)) +
  geom_hline(yintercept = 100, linetype = 2, colour = "grey50") +
  geom_line() +
  geom_point(size = 0.8) +
  facet_wrap(~ origin_country) +
  scale_colour_manual(values = group_cols) +
  scale_x_continuous(breaks = 2020) +
  labs(x = NULL, y = "National arrivals (2019 = 100)", colour = "Market")


#eda fx

ggplot(hotel, aes(real_fx_index, log_arrivals)) +
  geom_point(alpha = 0.2, size = 0.8) +
  geom_smooth() +
  labs(x = "Real exchange-rate index", y = "log(arrivals)")


ggplot(hotel, aes(real_fx_index, log_arrivals, colour = origin_group)) +
  geom_point(alpha = 0.15, size = 0.7) +
  geom_smooth(method = "gam", formula = y ~ s(x)) +
  scale_colour_manual(values = group_cols) +
  labs(x = "Real exchange-rate index (2016 = 100)",
       y = "log(arrivals)", colour = "Market")

#eda beds

ggplot(hotel, aes(log_beds, log_arrivals)) +
  geom_point(alpha = 0.2, size = 0.8) +
  geom_smooth() +
  labs(x = "log(beds)", y = "log(arrivals)")


ggplot(hotel, aes(log_beds, log_arrivals, colour = origin_group)) +
  geom_point(alpha = 0.15, size = 0.7) +
  geom_smooth(method = "gam", formula = y ~ s(x)) +
  scale_colour_manual(values = group_cols) +
  labs(x = "log(hotel beds in canton)", y = "log(arrivals)", colour = "Market")















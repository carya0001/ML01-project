# Linear model for log(arrivals): main effects, then near/far interactions.

# ---------------------------------data----------------------------------------

source("scripts/data_prep_eda.R")
# hotel: 2 NA rows dropped, text columns as factors, log_arrivals = log1p(arrivals)

nrow(hotel)               # 3118
is.factor(hotel$canton)   # TRUE

# ---------------------------------models--------------------------------------

#main effects (no beds and rel_income, see VIF check in data_prep_eda.R)
lm.main <- lm(log_arrivals ~ canton + origin_country + year + covid_period +
                real_fx_index + gdp_growth_origin,
              data = hotel)
summary(lm.main)$r.squared
drop1(lm.main, test = "F")
# fx and gdp growth not significant once country is in the model

#covid, fx and canton effects can differ between near and far markets
lm.int <- lm(log_arrivals ~ canton + origin_country + year + covid_period +
               real_fx_index + gdp_growth_origin +
               origin_group:real_fx_index + origin_group:covid_period +
               canton:origin_group,
             data = hotel)
summary(lm.int)$r.squared

#check if the interactions are needed
anova(lm.main, lm.int)
drop1(lm.int, test = "F")

# ------------------------------final model------------------------------------

#effects in % change of arrivals: 100 * (exp(b) - 1)
#for far markets: main effect + interaction
b <- coef(lm.int)
V <- vcov(lm.int)

pct_effect <- function(terms) {
  est <- sum(b[terms])
  se  <- sqrt(sum(V[terms, terms]))
  round(100 * (exp(est + c(-1.96, 0, 1.96) * se) - 1), 1)
}

effects <- rbind(
  "year (+1)"           = pct_effect("year"),
  "during covid, near"  = pct_effect("covid_periodduring"),
  "during covid, far"   = pct_effect(c("covid_periodduring", "covid_periodduring:origin_groupfar")),
  "post covid, near"    = pct_effect("covid_periodpost"),
  "post covid, far"     = pct_effect(c("covid_periodpost", "covid_periodpost:origin_groupfar")),
  "fx index +1, near"   = pct_effect("real_fx_index"),
  "fx index +1, far"    = pct_effect(c("real_fx_index", "real_fx_index:origin_groupfar")),
  "gdp growth +1 pp"    = pct_effect("gdp_growth_origin")
)
colnames(effects) <- c("lower", "estimate", "upper")
effects

#which cantons attract far markets more than near markets (vs Aargau)
ci <- confint(lm.int)
k  <- grep("^canton.*:origin_groupfar$", names(b))
canton_far <- data.frame(
  canton = sub("^canton(.*):origin_groupfar$", "\\1", names(b)[k]),
  est    = 100 * (exp(b[k]) - 1),
  lower  = 100 * (exp(ci[k, 1]) - 1),
  upper  = 100 * (exp(ci[k, 2]) - 1)
)

ggplot(canton_far, aes(est, reorder(canton, est))) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_pointrange(aes(xmin = lower, xmax = upper), colour = group_cols["far"]) +
  labs(x = "Extra % arrivals of far markets compared with near markets (vs Aargau)",
       y = NULL)

#residual checks
par(mfrow = c(2, 2))
plot(lm.int)
par(mfrow = c(1, 1))

#-----------------------------cross validation---------------------------------

set.seed(10)
n     <- nrow(hotel)
folds <- sample(rep(1:10, length.out = n))

rmse.main <- c()
rmse.int  <- c()

for (k in 1:10) {
  hotel.train <- hotel[folds != k, ]
  hotel.test  <- hotel[folds == k, ]

  fit  <- lm(formula(lm.main), data = hotel.train)
  pred <- predict(fit, newdata = hotel.test)
  rmse.main[k] <- sqrt(mean((hotel.test$log_arrivals - pred)^2))

  fit  <- lm(formula(lm.int), data = hotel.train)
  pred <- predict(fit, newdata = hotel.test)
  rmse.int[k] <- sqrt(mean((hotel.test$log_arrivals - pred)^2))
}

mean(rmse.main)
mean(rmse.int)

# GAM: non-linear model for log(arrivals), compared against LM 2

# ---------------------------------data----------------------------------------

source("scripts/data_prep_eda.R")

#packages
library(mgcv)

# ---------------------------------models--------------------------------------
# no covid_period: the year curve can follow each year separately and show
# the covid drop and recovery directly
# canton and origin_country stay as factors: each keeps its own level, so the
# curves show changes within the same canton and market
# log_beds and rel_income left out (collinear with canton/origin_country from VIF)

#GAM 1: main effects
gam.main <- gam(log_arrivals ~ canton + origin_country +
                  s(year, k = 10) + s(real_fx_index) + s(gdp_growth_origin),
                data = hotel)
summary(gam.main)


#GAM 2: time and fx effect differ between near and far markets
# origin_group adds nothing on top of origin_country, it is only needed for by =
gam.noint <- gam(
  log_arrivals ~ canton + origin_group + origin_country +
    s(year, by = origin_group, k = 10) +
    s(real_fx_index, by = origin_group) +
    s(gdp_growth_origin),
  data = hotel
)
summary(gam.noint)

anova(gam.main, gam.noint, test = "F")

#GAM 3: canton x origin_group interaction
# each canton's level can differ between near and far markets
gam.int <- gam(
  log_arrivals ~ canton * origin_group + origin_country +
    s(year, by = origin_group, k = 10) +
    s(real_fx_index, by = origin_group) +
    s(gdp_growth_origin),
  data = hotel
)

anova(gam.noint, gam.int, test = "F")
# improves the fit again: final GAM
# year has only 10 distinct values, so k = 10 is the most flexible it can be

# ------------------------------final model------------------------------------
summary(gam.int)
anova(gam.int)
# canton x origin_group clearly significant: near and far markets do not go to 
# the same cantons (canton and origin_group not interpreted on their own (marginality)
# all smooths significant, except exchange rate for near markets: no evidence 
# that it changes arrivals of near markets
# "Rank: 107/108": origin_group is redundant with origin_country (each
# country is either near or far), so R drops it automatically; we keep the term 
# so that the main effect stays in the model together with its interaction 

#final LM from the LM section for comparison
lm.int <- lm(log_arrivals ~ canton + origin_country + year + covid_period +
               real_fx_index + gdp_growth_origin +
               origin_group:real_fx_index + origin_group:covid_period +
               canton:origin_group,
             data = hotel)

summary(gam.int)$dev.expl # GAM 3 explains about 91 % of the variation
summary(gam.int)$r.sq     # adjusted R^2 0.903
summary(lm.int)$r.squared # 0.895 for LM 2: almost the same
# smooth curves add not much on top of the near/far interactions  

# smooth curves on the log scale: +0.1 = about 10% more arrivals,
# +1 = almost 3 times as many, -1 = about 63% fewer
par(mfrow = c(3, 2))
plot(gam.int, select = 1, shade = TRUE, main = "Year, near markets")
plot(gam.int, select = 2, shade = TRUE, main = "Year, far markets")
plot(gam.int, select = 3, shade = TRUE, main = "Exchange rate, near markets")
plot(gam.int, select = 4, shade = TRUE, main = "Exchange rate, far markets")
plot(gam.int, select = 5, shade = TRUE, main = "GDP growth of origin")
par(mfrow = c(1, 1))
# year: both non-linear, sharp drop 2020-2021, then recovery; near markets back 
# at 2019 level by 2023, far markets only in 2025
# exchange rate, near: almost a straight line, not significant
# exchange rate, far: rising but not steadily; it may partly reflect slow
# post covid recovery, so not read as a clear currency effect
# GDP growth: very wiggly, lowest around -4%, highest around 6-7%; goes up and 
# down: no clear pattern

#model checks
par(mfrow = c(2, 2))
gam.check(gam.int)
par(mfrow = c(1, 1))
# QQ-plot: most points on the line, lower tail clearly below, so not fully normal
# residuals vs linear predictor: centred on 0, more spread for small predictions;
# the diagonal line on the left are 25 rows with zero arrivals
# histogram: roughly symmetric bell, long left tail (zeros)
# response vs fitted: close to the diagonal, except the zero-arrival rows:
# assumptions reasonably met; only clear problem: zero rows pulled far down
# by the log scale (as in the LM)
# k-index slightly below 1 for all smooths: k cannot be raised for year (10 years),
# and a larger k for GDP would make curve more wiggler

#-----------------------------cross validation---------------------------------

#compare with the final LM (LM 2)
# repeated 10-fold CV: 10 repetitions x 10 folds, each fold 90% train/10% test
# all models use the same folds in each repetition for fair comparison
set.seed(10)
n <- nrow(hotel)

rmse.gam.main  <- c()
rmse.gam.noint <- c()
rmse.gam.int   <- c()
rmse.lm.int    <- c()

for(i in 1:10){
  # 1) prepare data: new random permutation each repetition
  inds.permuted  <- sample(1:n, replace = FALSE)
  hotel.permuted <- hotel[inds.permuted, ]
  folds <- cut(1:n, breaks = 10, labels = FALSE)
  
  rmse.fold.main  <- c()
  rmse.fold.noint <- c()
  rmse.fold.int   <- c()
  rmse.fold.lm    <- c()
  
  for(k in 1:10){
    ind.test    <- which(folds == k)
    hotel.test  <- hotel.permuted[ind.test, ]
    hotel.train <- hotel.permuted[-ind.test, ]
    
    # 2) fit on train, 3) predict test, 4) compute RMSE
    fit  <- gam(formula(gam.main), data = hotel.train)
    pred <- predict(fit, newdata = hotel.test)
    rmse.fold.main[k] <- sqrt(mean((hotel.test$log_arrivals - pred)^2))
    
    fit  <- gam(formula(gam.noint), data = hotel.train)
    pred <- predict(fit, newdata = hotel.test)
    rmse.fold.noint[k] <- sqrt(mean((hotel.test$log_arrivals - pred)^2))
    
    fit  <- gam(formula(gam.int), data = hotel.train)
    pred <- predict(fit, newdata = hotel.test)
    rmse.fold.int[k] <- sqrt(mean((hotel.test$log_arrivals - pred)^2))
    
    fit  <- lm(formula(lm.int), data = hotel.train)
    pred <- predict(fit, newdata = hotel.test)
    rmse.fold.lm[k] <- sqrt(mean((hotel.test$log_arrivals - pred)^2))
  }
  
  # average over 10 folds
  rmse.gam.main[i]  <- mean(rmse.fold.main)
  rmse.gam.noint[i] <- mean(rmse.fold.noint)
  rmse.gam.int[i]   <- mean(rmse.fold.int)
  rmse.lm.int[i]    <- mean(rmse.fold.lm)
}

mean(rmse.gam.main)     # 0.92
mean(rmse.gam.noint)    # 0.89
mean(rmse.gam.int)      # 0.78
mean(rmse.lm.int)       # 0.81      

boxplot(rmse.gam.main, rmse.gam.noint, rmse.gam.int, rmse.lm.int,
        names = c("GAM 1", "GAM 2", "GAM 3", "LM 2"),
        ylab = "Test RMSE")
# GAM 3 and LM 2 boxes do not overlap: the ranking does not depend on the split
# gain over LM 2 is small; the near/far interactions add much more than the
# smooth curves

#-----------------------------time split check---------------------------------
# random folds = filling in canton-market-years the model has not seen;
# the agency wants the next years, so we fit 2016-2023, predict 2024-2025 (~80/20)

hotel.train <- hotel[hotel$year <= 2023, ]
hotel.test  <- hotel[hotel$year >= 2024, ]

# only 8 training years, so year smooth can have max k = 8
f.time <- update(formula(gam.int), . ~ . - s(year, by = origin_group, k = 10) +
                   s(year, by = origin_group, k = 8))

fit  <- gam(f.time, data = hotel.train)
pred <- predict(fit, newdata = hotel.test)
rmse.time.gam <- sqrt(mean((hotel.test$log_arrivals - pred)^2))

fit  <- lm(formula(lm.int), data = hotel.train)
pred <- predict(fit, newdata = hotel.test)
rmse.time.lm <- sqrt(mean((hotel.test$log_arrivals - pred)^2))

rmse.time.gam   # 0.95
rmse.time.lm    # 0.79
# for 2024-2025 the LM has the lower error, so it predicts new years better
# with random folds the GAM had the lower error

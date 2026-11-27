# Support Vector Machine: predicting year-over-year growth compared with the binomial GLM

#packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(e1071)   
library(caret)   

# ---------------------------------data----------------------------------------

hotel <- read_csv("data/processed/swiss_hotel_panel.csv") |>
  filter(!is.na(arrivals)) |>
  mutate(canton       = factor(canton),
         origin_group = factor(origin_group, levels = c("near", "far")))

# response growth as a factor so that svm() does classification 
# keep only the columns we need because tune() drops rows with NA in any column
svm.data <- hotel |>
  filter(!is.na(growth)) |>
  mutate(growth = factor(growth, levels = c(0, 1), labels = c("decline", "growth"))) |>
  select(year, growth, gdp_growth_origin, real_fx_index, rel_income,
         visa_required, origin_group, canton) |>
  as.data.frame()

# baseline always predicting the most common class (no info rate)
prop.table(table(svm.data$growth))
# around 60% growth

# the GLM predictors only change between origin countries and years
# 3116 rows but only 120 different values
nrow(distinct(svm.data, gdp_growth_origin, real_fx_index, rel_income,
              visa_required, origin_group))

# -----------------------------train/test split--------------------------------

set.seed(10)
svm.ind   <- createDataPartition(svm.data$growth, p = 0.7, list = FALSE)
svm.train <- svm.data[svm.ind, ]
svm.test  <- svm.data[-svm.ind, ]

# same predictors as GLM
f.svm <- growth ~ gdp_growth_origin + real_fx_index + rel_income + 
  visa_required + origin_group

# SVM 3 adds canton, since it is the only predictor that changes within a 
# market-year, and the LM and GAM found a strong canton x market group effect 
# on arrivals. CV tests whether it also helps to predict growth direction.
f.canton <- update(f.svm, . ~ . + canton)

# ---------------three SVMs, tuned with 10-fold CV (tune())--------------------
# svm() scales the predictors by default, since svm uses distances
# cost: large -> narrow margin, few sv, risk of overfitting
#       small -> wide margin, many sv, simpler model
# gamma: large -> each support vector only affects close points, wiggly boundary
# same seed before each tune(), same folds, fair comparison

# SVM 1: linear kernel (straight boundary, like GLM)
set.seed(10)
tune.lin <- tune(svm, f.svm, data = svm.train, kernel = "linear",
                 ranges = list(cost = c(0.01, 0.1, 1, 10)))

# SVM 2: radial kernel (curved boundary), tune cost and gamma
set.seed(10)
tune.rad <- tune(svm, f.svm, data = svm.train, kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100),
                               gamma = c(0.01, 0.1, 1, 10)))

# SVM 3: radial kernel + canton
set.seed(10)
tune.can <- tune(svm, f.canton, data = svm.train, kernel = "radial",
                 ranges = list(cost = c(1, 10, 100), gamma = c(0.01, 0.1, 1)))

# best tuning values, best CV error and the error for every combination
summary(tune.lin)
summary(tune.rad)
summary(tune.can)

# $best.model is refit on the whole training set with the best values
svm.lin <- tune.lin$best.model
svm.rad <- tune.rad$best.model
svm.can <- tune.can$best.model

# number of support vectors of each best model
svm.lin
svm.rad
svm.can

# CV errors
c(linear = tune.lin$best.performance,
  radial = tune.rad$best.performance,
  canton = tune.can$best.performance)
# SVM 2, lowest error, with large gamma
# best cost (100), larger cost = narrower margin = more overfitting risk

nrow(svm.train)
# compare with number of support vectors

# decision regions in 2D: the SVM uses 5 predictors but a plot shows only 2
# (GDP growth vs exchange rate), so the other 3 are fixed at typical values:
# median relative income, no visa required, near market
# x = support vectors, o = other points (outside the margin, no effect on the model)
# colour of the area = predicted class
plot(svm.lin, svm.train, gdp_growth_origin ~ real_fx_index,
     slice = list(rel_income    = median(svm.train$rel_income),
                  visa_required = 0, origin_group = "near"))
plot(svm.rad, svm.train, gdp_growth_origin ~ real_fx_index,
     slice = list(rel_income    = median(svm.train$rel_income),
                  visa_required = 0, origin_group = "near"))
# SVM 1: one straight boundary, "decline" only when GDP falls below about -3 to -5 %
# same kind of boundary as the GLM, but the GLM predicts "decline" more often
# (higher specificity): e.g. at FX index 100 the GLM predicts "decline" below
# about 0 % GDP growth, SVM 1 only below about -3 %
# SVM 2: separate "decline" islands = very wiggly boundary from the large gamma
# (it recognises single market-years)
# note: the fixed values create combinations that do not exist in the data
# (plot shows the shape of the boundary, not real predictions)

# ----------------------------------test---------------------------------------

# GLM with the same predictors, refit on the same training rows
glm.svm.split <- glm(f.svm, family = binomial, data = svm.train)

# predicted classes on the test set
# predict() only uses the predictors from the formula, the growth column in
# svm.test is ignored (= test_in); svm.test$growth is the truth (= test_truth)
# SVM 1 (linear) is kept as a link between the GLM and SVM 2(log-loss vs hinge loss)
# GLM vs SVM 1 = effect of the method, SVM 1 vs SVM 2 = effect of the curved boundary
pred.glm <- factor(ifelse(predict(glm.svm.split, svm.test, type = "response") > 0.5,
                          "growth", "decline"), levels = c("decline", "growth"))
pred.lin <- predict(svm.lin, svm.test)
pred.rad <- predict(svm.rad, svm.test)

# confusion matrices; positive = "growth", otherwise "decline"
# sensitivity = share of growing rows found, specificity = share of declines found
confusion.matrix.glm <- confusionMatrix(pred.glm, svm.test$growth, positive = "growth")
confusion.matrix.lin <- confusionMatrix(pred.lin, svm.test$growth, positive = "growth")
confusion.matrix.rad <- confusionMatrix(pred.rad, svm.test$growth, positive = "growth")

confusion.matrix.glm
confusion.matrix.lin
confusion.matrix.rad
# Accuracy: GLM and SVM 1 ~68 %, SVM 2 ~75 %, NIR ~60 %
# GLM and SVM 1 mostly predict "growth" (high sensitivity, low specificity)

# share of rows with growth per year: observed, GLM and SVM 2
data.frame(year = svm.test$year,
           Observed = svm.test$growth == "growth",
           GLM = pred.glm == "growth",
           `SVM 2` = pred.rad == "growth",
           check.names = FALSE) |>
  pivot_longer(-year, names_to = "source", values_to = "growth") |>
  group_by(year, source) |>
  summarise(share = mean(growth), .groups = "drop") |>
  ggplot(aes(year, share, colour = source, linetype = source)) +
  geom_line() +
  geom_point(size = 1.2) +
  scale_colour_manual(values = c(GLM = "grey50", Observed = "black", `SVM 2` = "#E69F00")) +
  scale_linetype_manual(values = c(GLM = 2, Observed = 1, `SVM 2` = 1)) +
  scale_x_continuous(breaks = 2016:2025) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Share of rows with growth", colour = NULL, linetype = NULL) +
  theme_minimal()
# both catch 2020
# GLM predicts growth everywhere in 2021 (but only ~60 % grew); SVM follows
# recovery closely in 2021-23
# before covid both predict ~90 % growth while ~60 % grew (cantons split
# within a market-year, models can only predict the majority)

# -----------------GLM vs SVM with 10-fold CV on the same folds-----------------

# the test set above is one split, and one (un)lucky split can favour one model, 
# so we compare them over 10 different splits/folds: in each fold they are 
# trained and tested on the same rows, so differences come from models, not rows.

set.seed(10)
n <- nrow(svm.data)
svm.data.perm <- svm.data[sample(1:n), ]          # permute rows
folds <- cut(1:n, breaks = 10, labels = FALSE)    # cut into 10 folds

acc.glm <- c()
acc.lin <- c()
acc.rad <- c()

for (k in 1:10) {
  cv.test  <- svm.data.perm[folds == k, ]
  cv.train <- svm.data.perm[folds != k, ]
  
  # GLM: probability > 0.5 -> growth
  fit  <- glm(f.svm, family = binomial, data = cv.train)
  pred <- ifelse(predict(fit, cv.test, type = "response") > 0.5, "growth", "decline")
  acc.glm[k] <- mean(pred == cv.test$growth)
  
  # SVM 1: linear
  fit  <- svm(f.svm, data = cv.train, kernel = "linear",
              cost = tune.lin$best.parameters$cost)
  acc.lin[k] <- mean(predict(fit, cv.test) == cv.test$growth)
  
  # SVM 2: radial
  fit  <- svm(f.svm, data = cv.train, kernel = "radial",
              cost  = tune.rad$best.parameters$cost,
              gamma = tune.rad$best.parameters$gamma)
  acc.rad[k] <- mean(predict(fit, cv.test) == cv.test$growth)
}

# mean accuracy over the 10 folds
c("GLM (binomial)" = mean(acc.glm),
  "SVM 1: linear"  = mean(acc.lin),
  "SVM 2: radial"  = mean(acc.rad)) |>
  round(3)

# spread over the folds: does the ranking depend on the split?
boxplot(acc.glm, acc.lin, acc.rad,
        names = c("GLM", "SVM 1: linear", "SVM 2: radial"),
        ylab = "Accuracy per fold")
abline(h = mean(svm.data$growth == "growth"), lty = 2)   # always "growth"

# SVM 2 ~74 %, GLM and SVM 1 ~68 %; SVM 2 is better in all 10 folds,
# so the ranking does not depend on one lucky split

# -----------------------train 2016-2023, test 2024-2025-----------------------

# random split = market-years already seen in other cantons
# time split = new years, which is what the agency cares about

svm.train.t <- svm.data[svm.data$year <= 2023, ]
svm.test.t  <- svm.data[svm.data$year >= 2024, ]

# same tuning values as above, no re-tuning
fit.glm.t <- glm(f.svm, family = binomial, data = svm.train.t)
fit.lin.t <- svm(f.svm, data = svm.train.t, kernel = "linear",
                 cost = tune.lin$best.parameters$cost)
fit.rad.t <- svm(f.svm, data = svm.train.t, kernel = "radial",
                 cost  = tune.rad$best.parameters$cost,
                 gamma = tune.rad$best.parameters$gamma)

c("Always 'growth'" = mean(svm.test.t$growth == "growth"),
  "GLM (binomial)"  = mean(ifelse(predict(fit.glm.t, svm.test.t, type = "response") > 0.5,
                                  "growth", "decline") == svm.test.t$growth),
  "SVM 1: linear"   = mean(predict(fit.lin.t, svm.test.t) == svm.test.t$growth),
  "SVM 2: radial"   = mean(predict(fit.rad.t, svm.test.t) == svm.test.t$growth))
# no model beats "always growth" (~64 %)
# SVM 1 = "always growth" (it predicts growth for all rows)
# GLM predicts decline only 26 times and is slightly worse
# SVM 2 worst (~61 %): it learned past market-years by heart and does not 
# carry over to new years (same lesson as the GAM time split)


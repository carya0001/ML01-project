# GAM: non-linear model for log(arrivals), compared with an equivalent LM.

# ---------------------------------data----------------------------------------

source("scripts/data_prep_eda.R")

#packages
library(mgcv)

# ---------------------------------models--------------------------------------

#main effects
gam.main <- gam(log_arrivals ~ canton + origin_country +
                  s(year, k = 10) + s(real_fx_index) + s(gdp_growth_origin),
                data = hotel)
summary(gam.main)


#time and fx effect differ between near and far markets
gam.noint <- gam(
  log_arrivals ~ canton + origin_group + origin_country +
    s(year, by = origin_group, k = 10) +
    s(real_fx_index, by = origin_group) +
    s(gdp_growth_origin),
  data = hotel
)
summary(gam.noint)

anova(gam.main, gam.noint, test = "F")

#canton x origin_group interaction
gam.int <- gam(
  log_arrivals ~ canton * origin_group + origin_country +
    s(year, by = origin_group, k = 10) +
    s(real_fx_index, by = origin_group) +
    s(gdp_growth_origin),
  data = hotel
)

#check if interaction is needed
anova(gam.noint, gam.int, test = "F")

# ------------------------------final model------------------------------------
summary(gam.int)

gam.check(gam.int)

plot(gam.int, select = 1, shade = TRUE)
plot(gam.int, select = 2, shade = TRUE)
plot(gam.int, select = 3, shade = TRUE)
plot(gam.int, select = 4, shade = TRUE)
plot(gam.int, select = 5, shade = TRUE)

#-----------------------------cross validation---------------------------------

#linear model with the same structure as the final GAM (fair comparison)
lm.equiv <- log_arrivals ~ canton * origin_group + origin_country +
  factor(year) * origin_group + real_fx_index * origin_group +
  gdp_growth_origin

set.seed(10)
n <- nrow(hotel)

mse.gam.main  <- c()
mse.gam.noint <- c()
mse.gam.int   <- c()
mse.lm.equiv  <- c()

for(i in 1:20){
  # 1) prepare data: new random permutation each repetition
  inds.permuted  <- sample(1:n, replace = FALSE)
  hotel.permuted <- hotel[inds.permuted, ]
  folds <- cut(1:n, breaks = 10, labels = FALSE)
  
  mse.fold.main  <- c()
  mse.fold.noint <- c()
  mse.fold.int   <- c()
  mse.fold.lm    <- c()
  
  for(k in 1:10){
    ind.test    <- which(folds == k)
    hotel.test  <- hotel.permuted[ind.test, ]
    hotel.train <- hotel.permuted[-ind.test, ]
    
    # 2) fit on train, 3) predict test, 4) compute MSE
    fit  <- gam(formula(gam.main), data = hotel.train)
    pred <- predict(fit, newdata = hotel.test)
    mse.fold.main[k] <- mean((hotel.test$log_arrivals - pred)^2)
    
    fit  <- gam(formula(gam.noint), data = hotel.train)
    pred <- predict(fit, newdata = hotel.test)
    mse.fold.noint[k] <- mean((hotel.test$log_arrivals - pred)^2)
    
    fit  <- gam(formula(gam.int), data = hotel.train)
    pred <- predict(fit, newdata = hotel.test)
    mse.fold.int[k] <- mean((hotel.test$log_arrivals - pred)^2)
    
    fit  <- lm(lm.equiv, data = hotel.train)
    pred <- predict(fit, newdata = hotel.test)
    mse.fold.lm[k] <- mean((hotel.test$log_arrivals - pred)^2)
  }
  
  # average over 10 folds
  mse.gam.main[i]  <- mean(mse.fold.main)
  mse.gam.noint[i] <- mean(mse.fold.noint)
  mse.gam.int[i]   <- mean(mse.fold.int)
  mse.lm.equiv[i]  <- mean(mse.fold.lm)
}

mean(mse.gam.main)
mean(mse.gam.noint)
mean(mse.gam.int)
mean(mse.lm.equiv)

boxplot(mse.gam.main, mse.gam.noint, mse.gam.int, mse.lm.equiv,
        names = c("GAM 1", "GAM 2", "GAM 3", "LM"),
        ylab = "Test MSE")

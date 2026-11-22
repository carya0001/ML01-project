# GAM: non-linear model for log(arrivals), compared against LM 2

# ---------------------------------data----------------------------------------

source("scripts/data_prep_eda.R")

#packages
library(mgcv)

# ---------------------------------models--------------------------------------

#GAM 1: main effects
gam.main <- gam(log_arrivals ~ canton + origin_country +
                  s(year, k = 10) + s(real_fx_index) + s(gdp_growth_origin),
                data = hotel)
summary(gam.main)


#GAM 2: time and fx effect differ between near and far markets
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
gam.int <- gam(
  log_arrivals ~ canton * origin_group + origin_country +
    s(year, by = origin_group, k = 10) +
    s(real_fx_index, by = origin_group) +
    s(gdp_growth_origin),
  data = hotel
)

anova(gam.noint, gam.int, test = "F")

# ------------------------------final model------------------------------------
summary(gam.int)

#final LM from the LM section for comparison
lm.int <- lm(log_arrivals ~ canton + origin_country + year + covid_period +
               real_fx_index + gdp_growth_origin +
               origin_group:real_fx_index + origin_group:covid_period +
               canton:origin_group,
             data = hotel)

summary(gam.int)$dev.expl    
summary(gam.int)$r.sq        
summary(lm.int)$r.squared    

par(mfrow = c(3, 2))
plot(gam.int, select = 1, shade = TRUE, main = "Year, near markets")
plot(gam.int, select = 2, shade = TRUE, main = "Year, far markets")
plot(gam.int, select = 3, shade = TRUE, main = "Exchange rate, near markets")
plot(gam.int, select = 4, shade = TRUE, main = "Exchange rate, far markets")
plot(gam.int, select = 5, shade = TRUE, main = "GDP growth of origin")
par(mfrow = c(1, 1))

#model checks
par(mfrow = c(2, 2))
gam.check(gam.int)
par(mfrow = c(1, 1))

#-----------------------------cross validation---------------------------------

#compare with the final LM (LM 2), RMSE instead of MSE (same units as log(arrivals))
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

mean(rmse.gam.main)     
mean(rmse.gam.noint)    
mean(rmse.gam.int)      
mean(rmse.lm.int)       

boxplot(rmse.gam.main, rmse.gam.noint, rmse.gam.int, rmse.lm.int,
        names = c("GAM 1", "GAM 2", "GAM 3", "LM 2"),
        ylab = "Test RMSE")

#-----------------------------time split check---------------------------------

hotel.train <- hotel[hotel$year <= 2023, ]
hotel.test  <- hotel[hotel$year >= 2024, ]

f.time <- update(formula(gam.int), . ~ . - s(year, by = origin_group, k = 10) +
                   s(year, by = origin_group, k = 8))

fit  <- gam(f.time, data = hotel.train)
pred <- predict(fit, newdata = hotel.test)
rmse.time.gam <- sqrt(mean((hotel.test$log_arrivals - pred)^2))

fit  <- lm(formula(lm.int), data = hotel.train)
pred <- predict(fit, newdata = hotel.test)
rmse.time.lm <- sqrt(mean((hotel.test$log_arrivals - pred)^2))

rmse.time.gam   
rmse.time.lm    

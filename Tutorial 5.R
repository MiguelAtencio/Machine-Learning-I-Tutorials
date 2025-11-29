library(tidyverse)
library(tidymodels)
library(ISLR2)
library(leaps)
library(glmnet)

# Exercise 1
#1.1
boston_df <- Boston

head(boston_df)

split <- initial_split(boston_df, prop=0.8)
training_data <- training(split)
testing_data <- testing(split)


#1.2
comp_subset <- regsubsets(medv ~ ., nvmax=12, data=training_data) #Nvmax is p - 1 since we use one as our target variable

summary(comp_subset)
names(summary(comp_subset))

summary(comp_subset)$bic
which.min(summary(comp_subset)$bic)
# Best model appears to be with 10 predictors with BIC
which.min(summary(comp_subset)$cp)
# Best model appears to be with 10 predictors with BIC

coef(comp_subset, 10)

# The variables that are not included in the model are
# Age and Industry

#1.3
comp_subset_for <- regsubsets(medv ~ ., nvmax=12, method="forward", data=training_data)

summary(comp_subset_for)

summary(comp_subset_for)$bic
which.min(summary(comp_subset_for)$bic)
# Best model is with 10 predictors
which.min(summary(comp_subset_for)$cp)
# Best model is with 10 predictors

coef(comp_subset_for, 10)

comp_subset_back <- regsubsets(medv ~ ., nvmax=12, method="backward", data=training_data)

summary(comp_subset_back)

summary(comp_subset_back)$bic
which.min(summary(comp_subset_back)$bic)
# Best model is with 10 predictors
which.min(summary(comp_subset_back)$bic)
# Best model is with 10 predictors

coef(comp_subset_back, 10)

# The results are the same for the exhaustive, forward, and backward selection.
# We are finding the global minimum for BIC and Cp when using Best subset selection.

#1.4

# Separating into training and testing datasets
x_training <- training_data %>% select(-medv)
y_training <- training_data$medv
x_test <- testing_data %>% select(-medv)
y_test <- testing_data$medv


# Creating the lambda grid
lambda_grid <- 10^seq(3, -2, length=100)
# Running ridge regression for the grid of lambdas
ridge_reg <- glmnet(as.matrix(x_training), y_training, alpha=0, lambda=lambda_grid)


chosen_lambda <- c(1, 50, 100)

# Running OLS
OLS_reg <- lm(y_training ~ as.matrix(x_training), training_data)

round(cbind(coef(ridge_reg)[,chosen_lambda], coef(OLS_reg)), digits=5)
# The coefficients of lambda = ~0.01 and OLS are very similar

plot(ridge_reg, xvar="lambda")

#As can be seen from the plot, the coefficients are close to the OLS coefficients when lambda is very close to 0 (the right hand side of the plot)



# Exercise 5
#i)
ridge_LOOCV <- cv.glmnet(as.matrix(x_training), y_training, alpha = 0, lambda=lambda_grid, nfolds=length(y_training))
bestlam_LOOCV <- ridge_LOOCV$lambda.min
#ii)
ridge_5_fold <- cv.glmnet(as.matrix(x_training), y_training, alpha = 0, lambda=lambda_grid, nfolds=5)
bestlam_5_fold <- ridge_5_fold$lambda.min
#iii)
ridge_10_fold <- cv.glmnet(as.matrix(x_training), y_training, alpha = 0, lambda=lambda_grid, nfolds=10)
bestlam_10_fold <- ridge_10_fold$lambda.min

c(bestlam_LOOCV, bestlam_5_fold, bestlam_10_fold)
# By testing with multiple cross validations, it looks like the highest lambda is given by LOOCV
# while the lowest is by the one with 5 folds

# Exercise 6

# Running LASSO regression
lasso_reg <- glmnet(as.matrix(x_training), y_training, alpha = 1, lambda=lambda_grid)
round(cbind(coef(lasso_reg)[,chosen_lambda], coef(OLS_reg)), digits=5)
# In this case, the lower the lambda closest it is to OLS
# The image is flipped horizontally here, so the more to the right it is, the closest it is to OLS

plot(lasso_reg, xvar="lambda")

# Exercise 7

lasso_5_folds<- cv.glmnet(as.matrix(x_training), y_training, alpha = 1, lambda=lambda_grid, nfolds=5)
best_lambda_lasso_5 <- lasso_5_folds$lambda.min
lasso_5_coef <- predict(lasso_5_folds, type = "coefficients", s = best_lambda_lasso_5)

#Exercise 8
X <- model.matrix(medv ~ ., boston_df)[, -1]
y <- boston_df$medv

# Creating the cv ridge using the whole dataset
ridge_cv <- cv.glmnet(X, y, alpha=0, lambda=lambda_grid, nfolds=5)
bestlambda_ridge <- ridge_cv$lambda.min #Getting minimum lambda
ridge_cv_pred <- predict(ridge_cv, s=bestlambda_ridge, newx=X)

# Creating the cv lasso using the whole dataset
lasso_cv <- cv.glmnet(X, y, alpha=1, lambda=lambda_grid, nfolds=5)
bestlambda_lasso <- lasso_cv$lambda.min
lasso_cv_pred <- predict(lasso_cv, s=bestlambda_lasso, newx=X)

# Creating an OLS using the whole dataset
OLS <- lm(medv ~ ., data=boston_df)
OLS_pred <- predict(OLS, newx=boston_df)

summary(ridge_cv_pred)
summary(lasso_cv_pred)
summary(OLS_pred)

cor(OLS_pred, ridge_cv_pred)
cor(OLS_pred, lasso_cv_pred)


# 1.9
set.seed(1)

sample_id <- sample(1:dim(boston_df)[1], dim(boston_df)[1], replace=FALSE)

train_idx <- sample_id[1:404]
test_idx <- sample_id[405:506]

# Train and test
X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test <- X[test_idx, ]
y_test <- y[test_idx]


# Ridge
ridge_final <- cv.glmnet(X_train, y_train, alpha=0, nfolds=5)
best_k <- ridge_final$lambda.min
ridge_final_coef <- predict(ridge_final, type="coefficients", s=best_k)
ridge_final_pred <- predict(ridge_final, s=best_k, newx=X_test)


#Lasso
lasso_final <- cv.glmnet(X_train, y_train, alpha=1, nfolds=5)
best_k1 <- lasso_final$lambda.min
lasso_final_pred <- predict(lasso_final, s=best_k1, newx=X_test)

#Elastic net
EN_final <- cv.glmnet(X_train, y_train, alpha=0.5, nfolds=5)
best_k2 <- EN_final$lambda.min
EN_final_pred <- predict(EN_final, s=best_k2, newx=X_test)

#OLS
OLS_final <- lm(medv ~ ., data=boston_df[train_idx, ])


mse <- matrix(0,1,4)
colnames(mse) <- c("OLS","L2","L1","ENET")


OLS_pred <- predict(OLS_final, newdata=boston_df[test_idx, ])

mse[1, "OLS"] <- mean((y_test - OLS_pred)^2)
mse[1, "L2"] <- mean((y_test - ridge_final_pred)^2)
mse[1, "L1"] <- mean((y_test - lasso_final_pred)^2)
mse[1, "ENET"] <- mean((y_test - EN_final_pred)^2)

mse/mse[1, 1]
#ridge has 1.5% gain over ols.
#lasso and elastic net are comparable to ols.
#Results are not surprising because p=12 in is small relative to n=506. 
#High dimensional models excel relative to ols when p/n is high


#Exercise 2

# 2.1
df_2 <- Smarket

glimpse(df_2)

df_2$Direction <- (df_2$Direction == "Up")*1 

training_df <- df_2 %>% filter(Year != 2005)
test_df <- df_2 %>% filter(Year == 2005)

training_df <- training_df %>% select(-Year, -Today)
test_df <- test_df %>% select(-Year, -Today)


# 2.2
logit_fit <- glm(Direction ~ ., family=binomial, data=training_df)

MSE_logit <- sum((logit_fit$fitted.values - training_df$Direction)^2)

library(bestglm)

training_bglm <- training_df[, c("Lag1", "Lag2", "Lag3", 
                             "Lag4", "Lag5", "Volume", "Direction")]

logit_fit_AIC <- bestglm(training_bglm, family=binomial, IC = "AIC", method="exhaustive")

logit_fit_AIC$BestModel$coefficients

# When looking at all the subsets it appears that the variables we chose are not good enough to predict 
# whether the direction of the stock will be up or down. Therefore, our best model only contains an intercept.

# Since the intercept is only 0.07, the probability that the direction is up is 51% (exp(0.07)/(1+exp(0.07)))


#2.3
logit_predict <- predict(logit_fit, newdata=test_df, type="response")
mse_1 <- sum((logit_predict - test_df$Direction)^2)

logit_predict_2 <- predict(logit_fit_AIC$BestModel, newdata=test_df, type="response")
mse_2 <- sum((logit_predict_2-test_df$Direction)^2)

cbind(logit_predict, logit_predict_2)


#2.4

y_logit <- (logit_predict>0.5)*1
y_logit_aic <- (logit_predict_2>0.5)*1

c(sum(test_df$Direction == y_logit)/length(y_logit), sum(test_df$Direction == y_logit_aic)/length(y_logit_aic))

# The first model (the one using all the variables) has a 48% accuracy. Meaning that it performs worse than
# just random guessing. On the other hand, the second model with just the intercept, has a 55% accuracy
# Meaning that it performs slightly better than random guessing.

# Exercise 3

#3.1
lymphx <- read.table("lymphx.txt")
lymphstatus <- read.table("lymphstatus.txt")

X <- as.matrix(lymphx)
y <- as.matrix(lymphstatus)


logit_lymph <- glm(y ~ X, family="binomial")
# Since p > n, the algorithm does not converge. Therefore, we need to shrink our X variables.

#3.2
set.seed(100)


# Estimating the lasso regression
lasso <- cv.glmnet(X, y, family="binomial", alpha=1, type.measure="deviance", nfolds=10)

#Getting the best lambda
lasso_lambda <- lasso$lambda.min

# The estimated lambda is 0.069. Meaning that it is fairly close to OLS (if it could be run in this case)

#3.3
lasso_coef <- predict(lasso, type="coefficients", s=lasso_lambda)[2:7400]
rel_coef_index <- which(lasso_coef!=0)
# Total relevant coefficients exlcuding intercept

#Saving lasso coefficients
coef_1 <- lasso_coef[rel_coef_index]

# 3.4
# Post Lasso
X_post <- X[, rel_coef_index]
post_lasso <- glm(y ~ X_post, family="binomial")

coef_2 <- post_lasso$coefficients[-1]

cbind(coef_1, coef_2)


c(sum(sign(coef_1) != sign(coef_2)))

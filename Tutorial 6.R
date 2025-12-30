library(ISLR2)
library(pls)
library(boot)
library(tidymodels)
library(tidyverse)


# Exercise 1
df <- Boston

head(df)

#2

set.seed(1)

x <- df %>% select(medv)
y <- df %>% select(-medv)

train <- sample(1:nrow(df), 0.8*nrow(df)) 
test <- (-train) 

training_data <- df[train, ]
testing_data <- df[test, ]

nrow(training_data)
#404
nrow(testing_data)
#102

#3

OLS_fit <- lm(medv ~ ., training_data)
OLS_fit

OLS_pred <- predict(OLS_fit, newdata=testing_data)

mse_ols <- mean((testing_data$medv - OLS_pred)^2)

mse_ols

#4

x_train <- x[train, ] 
y_train <- y[train, ]
x_test <- x[test, ]
y_test <- y[test, ]

pcr_fit <- pcr(medv ~ ., data=training_data, scale=TRUE, validation="CV")
summary(pcr_fit)

#i
validationplot(pcr_fit, val.type="MSEP")

#ii
optimal_M <- which.min(pcr_fit$validation$PRESS)
optimal_M
# The optimal number based on cross-validation should be 12 components


#iii
pcr_pred <- predict(pcr_fit, testing_data, ncomp=optimal_M)
mse_pcr <- mean((testing_data$medv - pcr_pred)^2)
mse_pcr

# As we can see the mse from the pcr and ols are the same

#5
pls_fit <- plsr(medv ~ ., data=training_data, scale=TRUE, validation="CV")
summary(pls_fit)

# Without looking at the graph, it appears that based on CV
# the best model would have 7 components

#i
validationplot(pls_fit, val.type="MSEP")
#Based on the plot it is a bit difficult to tell which one is the best # of components
#especially since the MSEQ appears to be decreasing as the comp go up 

#ii
pls_M <- which.min(pls_fit$validation$PRESS)
pls_M


#iii
pls_pred <- predict(pls_fit, testing_data, ncomp=pls_M)
mse_pls <- mean((testing_data$medv - pls_pred)^2)
mse_pls

# 6

metrics <- data.frame(model=NULL, mse=NULL)
ols <- cbind(model="OLS", mse_ols)
pcr <- cbind(model="PCR", mse_pcr)
pls <- cbind(model="PLS", mse_pls)
rbind(ols, pcr, pls)


# The MSE of OLS and PCR are the same.
# This is because when running cross validation,
# it was found that the optimal number of components is 12 which is the same as OLS.
# On the other hand, PLS has a very slightly higher MSE than OLS and PCR
# meaning that OLS and PCR outperform PLS in this case.

# Exercise 2
# 1
df_2 <- USArrests
head(df_2)

df_2 %>% summarise(across(everything(), list(mean=mean, sd=sd)))

# After looking at the means and standard deviations
# it is clear that the variables are not on comparable scales.
# This makes sense, since crimes like murder are more severe than assault
# and therefore there are less instances of the commited crimes.

# The variables should be scaled when using PCA for multiple reasons.
# When calculating the variance of the components, we need to make sure
# that one variable with a different scaling, doesn't skew the variance and thus artificially
# making greater or lower than it should. 


# 2

pca_fit <- prcomp(df_2, scale=TRUE)
names(pca_fit)

summary(pca_fit)
# The summary output shows that 2 components
# explain 86% of the variance, while 3 components
# explain 95% of the variance. 

#3

pve <- pca_fit$sdev^2 / sum(pca_fit$sdev^2)
plot(pve, type = "o", xlab = "Principal Component", ylab="PVE", col="blue")

#4

#i
# By the elbow rule, the number of principal components to be chosen is 3

#ii
# On the other hand, when looking at the cumulative proportion
# we should chose the first 2 principal components since they explain 86% of the variation

# 5
# First component
pca_fit$rotation[,1]

# This component has all negative loadings
# meaning that the states with higher PCA1 scores
# have lower crime and lower populations on average

# Second component
pca_fit$rotation[,2]
# On the other hand, the second component has 
# murder and assault with negative values while 
# urban population and rape are lower.
# This indicates that states with high PCA2 scores
# have a higher population which a low but positive 
# number of rapes, but with lower murder and assault crimes.


#6
Z <- pca_fit$x

pc_df <- data.frame(PC1=Z[, 1], PC2=Z[, 2], State=rownames(df_2))

ggplot(pc_df, aes(x=PC1, y=PC2, label=rownames(Z))) + 
    geom_point(size=3) + 
    geom_text(vjust=-1) +
    labs(title = "PCA: PC1 vs PC2 for NCI60",x = "PC1", y = "PC2") +
    theme_minimal()


# California appears to be an extreme when looking at these two components.
# It has a low PC1 and a high PC2.
# The low PC1 means that the state has a high crime rate, while
# the high PC2 means that the state has higher rape rates and a higher population. 

# Solution from class:

# PC1 vs PC2 plot shows relative positions of states along the first two PCs.
# States with extreme PC1 values correspond to very high or very low overall violent crime.
# States with extreme PC2 values are outliers in urbanization versus murder rates.
# Although state labels may overlap, we can still observe clusters of states:
#   - Northeastern states tend to have lower violent crime (low PC1)
#   - Southern states tend to have higher violent crime (high PC1)
#   - Highly urbanized states have low PC2 scores, rural states have high PC2 scores
# Overall, the plot reveals general groupings and extremes rather than individual states.














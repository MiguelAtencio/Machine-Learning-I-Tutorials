
library(tidyverse)
library(tidymodels)
library(ISLR2)

# Problem 1

data(Boston)

glimpse(Boston)
# Part 1
set.seed(2)
data_split <- initial_split(Boston, prop=0.8)
training_data <- training(data_split)
testing_data <- testing(data_split)

# Part 2

lm_spec <- linear_reg(mode = "regression", engine="lm") 


# Model 1
model1_fit <- lm_spec %>% fit(medv ~ lstat, data=training_data)

# Model 2
recipe_2 <- recipe(medv ~ lstat, data=training_data) %>% 
    step_poly(lstat, degree=2)

model2_fit <- workflow() %>% 
    add_model(lm_spec) %>% 
    add_recipe(recipe_2)

model_2 <- model2_fit %>% fit(data=training_data)

# Model 3
recipe_3 <- recipe(medv ~ ., data=training_data) %>% 
    step_poly(lstat, degree=2)

model3_fit <- workflow() %>% 
    add_model(lm_spec) %>% 
    add_recipe(recipe_3)

model_3 <- model3_fit %>% fit(data=training_data)

# Part 3

info_m1 <- model1_fit %>% glance() %>% select(r.squared, adj.r.squared, AIC, BIC)
info_m2 <- model_2 %>%  glance() %>% select(r.squared, adj.r.squared, AIC, BIC)
info_m3 <- model_3 %>%  glance() %>% select(r.squared, adj.r.squared, AIC, BIC)

# Combine into one dataframe
model_comparison <- bind_rows(
    info_m1,
    info_m2,
    info_m3
) %>%
    mutate(model = c("m1", "m2", "m3"), .before = 1)


model_comparison

# The R squared with its adjusted version prefers the m3 model. The R squared is a measure that explains 
# how much the model can explain the variation in the data. Also, a common consequence of the calculation of the R squared is that 
# the more variables are added into the model, the higher the R square will be. 

# Once more, in terms of AIC and BIC model 3 is the best performing model probably since the other models are too limited. However, when 
# considering the difference in BIC between the models, the difference between model 1 and model 2 is the same as model 2 and model 3.


# Overall, the best model to choose would be model 3.


# Problem 2

# Part 1

# Data split is done


# Part 2

# Here I assume that the model will use all the variables of the dataset to precdict medv

knn_recipe <- recipe(medv ~ . , data=training_data) %>%  
    step_nzv(all_predictors()) %>% 
    step_center(all_predictors()) %>% 
    step_scale(all_predictors())

# Part 3
set.seed(2)
boston_folds <- vfold_cv(training_data, v = 10)


# Part 4
knn <- nearest_neighbor(mode="regression", engine="kknn", 
                        neighbors=tune(), dist_power = tune())

# The neighbors parameter equaling to tune() means that the number of neighbors
# that has the lowest training error will be chosen. 
# On the other hand, bu choosing tune() for dist_power, the function will find the 
# best p to calculate the minkoswki distance, where in the case that it is 1,
# the distance is equal to the manhattan distance and when is equal to 2, it is equal to the euclidean distance


# Part 5

knn_wf <- workflow() %>% 
    add_model(knn) %>% 
    add_recipe(knn_recipe)

# Part 6

knn_param_set <- parameters(neighbors(range = c(1, 20)),
                            dist_power(range = c(1, 5)))

knn_grid_regular <- knn_param_set %>% 
    grid_regular(levels=5)

knn_grid_regular

# Part 7

knn_tune <- knn_wf %>%
    tune_grid(
        resamples = boston_folds,
        grid = knn_grid_regular,
        metrics = metric_set(rmse)
    )


# Part 8
autoplot(knn_tune)

# Part 9
# Based on the rmse, the model to choose would have
# 5 neighbors using a manhattan distance. This can
# be easily seen in the plot, where the rmse from these parameters
# is the lowest out of all the combinations of the parameters we run.


# Problem 3

# Y truly depends on x1, x2, x3, x4, x5
# x6 through x15 are just noise
set.seed(42)
n <- 30
x <- matrix(rnorm(n * 15), n, 15)
y <- x[, 1:5] %*% rep(0.3, 5) + rnorm(n, 0, 1)

data <- as_tibble(x) %>%
    set_names(paste0("x", 1:15)) %>%
    mutate(y = as.vector(y))

# Store results in a dataframe
results <- tibble(
    num_predictors = 5:15,  # We'll test models with 5, 6, 7, ..., 15 predictors
    r_squared = NA,
    adj_r_squared = NA,
    AIC = NA,
    BIC = NA
)

# Example: fit a model
mod <- lm(y ~ x1 + x2 + x3, data = data)

# Extract criteria
summary(mod)$r.squared        # r_squared
summary(mod)$adj.r.squared    # Adjusted r_squared
AIC(mod)                      # AIC
BIC(mod)                      # BIC


for (i in 1:nrow(results)){
    p <- i + 4
    
    formula <- paste("y ~", paste0("x",1:p, collapse =  " + "))
    fm_object <- as.formula(formula)
    
    model <- lm(fm_object, data=data)
    
    results[i, 2] <- summary(model)$r.squared
    results[i, 3] <- summary(model)$adj.r.squared
    results[i, 4] <- AIC(model)
    results[i, 5] <- BIC(model)
}

results

# Graph 1

results %>% ggplot(aes(x=num_predictors)) + 
    geom_point(aes(y=r_squared, colour = "r squared")) + geom_line(aes(y=r_squared, colour = "r squared")) +
        geom_point(aes(y=adj_r_squared, colour = "adj r squared")) + geom_line(aes(y=adj_r_squared, colour = "adj r squared"))
    

# Graph 2
results %>%  ggplot(aes(x=num_predictors)) +
    geom_point(aes(y=AIC, colour="AIC")) + geom_line(aes(y=AIC, colour="AIC")) +
    geom_point(aes(y=BIC, colour="BIC")) + geom_line(aes(y=BIC, colour="BIC"))

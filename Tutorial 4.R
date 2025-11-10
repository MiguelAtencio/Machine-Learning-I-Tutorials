library(tidyverse)
library(tidymodels)
library(discrim) # for LDA/QDA models

set.seed(123)

# Number per class
n <- 1000

# Class means (separated to make the problem solvable)
mu_neg <- c(0, 0)
mu_pos <- c(2, 2)

# Identity covariance matrix = independent features, same scale
Sigma <- diag(2)

# Generate features
x_neg <- MASS::mvrnorm(n, mu = mu_neg, Sigma = Sigma)
x_pos <- MASS::mvrnorm(n, mu = mu_pos, Sigma = Sigma)

# Combine into a single data frame
data_balanced <- rbind(
    data.frame(x1 = x_neg[,1], x2 = x_neg[,2], y = "neg"),
    data.frame(x1 = x_pos[,1], x2 = x_pos[,2], y = "pos")
)

# Convert y to factor
data_balanced$y <- factor(data_balanced$y)


# Task 1
ggplot(data=data_balanced, aes(x=x1, y=x2, color=y)) + geom_jitter(alpha=0.5)

# In the plot, it can be seen that the two classes are different. While the negative class has more points 
# in the lower left side of the plot, the positive class has a greater x1 and and greater x2 on average.
# To try to do predictions on these classes we could use either logistic regression or discriminant analysis.
# Since in this case we know how the data follows a normal distribution where each feature has the same covariance matrix,
# we now that the linear discriminant analysis will work the best.


# Task 2
split <- initial_split(data_balanced, prop=0.8)
training_data <- training(split)
test_data <- testing(split)

# Task 3
lda_spec <- discrim_linear(mode="classification", engine="MASS")

lda_fit <- lda_spec %>% fit(y ~ ., data=training_data)

# Task 4
prediction_lda <- predict(lda_fit, new_data=test_data)


results_lda_balanced <- bind_cols(test_data, prediction_lda)

conf_mat(results_lda_balanced, truth = y, estimate = .pred_class)
acc_balanced <- accuracy(results_lda_balanced, truth = y, estimate = .pred_class)
sen_balanced <- sensitivity(results_lda_balanced, truth = y, estimate = .pred_class)
spe_balanced <- specificity(results_lda_balanced, truth = y, estimate = .pred_class)

# The accuracy is very high


#Imbalance datasets
set.seed(123)

# Downsample to 200 positive
data_pos200 <- bind_rows(
    data_balanced %>% filter(y == "pos") %>% slice_sample(n = 200),
    data_balanced %>% filter(y == "neg")
)

# Downsample to 50 positive
data_pos050 <- bind_rows(
    data_balanced %>% filter(y == "pos") %>% slice_sample(n = 50),
    data_balanced %>% filter(y == "neg")
)

# Downsample to 20 positive
data_pos020 <- bind_rows(
    data_balanced %>% filter(y == "pos") %>% slice_sample(n = 20),
    data_balanced %>% filter(y == "neg")
)

table(data_pos200$y)
table(data_pos050$y)
table(data_pos020$y)

# Task 5

ggplot(data=data_pos200, aes(x=x1, y=x2, color=y)) + geom_jitter(alpha=0.5)
ggplot(data=data_pos050, aes(x=x1, y=x2, color=y)) + geom_jitter(alpha=0.5)
ggplot(data=data_pos020, aes(x=x1, y=x2, color=y)) + geom_jitter(alpha=0.5)



summary_table <- data.frame(dataset_type=character(),
                            accuracy=numeric(),
                            sensitivity=numeric(),
                            specificity=numeric())

datasets <- list(data_pos020, data_pos050, data_pos200)
dataset_n <- list("data_pos020", "data_pos050", "data_pos200")

for (i in 1:length(datasets)){
    dataset <- datasets[[i]]
    split <- initial_split(data=dataset, prop=0.8) 
    training_data <- training(split)
    test_data <- testing(split)
    
    lda_spec <- discrim_linear(mode="classification", engine="MASS")
    
    lda_fit <- lda_spec %>% fit(y ~ ., data=training_data)
    
    prediction_lda <- predict(lda_fit, new_data=test_data)
    
    results_lda <- bind_cols(test_data, prediction_lda)
    
    conf_mat(results_lda, truth = y, estimate = .pred_class)
    acc <- accuracy(results_lda, truth = y, estimate = .pred_class)
    sen <- sensitivity(results_lda, truth=y, estimate=.pred_class)
    spe <- specificity(results_lda, truth=y, estimate=.pred_class)
    
    summary_table <- rbind(summary_table, data.frame(
        dataset_type = dataset_n[[i]],
        accuracy = acc$.estimate,
        sensitivity = sen$.estimate,
        specificity = spe$.estimate
    ))
}


summary_table <- rbind(summary_table, data.frame(
    dataset_type = "data_pos1000",
    accuracy = acc_balanced$.estimate,
    sensitivity = sen_balanced$.estimate,
    specificity = spe_balanced$.estimate
))

# Task 6

summary_long <- summary_table %>% 
    pivot_longer(cols = c(accuracy, sensitivity, specificity),
                 names_to = "metric",
                 values_to = "value")

ggplot(summary_long, aes(x = metric, y = value, fill = dataset_type)) +
    geom_col(position = "dodge")

# Task 7
# Accuracy and sensitivity. This is because when we have imbalanced datasets and we do not correct for this, 
# most of the values that the model predict will fall into True positives (even if they're not). Hence,
# inflating our accuracy and sensitivity measures. 


# Task 8
# The metric that becomes misleading is accuracy. This is because accuracy is supposed to measure the whole model,
# however, when we have imbalanced datasets, most of our predictions will be "correct" by construction. Or
# in other words, by the already existing distribution of the classes within a given dataset.

# Task 9
# I don't understand question 9



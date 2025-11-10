
library(tidymodels)
library(tidyverse)

# Exercise 1

data <- read_csv("Cars_data.csv")

glimpse(data)

head(data)


# Exercise 2

data <- data %>% mutate(Make = factor(Make), Year = factor(Year), `Engine Fuel Type` = factor(`Engine Fuel Type`),
                `Transmission Type` = factor(`Transmission Type`), Driven_Wheels = factor(Driven_Wheels),
                `Market Category` = factor(`Market Category`), `Vehicle Size` = factor(`Vehicle Size`),
                `Vehicle Style` = factor(`Vehicle Style`)) 
glimpse(data)

# Exercise 3

data$Model

data <- data %>% select(-Model)

data


# A possible reason why the model column is removed is that the relevant information of the car is 
# already given by the car specifications (e.g. year, engine, transmission, etc.) and the model does not add any additional information.

# Exercise 4

levels(data$Year) 

# Year should be treated as a numerical variable

set.seed(55)


# Spliting the data
split <- data %>% initial_split(prop=0.7)
training_data <- training(split)
test_data <- testing(split)

# Exercise 5

ggplot(training_data, aes(x = MSRP)) + geom_histogram() # Data is clearly right skewed

# Checking log
ggplot(training_data, aes(x = log(MSRP))) + geom_histogram()

# Checking Box-Cox
lambda <- forecast::BoxCox.lambda(training_data$MSRP)  # box.cox.lambda from the forecast package

boxcox <- if (abs(lambda) < 1e-8) { # we take the absolute to check if lambda is effectively = 0
    log(training_data$MSRP)
} else {
    (training_data$MSRP^lambda - 1) / lambda
}

training_data$BoxCox_MSRP <- boxcox

ggplot(data=NULL, aes(x = boxcox)) + geom_histogram()

# Checking if there are MSRP negative values (there shouldn't be)
training_data %>% filter(MSRP < 0)

# Both the log or boxcox transformation are very similar. So, I choose the box cox transformation

training_data <- training_data %>% select(-MSRP) # Dropping the original MSRP

# Exercise 7

library(skimr)

skim(data)

# There are very little missing values in total, this means that we can either impute the variables or delete the rows.
# I will delete the rows

training_data <- training_data %>% drop_na() 

# Exercise 8
# Checking zero and near zero variance

# Feature filtering
data_recipe <- recipe(BoxCox_MSRP ~ ., data=training_data)

data_recipe %>% 
    step_zv(all_predictors()) %>% 
    prep() 

data_recipe %>%
    step_nzv(all_predictors(), freq_cut = 100/1) %>%  
    prep()

# No variables will be deleted

# Exercise 9

library(DataExplorer)

plot_histogram(training_data)

# Checking city.mpg log
training_data %>% ggplot(aes(log(`city mpg`))) + geom_histogram()
    
# Checking city mpg box cox
lambda <- forecast::BoxCox.lambda(training_data$`city mpg`) 

boxcox <- if (abs(lambda) < 1e-8) { # we take the absolute to check if lambda is effectively = 0
    log(training_data$`city mpg`)
} else {
    (training_data$`city mpg`^lambda - 1) / lambda
}

ggplot(data=NULL, aes(x = boxcox)) + geom_histogram()

# Log citympg looks better than box cox

log_plot <- function(df, col){
    
    x <- dplyr::pull(df, {{ col }})
    
    ggplot(data=NULL, aes(x = log(x))) + geom_histogram()
}

boxcox_plot <- function(df, col){
    x <- dplyr::pull(df, {{ col }})
    
    lambda <- forecast::BoxCox.lambda(x) 
    
    boxcox <- if (abs(lambda) < 1e-8) { # we take the absolute to check if lambda is effectively = 0
        log(x)
    } else {
        (x^lambda - 1) / lambda
    }
    
    ggplot(data=NULL, aes(x = boxcox)) + geom_histogram()
}

log_plot(training_data, `Engine HP` )
boxcox_plot(training_data, `Engine HP` )
min_max <- (training_data$`Engine HP` - min(training_data$`Engine HP`)) / (max(training_data$`Engine HP`)-min(training_data$`Engine HP`))
ggplot(data=NULL, aes(x = min_max)) + geom_histogram()

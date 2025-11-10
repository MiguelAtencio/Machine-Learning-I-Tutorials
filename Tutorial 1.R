
library(tidyverse)

housing.df <- read.csv("Tutorials/Tutorial 1/WestRoxbury.csv")

dim(housing.df)
head(housing.df)
View(housing.df)

head(housing.df$TOTAL.VALUE, 10)
head(housing.df, 10)
# Rows then columns
housing.df[1:10, 1]
housing.df[5, ]



housing.df$TOTAL.VALUE

mean(housing.df$TOTAL.VALUE)

summary(housing.df)

str(housing.df)

housing.df %>% mutate(
    REMODEL = as.factor(REMODEL)
)

housing.df %>% write.csv("Tutorials/Tutorial 1/part2.csv")


# Part 3
v1 <- c(1, 2, 2, 1)
v2 <- c(2, 3, 3, 2)

v1 + v2
v1 - v2

v1 * v2

v3 <- combine(v1, v2)
v3


ma <- matrix(seq(1, 6),
             nrow=2,
             ncol=3)

# Min and max of each row

for (i in 1:nrow(ma)){
    print(max(ma[i, ]))
    print(min(ma[i, ]))
}

vec <- c()

for (i in 1:ncol(ma)){
    vec <- combine(vec, sum(ma[, i]))
}
vec

sort(ma[, 1], decreasing=TRUE)


unnamed_func <- function(x){
    if (x <= 0){
        return(-x^3)
    }
    else if (x > 0 & x < 1){
        return(x^2)
    }
    else{
        return(sqrt(x))
    }
} 

unnamed_func(-3)
unnamed_func(0.5)
unnamed_func(2)

power_of_n <- function(x, n){
    suma <- 0
    for (i in 0:n){
        suma <- suma + x^i
    }
    return(suma)
}

power_of_n(5, 4)


power_of_n_while <- function(x, n){
    
    suma <- 0
    i <- 0
    while (i <= n){
        suma <- suma + x^i
        i <- i + 1
    }
    return(suma)
}

power_of_n_while(5, 4)

ma1 <- matrix(1, 4, 4)

ma1 <- ma1 - diag(4)


ma2 <- matrix(1:16, 4, 4)

ma2

ma2[!(ma2 %in% c(2, 3, 6, 9, 16))] <- 0

ma2

switches <- rep(0, 100)
persons <- 1:100


toggle_switches <- function(swtiches, person){
    for (i in seq_along(switches)){
       for (person_i in seq_along(person)){
           if (i %% person_i == 0){
               if (switches[i] == 1){
                   switches[i] <- 0
               }
               else{
                   switches[i] <- 1
               }
           }
       } 
    }
    return(switches)
}

which(toggle_switches(swtiches, persons)==1)


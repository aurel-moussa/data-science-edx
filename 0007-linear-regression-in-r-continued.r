#create a fake dataset
library(tidyverse)
library(caret)
        
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later2, just to ensure reproducibility
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
      data.frame() %>% setNames(c("x", "y"))

#set the seed to 1 again (make sure to use sample.kind="Rounding" if your R is version 3.6 or later). 
#Then, within a replicate() loop, (1) partition the dataset into test and training sets with p = 0.5 
#and using dat$y to generate your indices, (2) train a linear model predicting y from x, 
#(3) generate predictions on the test set, and (4) calculate the RMSE of that model.
#Then, report the mean and standard deviation (SD) of the RMSEs from all 100 models.

output_square_deviation <- c() #empty vector

partition_and_model_function <- function(input_dataset) {
y <- input_dataset$y        
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- input_dataset %>% slice(-test_index) #slice the input_dataset set, and drop all the indexes as defined in test-index
test_set <- input_dataset %>% slice(test_index)  #slice the input_dataset set, and keep all the indexes as defined in test-index
fit <- lm(y ~ x, data = train_set) #lm fits linear models, including multivariate ones, first parameter being the symbolic description of the model to be fitted, here: describe y via x
#print(fit$coef)
y_hat <- predict(fit, test_set)
root_mean_square_error <- sqrt(mean((y_hat - test_set$y)^2))
#print(root_mean_square_deviation)
#output_square_deviation <- append(output_square_deviation, root_mean_square_deviation)        #does not work?
return(root_mean_square_error)
}

output_square_deviation <- replicate(100, partition_and_model_function(dat))
mean(output_square_deviation)
sd(output_square_deviation)

##########################################
#we will repeat the exercise above but using larger datasets. 
#Write a function that takes a size n, 
#then (1) builds a dataset using the code provided at the top 
#but with n observations instead of 100 and without the set.seed(1), 
#(2) runs the replicate() loop that I wrote above, which builds 100 linear models and returns a vector of RMSEs, 
#and (3) calculates the mean and standard deviation of the 100 RMSEs.

#Set the seed to 1 (if using R 3.6 or later, use the argument sample.kind="Rounding") 
#and then use sapply() or map() to apply your new function to n <- c(100, 500, 1000, 5000, 10000).
#Note: You only need to set the seed once before running your function; 
#do not set a seed within your function.
#Also be sure to use sapply() or map() as you will get different answers running the simulations individually due to setting the seed.

#Now let's work with multiple x's predicting the y
      
#Creating a normally distributed dataset
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
	data.frame() %>% setNames(c("y", "x_1", "x_2"))

#y is correlated with both x_1 and x_2 but the two predictors are independent of each other, as seen by cor(dat).
#Set the seed to 1, then use the caret package to partition into test and training sets with p = 0.5. 
#Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2. Train a single linear model for each.    

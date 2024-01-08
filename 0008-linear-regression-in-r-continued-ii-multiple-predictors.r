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

function(input_dataset) {test_index <- createDataPartition(input_dataset$y, times = 1, p = 0.5, list = FALSE) #important! It is actually the y that needs partioning!
train_set <- input_dataset %>% slice(-test_index) #slice the input_dataset set, and drop all the indexes as defined in test-index
test_set <- input_dataset %>% slice(test_index)  #slice the input_dataset set, and keep all the indexes as defined in test-index
fit <- lm(y ~ x, data = train_set) #lm fits linear models, including multivariate ones, first parameter being the symbolic description of the model to be fitted, here: describe y via x
print(fit$coef)
y_hat <- predict(fit, test_set)
root_mean_square_deviation <- mean((y_hat - test_set$y)^2)
print(root_mean_square_deviation)                                                 
}

#then, since we care about the ROOT mean square deviation
sqrt(rootm_mean_square_deviation)

#we can make the above more multi-purpose, by allowing the user to specificy which x variables to use for the fitting
#we managed to find that the both x's together make the least root mean square error

####
#Now, lets build a dataset where x1 and x2 are highly correlated, so not really indepedent variables

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
	data.frame() %>% setNames(c("y", "x_1", "x_2"))

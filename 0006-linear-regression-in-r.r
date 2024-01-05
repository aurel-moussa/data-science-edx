#Linear regression could be considered a ML algorithm
#Usually it is too rigid to be very accurate
#However it can serve as a baseline against which to compare more complex algorithms, being a bit better than just guessing the mean/average

library(tidyverse)
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>% #only interested in data on first-born sons
  select(father, childHeight) %>%
  rename(son = childHeight)

library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
#y what to partiion, times how many partitions to make (I can just make one, and if included make it the test set, and if NOT included make it the train set)
#p the percentage to be included in the partition
#list FALSE to make a matrix instead of a list, this is required so that later I can index using test_index[1], test_index[2] and so on

train_set <- galton_heights %>% slice(-test_index) #slice the galton_heights set, and drop all the indexes as defined in test-index
test_set <- galton_heights %>% slice(test_index)  #slice the galton_heights set, and keep all the indexes as defined in test-index

#the simplest model: the average
avg <- mean(train_set$son)
avg

#mean square error loss function
mean((avg - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set) #lm fits linear models, including multivariate ones, first parameter being the symbolic description of the model to be fitted, here: describe son via father
fit$coef

#predicting y based on the model
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father

#mean square error loss function
mean((y_hat - test_set$son)^2)

#instead of writing out the predictor function y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
#we can also use the predict() function, which can take lm models as inputs (and other models)
y_hat <- predict(fit, test_set)

#mean square error loss function
mean((y_hat - test_set$son)^2)

#predict() is a generic function in R that calls other functions depending on what kind of object it receives
#always look into the specifics, e.g., by running ?predict.lm or ?predict.knn

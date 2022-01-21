###Non-ML algorithms to predict a category###

#Get all required R packages
library(tidyverse)
library(caret)

#Get the source data
library(dslabs)
data(heights)

#define the dependent (outcome) and indepdent variables (predictors)
y <- heights$sex
x <- heights$height

# split up data into training and test sets using caret's createDataPartition
set.seed(2, sample.kind = "Rounding") # only use seed setting for testing purposes!
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) #generate indexes for randomly splitting data.
#https://topepo.github.io/caret/data-splitting.html
test_set <- heights[test_index, ] #put all rows in test_index in the test_set
train_set <- heights[-test_index, ] #put all rows apart from those in test_index in the train_set

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% factor(levels = levels(test_set$sex)) #create a random outcome

# compute accuracy of random guess
mean(y_hat == test_set$sex)

#find out more about the data, to use other guessing methods, such as cutoff points
heights %>% group_by(sex) %>% summarize(mean(height), sd(height)) #oh, so interesting, if height is more than 62, let's say it's a man
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))

#how good is this accuracy of cut off point?
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(50, 70, 0.5) #create different cut-off points, with 0.5 as the steps
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
}) #take different cutoff points and do the same ifelse function as before


#visualize the accuracy levels of different cutoffs
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

#which cut-off has the max value in accuracy list?
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#use this best cutoff on our test set to see how it plays out in real life
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

# tabulate each combination of prediction and actual value to see why the accuracy is misleading!
table(predicted = y_hat, actual = test_set$sex)

#how accurate are we for the different categories?
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")
prev #what is the prevalence of males in this dataset?

confusionMatrix(data = y_hat, reference = test_set$sex) #create confusion matris using caret

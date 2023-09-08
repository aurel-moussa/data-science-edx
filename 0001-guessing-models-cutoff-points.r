#install any missing packages
#install.packages(c("caret","tidyverse","dslabs"))

#using the caret package which has useful function for ML
library(caret)
library(tidyverse)

#predict gender using height
library(dslabs)
data(heights)

#define outcome and predictor
y <- heights$sex
x <- heights$height

#createDataPartition is a caret 
#function for splitting training and testing sets

set.seed(2) #only for ensuring reproducibility
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index,]
training_set <- heights[-test_index,]

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

#compute accuracy of guessing the outcome
mean(y_hat == test_set$sex)

#do some data exploration to have an "even" better model compared to guessing xD
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
y_hat <- ifelse(x>62,"Male","Female") %>% factor(levels = levels(test_set$sex))

#compute accuracy of cut-off point
mean(y == y_hat)

#create multiple cut-off points to understand accuracy
cutoff_points_100 <- seq(61,70,length.out = 100)
#iterate over a value of cutoffpoints with the function, which takes x
#which is the currently selected value fromcutoffpoints as input
#map_dbl used to apply the function to each element of cutoff
#function is anonymous here, that is why it is only called function
#dbl is double meaning we are expeting a double (numeric) vector as result
accuracies_of_various_cutoff_points <- map_dbl(cutoff_points_100, function(cutoffpoint)
{
  y_hat <- ifelse(training_set$height > cutoffpoint, "Male","Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == training_set$sex) #unless explicitiy stated with return(), the last line is funciton output
}
  )

#Let us have a look at the accuracies of various cut-offs
data.frame(cutoff_points_100, accuracies_of_various_cutoff_points) %>%
  ggplot(aes(cutoff_points_100, accuracies_of_various_cutoff_points)) +
  geom_point() +
  geom_line()
max(accuracies_of_various_cutoff_points)

#Use the best cutoff point for our newest model
best_cutoff <- cutoff_points_100[which.max(accuracies_of_various_cutoff_points)]

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

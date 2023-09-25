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

#create multiple cut-off points to understand accuracy
cutoff_points_100 <- seq(61,70,length.out = 100)

#iterate over a value of cutoffpoints with the function, which takes x
#which is the currently selected value fromcutoffpoints as input
#map_dbl used to apply the function to each element of cutoff
#function is anonymous here, that is why it is only called function
#dbl is double meaning we are expeting a double (numeric) vector as result
f1_scores_of_various_cutoff_points <- map_dbl(cutoff_points_100, function(input_cutoffpoint)
{
  y_hat <- ifelse(training_set$height > input_cutoffpoint, "Male","Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(training_set$sex)) #unless explicitiy stated with return(), the last line is funciton output
  #f_meas uses a beta of 1 if you do not change it explicitly, meaning that the score takes accuracy of specificity and sensitivy as equally important
  #in certain cases you need to make one or the other more important, e.g., safety of a critical plane part 
  #-- here it is better to have the algorithms falsely state that a part is defected, rather than to state a plane part is OK when actually it is defect
}
)

#Let us have a look at the accuracies of various cut-offs
data.frame(cutoff_points_100, f1_scores_of_various_cutoff_points) %>%
  ggplot(aes(cutoff_points_100, f1_scores_of_various_cutoff_points)) +
  geom_point() +
  geom_line()
max(f1_scores_of_various_cutoff_points)

#Use the best cutoff point for our newest model
best_cutoff <- cutoff_points_100[which.max(f1_scores_of_various_cutoff_points)]

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

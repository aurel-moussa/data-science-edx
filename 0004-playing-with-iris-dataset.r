#practicing ml using iris dataset
library(caret)
data("iris")
#getting rid of the setosa species records in this dataset
iris_dataset <- iris[-which(iris$Species=='setosa'),]

#our outcome variable
y <- iris_dataset$Species

#for reproducibility
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test_set <- iris_dataset[test_index,]
train_set <- iris_dataset[-test_index,]

cutoffpoints <- seq(from=0,to=10, by = 0.1)

accuracies_of_various_points_sepal_length <- 
  map_dbl(cutoffpoints, function(input_cutoffpoint)
  {
    y_hat <- ifelse(train_set$Sepal.Length > input_cutoffpoint, "virginica","versicolor")%>%
      factor(levels=levels(train_set$Species))
    mean(y_hat == train_set$Species)
  }
    )

accuracies_of_various_points_sepal_width <- 
  map_dbl(cutoffpoints, function(input_cutoffpoint)
  {
    y_hat <- ifelse(train_set$Sepal.Width > input_cutoffpoint, "virginica","versicolor")%>%
      factor(levels=levels(train_set$Species))
    mean(y_hat == train_set$Species)
  }
  )

accuracies_of_various_points_petal_width <- 
  map_dbl(cutoffpoints, function(input_cutoffpoint)
  {
    y_hat <- ifelse(train_set$Petal.Width > input_cutoffpoint, "virginica","versicolor")%>%
      factor(levels=levels(train_set$Species))
    mean(y_hat == train_set$Species)
  }
  )

accuracies_of_various_points_petal_length <- 
  map_dbl(cutoffpoints, function(input_cutoffpoint)
  {
    y_hat <- ifelse(train_set$Petal.Length > input_cutoffpoint, "virginica","versicolor")%>%
      factor(levels=levels(train_set$Species))
    mean(y_hat == train_set$Species)
  }
  )


dataframe_with_all_accuracies <- data.frame(cutoffpoints, accuracies_of_various_points_petal_length, 
                                            accuracies_of_various_points_petal_width, accuracies_of_various_points_sepal_length,
                                            accuracies_of_various_points_sepal_width)

ggplot(data = dataframe_with_all_accuracies) +
  geom_line(aes(x = cutoffpoints, y = accuracies_of_various_points_petal_length), color = "blue") +
  geom_line(aes(x = cutoffpoints, y = accuracies_of_various_points_petal_width), color = "red") +
  geom_line(aes(x = cutoffpoints, y = accuracies_of_various_points_sepal_length), color = "yellow") +
  geom_line(aes(x = cutoffpoints, y = accuracies_of_various_points_sepal_width), color = "green")

#We will take our best guess as the starting point, to see how this matches with our testing data
y_hat <- ifelse(test_set$Petal.Length > 4.7, "virginica","versicolor") %>% factor(levels = levels(test_set$Species))
confusion_matrix_using_starting_point <- confusionMatrix(table(y_hat, test_set$Species))

#now, lets do the same we did for our training data with our testing data instead, to see whether we were
#stupid in focusing on that specific single feature to make predictions

accuracies_of_various_points_sepal_length <- 
  map_dbl(cutoffpoints, function(input_cutoffpoint)
  {
    y_hat <- ifelse(test_set$Sepal.Length > input_cutoffpoint, "virginica","versicolor")%>%
      factor(levels=levels(test_set$Species))
    mean(y_hat == test_set$Species)
  }
  )

accuracies_of_various_points_sepal_width <- 
  map_dbl(cutoffpoints, function(input_cutoffpoint)
  {
    y_hat <- ifelse(test_set$Sepal.Width > input_cutoffpoint, "virginica","versicolor")%>%
      factor(levels=levels(test_set$Species))
    mean(y_hat == test_set$Species)
  }
  )

accuracies_of_various_points_petal_width <- 
  map_dbl(cutoffpoints, function(input_cutoffpoint)
  {
    y_hat <- ifelse(test_set$Petal.Width > input_cutoffpoint, "virginica","versicolor")%>%
      factor(levels=levels(test_set$Species))
    mean(y_hat == test_set$Species)
  }
  )

accuracies_of_various_points_petal_length <- 
  map_dbl(cutoffpoints, function(input_cutoffpoint)
  {
    y_hat <- ifelse(test_set$Petal.Length > input_cutoffpoint, "virginica","versicolor")%>%
      factor(levels=levels(test_set$Species))
    mean(y_hat == test_set$Species)
  }
  )


dataframe_with_all_accuracies <- data.frame(cutoffpoints, accuracies_of_various_points_petal_length, 
                                            accuracies_of_various_points_petal_width, accuracies_of_various_points_sepal_length,
                                            accuracies_of_various_points_sepal_width)

ggplot(data = dataframe_with_all_accuracies) +
  geom_line(aes(x = cutoffpoints, y = accuracies_of_various_points_petal_length), color = "blue") +
  geom_line(aes(x = cutoffpoints, y = accuracies_of_various_points_petal_width), color = "red") +
  geom_line(aes(x = cutoffpoints, y = accuracies_of_various_points_sepal_length), color = "yellow") +
  geom_line(aes(x = cutoffpoints, y = accuracies_of_various_points_sepal_width), color = "green")

#we can now see that the PETAL WIDTH instead is the best performing feature, if using cutoff points
#let us do some exploration plot
plot(iris_dataset,pch=21,bg=iris_dataset$Species) #pch is just how the datapoints are represented, ie with filled-in-cirlces

#optimizing the cutoffs for Petal-Length and Petal Width seperately in train dataset
cutoffpoints <- seq(from=0,to=10, by = 0.1)

accuracies_of_various_points_sepal_length <- 
  map_dbl(cutoffpoints, function(input_cutoffpoint)
  {
    y_hat <- ifelse(train_set$Sepal.Length > input_cutoffpoint, "virginica","versicolor")%>%
      factor(levels=levels(train_set$Species))
    mean(y_hat == train_set$Species)
  }
  )

accuracies_of_various_points_sepal_width <- 
  map_dbl(cutoffpoints, function(input_cutoffpoint)
  {
    y_hat <- ifelse(train_set$Sepal.Width > input_cutoffpoint, "virginica","versicolor")%>%
      factor(levels=levels(train_set$Species))
    mean(y_hat == train_set$Species)
  }
  )

accuracies_of_various_points_petal_width <- 
  map_dbl(cutoffpoints, function(input_cutoffpoint)
  {
    y_hat <- ifelse(train_set$Petal.Width > input_cutoffpoint, "virginica","versicolor")%>%
      factor(levels=levels(train_set$Species))
    mean(y_hat == train_set$Species)
  }
  )

accuracies_of_various_points_petal_length <- 
  map_dbl(cutoffpoints, function(input_cutoffpoint)
  {
    y_hat <- ifelse(train_set$Petal.Length > input_cutoffpoint, "virginica","versicolor")%>%
      factor(levels=levels(train_set$Species))
    mean(y_hat == train_set$Species)
  }
  )


dataframe_with_all_accuracies <- data.frame(cutoffpoints, accuracies_of_various_points_petal_length, 
                                            accuracies_of_various_points_petal_width, accuracies_of_various_points_sepal_length,
                                            accuracies_of_various_points_sepal_width)

ggplot(data = dataframe_with_all_accuracies) +
  geom_line(aes(x = cutoffpoints, y = accuracies_of_various_points_petal_length), color = "blue") +
  geom_line(aes(x = cutoffpoints, y = accuracies_of_various_points_petal_width), color = "red") +
  geom_line(aes(x = cutoffpoints, y = accuracies_of_various_points_sepal_length), color = "yellow") +
  geom_line(aes(x = cutoffpoints, y = accuracies_of_various_points_sepal_width), color = "green")

#best cutoff for Petal Length= 4.7
#best cutoff for Petal Width=1.5

y_hat_two_things <- ifelse(train_set$Petal.Length > 4.7 | train_set$Petal.Width > 1.5, "virginica","versicolor")%>%
  factor(levels=levels(train_set$Species))

confusion_matrix_using_both_points <- confusionMatrix(table(y_hat_two_things, train_set$Species))

#now applything this model to the test data
y_hat_two_things <- ifelse(test_set$Petal.Length > 4.7 | test_set$Petal.Width > 1.5, "virginica","versicolor")%>%
  factor(levels=levels(test_set$Species))

confusion_matrix_using_both_points <- confusionMatrix(table(y_hat_two_things, test_set$Species))

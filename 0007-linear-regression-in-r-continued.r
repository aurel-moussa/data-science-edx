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

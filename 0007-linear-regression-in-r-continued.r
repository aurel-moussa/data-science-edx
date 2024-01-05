#create a fake dataset
library(tidyverse)
library(caret)
        
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later2, just to ensure reproducibility
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
      data.frame() %>% setNames(c("x", "y"))

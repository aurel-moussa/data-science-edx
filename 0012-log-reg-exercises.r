#Define dataset
# set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
				mu_0 = 0, mu_1 = 2, 
				sigma_0 = 1,  sigma_1 = 1){

y <- rbinom(n, 1, p) #This generates a binary response variable y with n observations. Each observation is sampled from a binomial distribution with probability of success (class being 1) = to p
f_0 <- rnorm(n, mu_0, sigma_0)  #This generates n random values from a normal distribution with mean mu_0 and standard deviation sigma_0. These values represent the predictor variable x for class 0.
f_1 <- rnorm(n, mu_1, sigma_1)
x <- ifelse(y == 1, f_1, f_0) #outcome variable. Go through the vector y. If it is one, put the corresponding value of f_1 into x (predictor value); else put f_0 into it.
  
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
	test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

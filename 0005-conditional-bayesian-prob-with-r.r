#hypothetical population of 1 million individuals with the following conditional probabilities
#a disease test is positive 85% of the time when tested on a patient with the disease (high sensitivity)
#test is negative 90% of the time when tested on a healthy patient (high specificity)
#disease is prevalent in 2% of the population

#Bayes theorem
#P(A given B) = P(B given A)*(P of A) / (P of B)
#If B is a binary value (like a test result being positive or NOT), we have a special case that P(B) is equal to P(B given NOT A) * P(not A) + P(B given A) * P(A)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#what is prob that test is positive?
mean(test)

#probability that an individual has the disease if the test is negative?
#P(disease | negative) = P(negative | disease) * P(disease) / P(negative)
#Since this is a binary variable P(negative) = P(negative | NOT disease) * P(NOT disease) + P(negative | disease) * P(disease)
#or just using the dataset 1-mean(test)
0.15*mean(disease)/(1-mean(test))

# probability that you have the disease if the test is positive
#P(disease | positive) = P(positive | disease) * P(disease) / P(positive)
0.85*mean(disease)/mean(test)

#prevalence of disease in people who test positive to the overall prevalence of disease.
#If a patient's test is positive, by how many times does that increase their risk of having the disease?
#calculate the probability of having the disease given a positive test, then divide by the probability of having the disease

(0.85*mean(disease)/mean(test)) / 0.02


##compute conditional probabilities for being male in the heights dataset (part of the dslabs package). 
#Round the heights to the closest inch. Plot the estimated conditional probability

library(dslabs)
data("heights")
heights %>% 
	mutate(height = round(height)) %>%
	group_by(height) %>%
	summarize(p = mean(sex == "Male")) %>%    
	qplot(height, p, data =.)

#there'll be lots of variability for heights where we do not have lots of data points
#one way to deal with this is to instead group not by heights, but by quartiles
#we can use cut() function to make each group have the same number of data points
#cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE) cuts a dataset x into specific groups
#in this case, cutting x via quantiles

ps <- seq(0, 1, 0.1)
heights %>% 
	mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
	group_by(g) %>%
	summarize(p = mean(sex == "Male"), height = mean(height)) %>%
	qplot(height, p, data =.)

#one can create a random normal distribution (bi-variate) using these pacakges from MASS
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
	data.frame() %>% setNames(c("x", "y"))

#let's estimate the expectation of this random normal distribution
ps <- seq(0, 1, 0.1)
dat %>% 
	mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
	group_by(g) %>%
	summarize(y = mean(y), x = mean(x)) %>%
	qplot(x, y, data =.)

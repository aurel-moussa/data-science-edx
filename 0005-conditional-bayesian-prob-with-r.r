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

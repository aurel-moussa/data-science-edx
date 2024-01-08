#The problem with a linear regression is that the values of probability can be higher than 1, or negative, simply because it is a line
#this does not make sense for probabiltiy, because probability can only be between 0 and 1
#Logistic regression is an extension of linear regression that assures that the estimate of conditional probability is between 0 and 1. 
#This approach makes use of the logistic transformation: 

#g(probability) = log (p/1-p)

#idea of generalized linear models (GLM) is to 1) define a distribution of Y that is consistent with it’s possible outcomes 
#and 2) find a function g so that g(Pr(Y=1∣X=x)) can be modeled as a linear combination of predictors. 
#Logistic regression is the most commonly used GLM. 
#It is an extension of linear regression that assures that the estimate of Pr(Y=1∣X=x) is between 0 and 1. 
#This approach makes use of the logistic transformation

#logistic transformation converts probability to log odds. 
#the odds tell us how much more likely it is something will happen compared to not happening. 
#p=0.5 means the odds are 1 to 1, thus the odds are 1.
#If p=0.75, the odds are 3 to 1. 
#A nice characteristic of this transformation is that it converts probabilities to be symmetric around 0.

#we can no longer use least squares for measuring the error. Instead we compute the maximum likelihood estimate (MLE)

# fit logistic regression model
glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial") #we use function glm() (generalized linear models) and have to say family "binomal" to get a logistic reg model

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")  #we have to define response in order to get conditional probabilities instead of the logistic transfomred values

tmp <- heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) 
logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
tmp %>% 
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)

y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]

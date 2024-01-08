#let's apply regression approach to categorical data
#conditional probaility p(x) = Pr(Y=1 given X=x) = beta0 (slope intercept=) + beta1*x
#we give numerical values to our to outcomes e.g., Y=1 is female and Y=0 is male
#this will give us a probability; we can then create a decision rule, e.g., to say if x>0.5 probability, then we predict Y=1

library(dslabs)
data("heights")
y <- heights$height

set.seed(2, sample.kind = "Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>% 
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female")) #this is an example, filtering out only where height is 66 inches (rounded), let's find the y_hat that Y=1 (female)

heights %>% 
  mutate(x = round(height)) %>% #first we round all heights
  group_by(x) %>% #then we group them into bins
  filter(n() >= 10) %>% #then we only look at bins that have at least n data points
  summarize(prop = mean(sex == "Female")) %>% #then we summarize and put to a plot
  ggplot(aes(x, prop)) +
  geom_point()
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]

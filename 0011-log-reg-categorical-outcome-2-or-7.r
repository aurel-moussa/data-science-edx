##DATA DOWNLOAD AND WRANGLING

mnist <- read_mnist() #download the MNIST training and test data sets
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))] #gets the min value and max value of the mnist training set
titles <- c("smallest","largest") #c stands for COMBINE :D because for R, a number is just a 1x1 vector :)
tmp <- lapply(1:2, function(i){ #lapply stands for "list apply" in R. It is a function used to apply a given function to each element of a list (or vector) and return the results as a list.
    expand.grid(Row=1:28, Column=1:28) %>%   #the expand.grid function generates a data frame from all combinations of the supplied vectors or factors. In this case, it generates a data frame with all combinations of Row values from 1 to 28 and Column values from 1 to 28. This creates a data frame with 28*28 = 784 rows and two columns named Row and Column.
        mutate(label=titles[i], #then pipe the results into a mutation of the dataframe
               value = mnist$train$images[is[i],]) #adds two new variables to each row of the data frame:
   #label: Takes the value from the titles vector corresponding to index i.
    #value: Takes the image data from mnist$train$images corresponding to the index is[i]. 
    #The [is[i],] syntax subsets the mnist$train$images data frame using the index is[i], which was determined in the previous step.
})
tmp <- Reduce(rbind, tmp) #rbind is a function in R used to combine data frames by rows.
#When used with Reduce, it repeatedly applies rbind to combine the elements of the list row-wise

##DATA VISUALIZATION

tmp %>% ggplot(aes(Row, Column, fill=value)) +
    geom_raster() +
    scale_y_reverse() +
    scale_fill_gradient(low="white", high="black") +
    facet_grid(.~label) +
    geom_vline(xintercept = 14.5) +
    geom_hline(yintercept = 14.5)

data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
    expand.grid(Row=1:28, Column=1:28) %>%
        mutate(label=titles[i],
               value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
    geom_raster() +
    scale_y_reverse() +
    scale_fill_gradient(low="white", high="black") +
    facet_grid(.~label) +
    geom_vline(xintercept = 14.5) +
    geom_hline(yintercept = 14.5)

#MODELING

fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
#glm(): This function is used to fit generalized linear models in R.
#y ~ x_1 + x_2: This formula specifies the model formula. It indicates that the response variable y is modeled as a function of the predictor variables x_1 and x_2.
#data = mnist_27$train: This specifies the dataset to use for model fitting. In this case, it uses the training subset (train) of the MNIST_27 dataset (mnist_27).
#family = "binomial": This specifies the family of the distribution and link function to be used in the GLM. In this case, it indicates a binomial distribution, suitable for binary response variables.

p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]

mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
    geom_raster()

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster() +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5), color="black") 


p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
    mutate(p_hat = p_hat) %>%
    ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
    geom_raster() +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
    mutate(p_hat = p_hat) %>%
    ggplot() +
    stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
    geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)

mnist <- read_mnist() #download the MNIST training and test data sets
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]

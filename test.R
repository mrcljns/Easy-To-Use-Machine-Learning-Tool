library("Rcpp")
library("mlbench")
library("caret")

options(scipen=999)

sourceCpp('ml_model.cpp')

data <- read.csv("/home/maciej/Uni/Studia II stopień/I rok/Machine Learning I/titanic.csv")

data <- sapply(data[, c(1,2,5,6,7,8)], as.numeric)

data.index <- createDataPartition(data[,1], p = .7, list = FALSE)

X <- as.matrix(data[data.index, -c(1)])
y <- as.numeric(data[data.index, 1])

X_test <- as.matrix(data[-data.index, -c(1)])
y_test <- as.numeric(data[-data.index, 1])

num_outputs <- 2

net <- new(NeuralNet)

net$init_network(length(X[1,]), 4, num_outputs, "tanh", "classification")

net$train(X, y, 0.009, 1000, num_outputs)

net$predict(as.numeric(X[67,]))

outputs <- c(0)

for (i in 1:length(y_test))
  outputs[i] <- net$predict(X_test[i,])

paste(net$predict(X_test[55,]), y_test[55])

confusionMatrix(as.factor(outputs), as.factor(y_test))

net$forward_prop(X_test[35,])

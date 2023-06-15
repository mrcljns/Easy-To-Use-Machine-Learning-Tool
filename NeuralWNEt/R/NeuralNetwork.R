#' @title NeuralNetwork object
#'
#' @description
#' This R6 class represents a neural network model and provides methods for forward and backward propagation, as well as training and making predictions.
#' @importFrom R6 R6Class
#' @import Rcpp
#' @export

# Code inspired by https://machinelearningmastery.com/application-of-differentiations-in-neural-networks/

NeuralNetwork = R6Class("NeuralNetwork",
                         private = list(
                           # A list of output vectors for each layer.
                           z = NULL,
                           # A list of derivatives of output vectors for each layer.
                           dz = NULL,
                           # A list of weight matrices for each layer.
                           .W = NULL,
                           # A list of derivatives of weight matrices for each layer.
                           dW = NULL,
                           # A list of bias vectors for each layer.
                           .b = NULL,
                           # A list of derivatives of bias vectors for each layer.
                           db = NULL,
                           # A list of activation function output vectors for each layer.
                           a = NULL,
                           # A list of derivatives of activation function output vectors for each layer.
                           da = NULL,
                           # An integer specifying the number of hidden layers.
                           .layers_hidden = NULL,
                           # An integer specifying the number of neurons in each hidden layer.
                           .neurons_hidden = NULL,
                           # A character specifying the problem type ("classification" or "regression").
                           .problem_type = NULL,
                           # Loss function specific for each problem type.
                           loss_func = NULL,
                           # Derivative of the loss function.
                           dloss_func = NULL,
                           # A character specifying the type of activation function ("sigmoid", "tanh", or "relu").
                           .activ_type = NULL,
                           # The activation function.
                           activ_func = NULL,
                           # The derivative of the activation function.
                           dactiv_func = NULL,
                           # The output function.
                           out_func = NULL,
                           # The derivative of the output function.
                           dout_func = NULL,
                           # An integer specifying the random state.
                           rand_state = NULL,

                           # description
                           # Forward propagate through the network.
                           # x - A matrix of data.
                           forward = function(x){

                             private$a[[1]] = as.matrix(x)


                             for (l in 1:private$.layers_hidden){
                               private$z[[l]] = (private$a[[l]] %*% private$.W[[l]] + rep(private$.b[[l]], each=nrow(private$a[[l]])))
                               private$a[[l+1]] = apply(private$z[[l]], 2, private$activ_func)
                             }

                             private$z[[private$.layers_hidden + 1]] = (private$a[[private$.layers_hidden + 1]] %*% private$.W[[private$.layers_hidden + 1]] +
                                                                          rep(private$.b[[private$.layers_hidden + 1]], each = nrow(private$a[[private$.layers_hidden + 1]])))

                             private$a[[private$.layers_hidden + 2]] = private$out_func(private$z[[private$.layers_hidden + 1]])
                             return(private$a[[private$.layers_hidden + 2]])
                           },

                           # description
                           # Backward propagate the error through the network.
                           # y - A vector of target values.
                           # yhat - A vector of predictions.
                           backward = function(y, yhat){

                             private$da[[private$.layers_hidden + 2]] = private$dloss_func(y, yhat)
                             private$dz[[private$.layers_hidden + 1]] = private$da[[private$.layers_hidden + 2]] * private$dout_func(private$z[[private$.layers_hidden + 1]])
                             private$dW[[private$.layers_hidden + 1]] = t(private$a[[private$.layers_hidden + 1]]) %*% private$dz[[private$.layers_hidden + 1]]
                             private$db[[private$.layers_hidden + 1]] = apply(private$dz[[private$.layers_hidden + 1]], 2, mean)
                             private$da[[private$.layers_hidden + 1]] = private$dz[[private$.layers_hidden + 1]] %*% t(private$.W[[private$.layers_hidden + 1]])

                             for (l in (private$.layers_hidden):1){
                               private$dz[[l]] = private$da[[l+1]] * private$dactiv_func(private$z[[l]])
                               private$dW[[l]] = t(private$a[[l]]) %*% private$dz[[l]]
                               private$db[[l]] = apply(private$dz[[l]], 2, mean)
                               private$da[[l]] = private$dz[[l]] %*% t(private$.W[[l]])
                             }
                           },

                           # description
                           # Update the weights after backward propagation.
                           # lr - A numeric value of the learning rate.
                           update = function(lr){
                             # Updating the weights for each weight matrix
                             for (l in 1:length(private$.W)){
                               private$.W[[l]] = private$.W[[l]] - lr * private$dW[[l]]
                               private$.b[[l]] = private$.b[[l]] - lr * private$db[[l]]
                             }
                           },

                           # description
                           # This function creates mini-batches from the input data and labels for training purposes.
                           # X - A numeric matrix of the input data.
                           # y - A numeric vector of the target variable.
                           # batch_size - An integer specifying the size of each mini-batch.
                           # returns a list of mini-batches, where each mini-batch contains of a matrix of input data and a vector of target variable.
                           create_mini_batches = function(X, y, batch_size){
                             cppFunction(
                               "List create_mini_batches_cpp(NumericMatrix X, NumericVector y, int batch_size) {
                                  int n = X.nrow();
                                  int features = X.ncol();

                                  IntegerVector row_indices = seq(0, n - 1);
                                  std::random_shuffle(row_indices.begin(), row_indices.end());

                                  NumericMatrix data(n, features + 1);
                                  for (int i = 0; i < n; i++) {
                                    for (int j = 0; j < features; j++) {
                                      data(i, j) = X(row_indices(i), j);
                                    }
                                    data(i, features) = y(row_indices(i));
                                  }

                                  int n_minibatches = n / batch_size;
                                  List mini_batches(n_minibatches + 1);

                                  for (int i = 0; i <= n_minibatches - 1; i++) {
                                    int start = i * batch_size;
                                    int end = std::min((i + 1) * batch_size, n);
                                    NumericMatrix mini_batch = data(Range(start, end - 1), _);
                                    NumericMatrix X_mini = mini_batch(_, Range(0, features - 1));
                                    NumericVector Y_mini = mini_batch(_, features);
                                    mini_batches[i] = List::create(X_mini, Y_mini);
                                  }

                                  if (n % batch_size != 0) {
                                    int start = n_minibatches * batch_size;
                                    int end = n;
                                    NumericMatrix mini_batch = data(Range(start, end - 1), _);
                                    NumericMatrix X_mini = mini_batch(_, Range(0, features - 1));
                                    NumericVector Y_mini = mini_batch(_, features);
                                    mini_batches[n_minibatches] = List::create(X_mini, Y_mini);
                                  }

                                  return mini_batches;
                                }"
                             )
                             create_mini_batches_cpp(X, y, batch_size)
                           }
                         ),

                         # Some field are kept active
                         # The user can view them, but not alter them
                         active = list(
                           #' @field W A list of weight matrices for each layer.
                           W = function() private$.W,

                           #' @field b A list of bias vectors for each layer.
                           b = function() private$.b,

                           #' @field problem_type A character specifying the problem type ("classification" or "regression").
                           problem_type = function() private$.problem_type,

                           #' @field activ_type A character specifying the type of activation function ("sigmoid", "tanh", or "relu").
                           activ_type = function() private$.activ_type,

                           #' @field layers_hidden An integer specifying the number of hidden layers.
                           layers_hidden = function() private$.layers_hidden,

                           #' @field neurons_hidden An integer specifying the number of neurons in each hidden layer.
                           neurons_hidden = function() private$.neurons_hidden

                         ),

                         public = list(

                           #' @description
                           #' Customize the neural network.
                           #' @param rand_state An integer specifying the random state.
                           #' @param layers_hidden An integer specifying the number of hidden layers.
                           #' @param neurons_hidden An integer specifying the number of neurons in each hidden layer.
                           #' @param problem_type A character specifying the problem type ("classification" or "regression").
                           #' @param activ_type A character specifying the type of activation function ("sigmoid", "tanh", or "relu").
                           #' @examples
                                                      #' net <- NeuralNetwork$new(rand_state=42, layers_hidden=2, neurons_hidden = 4, problem_type = "classification", activ_type = "sigmoid")
                           initialize = function(rand_state = 42,
                                                 layers_hidden = 2,
                                                 neurons_hidden = 4,
                                                 problem_type = "classification",
                                                 activ_type = "sigmoid"){

                             # Defensive programming section
                             # We prepare for unexpected inputs
                             stopifnot(layers_hidden>0 & layers_hidden<4)
                             stopifnot(layers_hidden>0 & layers_hidden<21)
                             stopifnot(is.character(problem_type), problem_type %in% c("classification", "regression"))
                             stopifnot(is.character(activ_type), activ_type %in% c("sigmoid", "tanh", "relu"))

                             # The random state specified by the user
                             private$rand_state = as.integer(rand_state)

                             # Number of hidden layers
                             private$.layers_hidden = as.integer(layers_hidden)

                             # Number of neurons in hidden layers
                             # (for simplicity, the number of neurons is the same in each hidden layer)
                             private$.neurons_hidden = as.integer(neurons_hidden)

                             # The problem type - either classification or regression
                             private$.problem_type = problem_type

                             private$.activ_type = activ_type

                             # Initializing the activation function in hidden layers
                             if (activ_type == "sigmoid"){
                               private$activ_func = function(z) 1/(1+exp(-pmin(pmax(z, -500), 500)))

                               private$dactiv_func = function(z) {
                                 s = private$activ_func(z)
                                 2 * s * (1 - s)
                               }
                             }
                             else if (activ_type == "tanh"){
                               private$activ_func = function(z) tanh(z)
                               private$dactiv_func = function(z) 1 - tanh(z)**2
                             }
                             # in other case it's ReLU
                             else {
                               private$activ_func = function(z) pmax(z, 0)
                               private$dactiv_func = function(z) as.numeric(z>0)
                             }

                             # Initializing output and loss function based on problem type
                             if (problem_type == "classification"){

                               # Sigmoid as output
                               private$out_func = function(z) 1/(1+exp(-pmin(pmax(z, -500), 500)))
                               private$dout_func = function(z){
                                 s = private$out_func(z)
                                 2 * s * (1-s)
                               }

                               # Binary cross entropy
                               private$loss_func = function(y, yhat){
                                 -(t(y) %*% log(pmax(yhat, .Machine$double.eps)) + (1-t(y)) %*% log(pmax((1-yhat), .Machine$double.eps))) / length(y)
                               }

                               private$dloss_func = function(y, yhat){
                                 -y/pmax(yhat, .Machine$double.eps) + (1-y)/pmax((1-yhat), .Machine$double.eps)
                               }
                             }
                             else {
                               private$out_func = function(z) z
                               private$dout_func = function(z) 1

                               # MSE
                               private$loss_func = function(y, yhat){
                                 (yhat-y)**2 / length(y)
                               }

                               private$dloss_func = function(y, yhat){
                                 2*(yhat-y)
                               }
                             }

                             # Initialize empty lists for storing
                             # Adding 1 to the number of hidden layers to account for the output layer
                             private$z = vector(mode='list', length=private$.layers_hidden + 1)
                             private$.W = vector(mode='list', length=private$.layers_hidden + 1)
                             private$.b = vector(mode='list', length=private$.layers_hidden + 1)
                             private$a = vector(mode='list', length=private$.layers_hidden + 2)
                             private$dz = vector(mode='list', length=private$.layers_hidden + 1)
                             private$dW = vector(mode='list', length=private$.layers_hidden + 1)
                             private$db = vector(mode='list', length=private$.layers_hidden + 1)
                             private$da = vector(mode='list', length=private$.layers_hidden + 2)
                           },

                           #' @description
                           #' Initialize the weights and biases of the neural network.
                           #' @param input_neurons An integer specifying the number of input neurons.
                           #' @examples
                                                      #' X <- matrix(sample(1:100, 1200, replace=TRUE), ncol=3)
                                                      #' net <- NeuralNetwork$new(rand_state=42, layers_hidden=2, neurons_hidden = 4, problem_type = "classification", activ_type = "sigmoid")
                                                      #' net$init_network(ncol(X))
                           init_network = function(input_neurons){

                             set.seed(private$rand_state)

                             input_neurons <- as.integer(input_neurons)

                             # Initial weights and bias
                             if (private$.activ_type == "sigmoid" || private$.activ_type == "tanh"){

                               # For sigmoid and tanh, "glorot" weight initialization is used

                               # First layer
                               private$.W[[1]] <- matrix(runif(input_neurons * private$.neurons_hidden,
                                                               min=-(1/sqrt(input_neurons)),
                                                               max=(1/sqrt(input_neurons))),
                                                         input_neurons,
                                                         private$.neurons_hidden)

                               private$.b[[1]] <- runif(private$.neurons_hidden,
                                                        min=-(1/sqrt(input_neurons)),
                                                        max=(1/sqrt(input_neurons)))

                               # Hidden layers
                               if (private$.layers_hidden > 1){
                                 for(l in 1:(private$.layers_hidden-1)){

                                   private$.W[[l + 1]] <- matrix(runif(private$.neurons_hidden * private$.neurons_hidden,
                                                                       min=-(1/sqrt(private$.neurons_hidden)),
                                                                       max=(1/sqrt(private$.neurons_hidden))),
                                                                 private$.neurons_hidden,
                                                                 private$.neurons_hidden)

                                   private$.b[[l + 1]] <- runif(private$.neurons_hidden,
                                                                min=-(1/sqrt(private$.neurons_hidden)),
                                                                max=(1/sqrt(private$.neurons_hidden)))
                                 }
                               }

                               # Output layer
                               private$.W[[private$.layers_hidden + 1]] <- matrix(runif(private$.neurons_hidden * 1,
                                                                                        min=-(1/sqrt(private$.neurons_hidden)),
                                                                                        max=(1/sqrt(private$.neurons_hidden))),
                                                                                  private$.neurons_hidden,
                                                                                  1)

                               private$.b[[private$.layers_hidden + 1]] <- runif(1,
                                                                                 min=-(1/sqrt(private$.neurons_hidden)),
                                                                                 max=(1/sqrt(private$.neurons_hidden)))
                             }
                             else {

                               # For ReLU activation, "he" weight activation is used
                               private$.W[[1]] <- matrix(rnorm(input_neurons * private$.neurons_hidden,
                                                               mean=0,
                                                               sd=(2/sqrt(input_neurons))),
                                                         input_neurons,
                                                         private$.neurons_hidden)

                               private$.b[[1]] <- rnorm(private$.neurons_hidden,
                                                        mean=0,
                                                        sd=(2/sqrt(input_neurons)))

                               if (private$.layers_hidden > 1){
                                 for(l in 1:(private$.layers_hidden-1)){
                                   private$.W[[l+1]] <- matrix(rnorm(private$.neurons_hidden * private$.neurons_hidden,
                                                                     mean=0,
                                                                     sd=(2/sqrt(private$.neurons_hidden))),
                                                               private$.neurons_hidden,
                                                               private$.neurons_hidden)

                                   private$.b[[l+1]] <- rnorm(private$.neurons_hidden,
                                                              mean=0,
                                                              sd=(2/sqrt(private$.neurons_hidden)))
                                 }
                               }

                               private$.W[[private$.layers_hidden + 1]] <- matrix(rnorm(private$.neurons_hidden * 1,
                                                                                        mean=0,
                                                                                        sd=(2/sqrt(private$.neurons_hidden))),
                                                                                  private$.neurons_hidden,
                                                                                  1)

                               private$.b[[private$.layers_hidden + 1]] <- rnorm(1,
                                                                                 mean=0,
                                                                                 sd=(2/sqrt(private$.neurons_hidden)))
                             }

                           },

                           #' @description
                           #' Train the neural network.
                           #' @param X A data frame or matrix of the input data.
                           #' @param y A numeric vector of the target variable.
                           #' @param epochs An integer specifying the number of training epochs.
                           #' @param lr A numeric variable specifying the learning rate.
                           #' @param batch_size An integer specifying the size of each mini-batch.
                           #' @examples
                                                      #' X <- matrix(sample(1:100, 1200, replace=TRUE), ncol=3)
                                                      #' y <- sample(c(0,1), 400, replace=TRUE)
                                                      #' net <- NeuralNetwork$new(rand_state=42, layers_hidden=2, neurons_hidden = 4, problem_type = "classification", activ_type = "sigmoid")
                                                      #' net$init_network(ncol(X))
                                                      #' net$train(X, y, epochs=1000, lr=0.01, batch_size=64)
                           train = function(X, y, epochs=1000, lr=0.01, batch_size=64){

                             set.seed(private$rand_state)

                             # More defensive programming - checking if the data type is data frame or matrix
                             stopifnot(is.data.frame(X) || is.matrix(X))

                             # If there are NaN present an error is raised
                             if(any(is.na(X)) || any(is.na(y))){
                               stop("Error: There are missing values in your data!")
                             }

                             # Checking the type of target
                             if(private$.problem_type == "classification"){
                               stopifnot(is.integer(y) || is.numeric(y) || is.logical(y),
                                         length(unique(y)) == 2)
                             }
                             else {
                               stopifnot(is.numeric(y))
                             }

                             # Limiting the allowed number of epochs and verifying the data types
                             stopifnot(epochs <= 10000 && epochs >= 10)
                             stopifnot(is.numeric(lr))
                             stopifnot(batch_size <= dim(X)[1] && batch_size >= 32)

                             epochs = as.integer(epochs)

                             batch_size = as.integer(batch_size)

                             # Creating mini batches
                             batches = private$create_mini_batches(X, y, as.integer(batch_size))

                             prev_loss = 0

                             for (epo in 1:epochs){
                               loss = NULL
                               for (batch in batches){
                                 yhat = private$forward(batch[[1]])
                                 private$backward(batch[[2]], yhat)
                                 private$update(lr)
                                 loss = c(loss, private$loss_func(batch[[2]], yhat))
                               }

                               print(paste("[>] epoch=", epo, ", learning_rate=", lr, ", loss=", mean(loss), sep = ""))

                               if (is.nan(mean(loss))){
                                 break
                               }
                               else if (mean(loss) == prev_loss){
                                 break
                               }

                               prev_loss = mean(loss)

                             }
                           },

                           #' @description
                           #' Train the neural network.
                           #' @param X A data frame or matrix of the test data.
                           #' @param threshold For classification, threshold will describe where to cut off the predictions
                           #' @return A vector of predictions.
                           #' @examples
                                                      #' X <- matrix(sample(1:100, 1200, replace=TRUE), ncol=3)
                                                      #' y <- sample(c(0,1), 400, replace=TRUE)
                                                      #' X_test <- matrix(sample(1:100, 300, replace=TRUE), ncol=3)
                                                      #' net <- NeuralNetwork$new(rand_state=42, layers_hidden=2, neurons_hidden = 4, problem_type = "classification", activ_type = "sigmoid")
                                                      #' net$init_network(ncol(X))
                                                      #' net$train(X, y, epochs=1000, lr=0.01, batch_size=64)
                                                      #' predictions <- net$predict(X_test, threshold=0.5)
                           predict = function(X, threshold = 0.5){

                             stopifnot(is.data.frame(X) || is.matrix(X))

                             if(any(is.na(X))){
                               stop("Error: There are missing values in your data!")
                             }

                             stopifnot(is.numeric(threshold), threshold>=0.0 & threshold<=1.0)

                             preds = private$forward(X)
                             if(private$.problem_type == "classification"){
                               preds <- ifelse(preds>=threshold, 1, 0)
                             }
                             return(preds)
                           }
                         ))

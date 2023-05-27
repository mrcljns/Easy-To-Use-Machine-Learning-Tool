library(R6)
library(Rcpp)
sourceCpp('create_mini_batches.cpp')

# Code inspired by https://machinelearningmastery.com/application-of-differentiations-in-neural-networks/

NeuralNetwork <- R6Class("NeuralNetwork",
                         
                         private = list(
                           
                           z = NULL,
                           dz = NULL,
                           .W = NULL,
                           dW = NULL,
                           .b = NULL,
                           db = NULL,
                           a = NULL,
                           da = NULL,
                           .layers_hidden = NULL,
                           .neurons_hidden = NULL,
                           .problem_type = NULL,
                           loss_func = NULL,
                           dloss_func = NULL,
                           .activ_type = NULL,
                           activ_func = NULL,
                           dactiv_func = NULL,
                           out_func = NULL,
                           dout_func = NULL,
                           rand_state = NULL,
                           
                           forward = function(x){
                             private$a[[1]] = scale(as.matrix(x))
                             
                             for (l in 1:private$.layers_hidden){
                               private$z[[l]] = (private$a[[l]] %*% private$.W[[l]] + rep(private$.b[[l]], each=nrow(private$a[[l]])))
                               private$a[[l+1]] = apply(private$z[[l]], 2, private$activ_func)
                             }
                             
                             private$z[[private$.layers_hidden + 1]] = (private$a[[private$.layers_hidden + 1]] %*% private$.W[[private$.layers_hidden + 1]] +
                                                                         rep(private$.b[[private$.layers_hidden + 1]], each = nrow(private$a[[private$.layers_hidden + 1]])))
                             
                             private$a[[private$.layers_hidden + 2]] = private$out_func(private$z[[private$.layers_hidden + 1]])
                             return(private$a[[private$.layers_hidden + 2]])
                           },
                           
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
                           
                           update = function(lr){
                             for (l in 1:length(private$.W)){
                               private$.W[[l]] = private$.W[[l]] - lr * private$dW[[l]]
                               private$.b[[l]] = private$.b[[l]] - lr * private$db[[l]]
                             }
                           },
                           
                           create_mini_batches = function(X, y, batch_size) {
                             create_mini_batches_cpp(X, y, batch_size)
                             # mini_batches <- list()
                             # data <- cbind(X, y)
                             # data <- data[sample(nrow(data)), ]
                             # n_minibatches <- floor(nrow(data) / batch_size)
                             # 
                             # for (i in 0:(n_minibatches)) {
                             #   start <- i * batch_size + 1
                             #   end <- min((i + 1) * batch_size, nrow(data))
                             #   mini_batch <- data[start:end, ]
                             #   X_mini <- mini_batch[, -ncol(mini_batch)]
                             #   Y_mini <- matrix(mini_batch[, ncol(mini_batch)], nrow = length(start:end), ncol = 1)
                             #   mini_batches[[i + 1]] <- list(X_mini, Y_mini)
                             # }
                             # 
                             # if (nrow(data) %% batch_size != 0) {
                             #   start <- (n_minibatches) * batch_size + 1
                             #   end <- nrow(data)
                             #   mini_batch <- data[start:end, ]
                             #   X_mini <- mini_batch[, -ncol(mini_batch)]
                             #   Y_mini <- matrix(mini_batch[, ncol(mini_batch)], nrow = length(start:end), ncol = 1)
                             #   mini_batches[[n_minibatches + 1]] <- list(X_mini, Y_mini)
                             # }

                             #return(mini_batches)
                           }
                         ),
                         
                         # some field are kept active
                         # the user can view them, but not alter them
                         active = list(
                           
                           W = function() private$.W,
                           
                           b = function() private$.b,
                           
                           problem_type = function() private$.problem_type,
                           
                           activ_type = function() private$.activ_type,
                           
                           layers_hidden = function() private$.layers_hidden,
                           
                           neurons_hidden = function() private$.neurons_hidden
                         
                         ),
                         
                         public = list(

                           initialize = function(rand_state = 42,
                                                 layers_hidden,
                                                 neurons_hidden,
                                                 problem_type = "classification",
                                                 activ_type = "sigmoid"){
                             
                             # defensive programming
                             stopifnot(is.numeric(rand_state))
                             stopifnot(is.numeric(layers_hidden), layers_hidden>0 & layers_hidden<4)
                             stopifnot(is.numeric(neurons_hidden), layers_hidden>0 & layers_hidden<21)
                             stopifnot(is.character(problem_type), problem_type %in% c("classification", "regression"))
                             stopifnot(is.character(activ_type), activ_type %in% c("sigmoid", "tanh", "relu"))
                             
                             # the random state specified by the user
                             private$rand_state = as.integer(rand_state)
                             
                             # number of hidden layers
                             private$.layers_hidden = as.integer(layers_hidden)
                             
                             # number of neurons in hidden layers
                             # (for simplicity, the number of neurons is the same in each hidden layer)
                             private$.neurons_hidden = as.integer(neurons_hidden)
                             
                             # the problem type - either classification or regression
                             private$.problem_type = problem_type
                             
                             private$.activ_type = activ_type
                             
                             # initializing the activation function in hidden layers
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
                             # in other case it's relu
                             else {
                               private$activ_func = function(z) pmax(z, 0)
                               private$dactiv_func = function(z) as.numeric(z>0)
                             }
                             
                             # initializing output and loss function based on problem type
                             if (problem_type == "classification"){
                               
                               # sigmoid as output
                               private$out_func = function(z) 1/(1+exp(-pmin(pmax(z, -500), 500)))
                               private$dout_func = function(z){
                                 s = private$out_func(z)
                                 2 * s * (1-s)
                               }
                               
                               # binary cross entropy
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
                             
                             # initialize empty lists for storing
                             # adding 1 to number of hidden layers to account for output layer
                             private$z = vector(mode='list', length=private$.layers_hidden + 1)
                             private$.W = vector(mode='list', length=private$.layers_hidden + 1)
                             private$.b = vector(mode='list', length=private$.layers_hidden + 1)
                             private$a = vector(mode='list', length=private$.layers_hidden + 2)
                             private$dz = vector(mode='list', length=private$.layers_hidden + 1)
                             private$dW = vector(mode='list', length=private$.layers_hidden + 1)
                             private$db = vector(mode='list', length=private$.layers_hidden + 1)
                             private$da = vector(mode='list', length=private$.layers_hidden + 2)
                           },
                           
                           init_network = function(input_neurons){
                             
                             set.seed(private$rand_state)
                             
                             stopifnot(is.numeric(input_neurons))
                             
                             # initial weights and bias
                             if (private$.activ_type == "sigmoid" | private$.activ_type == "tanh"){
                               
                               # for sigmoid and tanh, "glorot" weight initialization is used
                               
                               # first layer
                               private$.W[[1]] <- matrix(runif(input_neurons * private$.neurons_hidden,
                                                           min=-(1/sqrt(input_neurons)),
                                                           max=(1/sqrt(input_neurons))),
                                                     input_neurons,
                                                     private$.neurons_hidden)
                               
                               private$.b[[1]] <- runif(private$.neurons_hidden,
                                                    min=-(1/sqrt(input_neurons)),
                                                    max=(1/sqrt(input_neurons)))
                               
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
                               
                               # last layer
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
                               
                               # for relu activation, "he" weight activation is used
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
                           
                           train = function(X, y, epochs, lr, batch_size){
                             
                             set.seed(private$rand_state)
                             
                             stopifnot(is.data.frame(X) | is.matrix(X))
                             
                             if(private$.problem_type == "classification"){
                               stopifnot(is.integer(y) | is.numeric(y), length(unique(y)) == 2)
                             }
                             else {
                               stopifnot(is.numeric(y))
                             }
                             
                             stopifnot(is.numeric(epochs), epochs <= 10000)
                             stopifnot(is.numeric(lr))
                             stopifnot(is.numeric(batch_size), batch_size <= dim(X)[1])
                             
                             batches = private$create_mini_batches(X, y, as.integer(batch_size))
                             for (epo in 1:epochs){
                               loss = NULL
                               for (batch in batches){
                                 yhat = private$forward(batch[[1]])
                                 private$backward(batch[[2]], yhat)
                                 private$update(lr)
                                 loss = c(loss, private$loss_func(batch[[2]], yhat))
                               }
                               print(paste("[>] epoch=", epo, ", learning_rate=", lr, ", loss=", mean(loss), sep = ""))
                             }
                           },
                           
                           predict = function(X, threshold = 0.5){
                             
                             stopifnot(is.data.frame(X) | is.matrix(X))
                             stopifnot(is.numeric(threshold), threshold>=0.0 & threshold<=1.0)
                             
                             preds = private$forward(X)
                             if(private$.problem_type == "classification"){
                               preds <- ifelse(preds>=threshold, 1, 0)
                             }
                             return(preds)
                           }
                         ))
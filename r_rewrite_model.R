library(R6)

# Code inspired by https://machinelearningmastery.com/application-of-differentiations-in-neural-networks/

NeuralNetwork <- R6Class("NeuralNetwork",
                         public = list(
                           z = NULL,
                           dz = NULL,
                           W = NULL,
                           dW = NULL,
                           b = NULL,
                           db = NULL,
                           a = NULL,
                           da = NULL,
                           layers_hidden = NULL,
                           neurons_hidden = NULL,
                           problem_type = NULL,
                           loss_func = NULL,
                           dloss_func = NULL,
                           activ_type = NULL,
                           activ_func = NULL,
                           dactiv_func = NULL,
                           out_func = NULL,
                           dout_func = NULL,
                           rand_state = NULL,
                           initialize = function(rand_state = 42,
                                                 layers_hidden,
                                                 neurons_hidden,
                                                 problem_type = "classification",
                                                 activ_type = "sigmoid"){
                             # the random state specified by the user
                             self$rand_state = rand_state
                             
                             # number of hidden layers
                             self$layers_hidden = layers_hidden
                             
                             # number of neurons in hidden layers
                             # (for simplicity, the number of neurons is the same in each hidden layer)
                             self$neurons_hidden = neurons_hidden
                             
                             # the problem type - either classification or regression
                             self$problem_type = problem_type
                             
                             self$activ_type = activ_type
                             
                             # initializing the activation function in hidden layers
                             if (activ_type == "sigmoid"){
                               self$activ_func = function(z) 1/(1+exp(-pmin(pmax(z, -500), 500)))
                               
                               self$dactiv_func = function(z) {
                                 s = self$activ_func(z)
                                 2 * s * (1 - s)
                               }
                             }
                             else if (activ_type == "tanh"){
                               self$activ_func = function(z) tanh(z)
                               self$dactiv_func = function(z) 1 - tanh(z)**2
                             }
                             # in other case it's relu
                             else {
                               self$activ_func = function(z) pmax(z, 0)
                               self$dactiv_func = function(z) as.numeric(z>0)
                             }
                             
                             # initializing output and loss function based on problem type
                             if (problem_type == "classification"){
                               
                               # sigmoid as output
                               self$out_func = function(z) 1/(1+exp(-pmin(pmax(z, -500), 500)))
                               self$dout_func = function(z){
                                 s = self$out_func(z)
                                 2 * s * (1-s)
                               }
                               
                               # binary cross entropy
                               self$loss_func = function(y, yhat){
                                 -(t(y) %*% log(pmax(yhat, .Machine$double.eps)) + (1-t(y)) %*% log(pmax((1-yhat), .Machine$double.eps))) / length(y)
                               }
                               
                               self$dloss_func = function(y, yhat){
                                 -y/pmax(yhat, .Machine$double.eps) + (1-y)/pmax((1-yhat), .Machine$double.eps)
                               }
                             }
                             else {
                               self$out_func = function(z) z
                               self$dout_func = function(z) 1
                               
                               # MSE
                               self$loss_func = function(y, yhat){
                                 (yhat-y)**2 / length(y)
                               }
                               
                               self$dloss_func = function(y, yhat){
                                 2*(yhat-y)
                               }
                             }
                             
                             # initialize empty lists for storing
                             # adding 1 to number of hidden layers to account for output layer
                             self$z = vector(mode='list', length=self$layers_hidden + 1)
                             self$W = vector(mode='list', length=self$layers_hidden + 1)
                             self$b = vector(mode='list', length=self$layers_hidden + 1)
                             self$a = vector(mode='list', length=self$layers_hidden + 2)
                             self$dz = vector(mode='list', length=self$layers_hidden + 1)
                             self$dW = vector(mode='list', length=self$layers_hidden + 1)
                             self$db = vector(mode='list', length=self$layers_hidden + 1)
                             self$da = vector(mode='list', length=self$layers_hidden + 2)
                           },
                           
                           init_network = function(input_neurons){
                             set.seed(self$rand_state)
                             
                             # initial weights and bias
                             if (self$activ_type == "sigmoid" | self$activ_type == "tanh"){
                               
                               # for sigmoid and tanh, "glorot" weight initialization is used
                               
                               # first layer
                               self$W[[1]] <- matrix(runif(input_neurons * self$neurons_hidden,
                                                           min=-(1/sqrt(input_neurons)),
                                                           max=(1/sqrt(input_neurons))),
                                                     input_neurons,
                                                     self$neurons_hidden)
                               
                               self$b[[1]] <- runif(self$neurons_hidden,
                                                    min=-(1/sqrt(input_neurons)),
                                                    max=(1/sqrt(input_neurons)))
                               
                               if (self$layers_hidden > 1){
                                 for(l in 1:(self$layers_hidden-1)){
                                   
                                   self$W[[l + 1]] <- matrix(runif(self$neurons_hidden * self$neurons_hidden,
                                                                   min=-(1/sqrt(self$neurons_hidden)),
                                                                   max=(1/sqrt(self$neurons_hidden))),
                                                             self$neurons_hidden,
                                                             self$neurons_hidden)
                                   
                                   self$b[[l + 1]] <- runif(self$neurons_hidden,
                                                            min=-(1/sqrt(self$neurons_hidden)),
                                                            max=(1/sqrt(self$neurons_hidden)))
                                 }
                               }
                               
                               # last layer
                               self$W[[self$layers_hidden + 1]] <- matrix(runif(self$neurons_hidden * 1,
                                                           min=-(1/sqrt(self$neurons_hidden)),
                                                           max=(1/sqrt(self$neurons_hidden))),
                                                     self$neurons_hidden,
                                                     1)
                               
                               self$b[[self$layers_hidden + 1]] <- runif(1,
                                                    min=-(1/sqrt(self$neurons_hidden)),
                                                    max=(1/sqrt(self$neurons_hidden)))
                             }
                             else {
                               
                               # for relu activation, "he" weight activation is used
                               self$W[[1]] <- matrix(rnorm(input_neurons * self$neurons_hidden,
                                                           mean=0,
                                                           sd=(2/sqrt(input_neurons))),
                                                     input_neurons,
                                                     self$neurons_hidden)
                               
                               self$b[[1]] <- rnorm(self$neurons_hidden,
                                                    mean=0,
                                                    sd=(2/sqrt(input_neurons)))
                               
                               if (self$layers_hidden > 1){
                                 for(l in 1:(self$layers_hidden-1)){
                                   self$W[[l+1]] <- matrix(rnorm(self$neurons_hidden * self$neurons_hidden,
                                                               mean=0,
                                                               sd=(2/sqrt(self$neurons_hidden))),
                                                         self$neurons_hidden,
                                                         self$neurons_hidden)
                                   
                                   self$b[[l+1]] <- rnorm(self$neurons_hidden,
                                                        mean=0,
                                                        sd=(2/sqrt(self$neurons_hidden)))
                                 }
                               }
                               
                               self$W[[self$layers_hidden + 1]] <- matrix(rnorm(self$neurons_hidden * 1,
                                                           mean=0,
                                                           sd=(2/sqrt(self$neurons_hidden))),
                                                           self$neurons_hidden,
                                                     1)
                               
                               self$b[[self$layers_hidden + 1]] <- rnorm(1,
                                                    mean=0,
                                                    sd=(2/sqrt(self$neurons_hidden)))
                             }
                             
                           },
                           
                           forward = function(x){
                             self$a[[1]] = scale(as.matrix(x))
                             
                             for (l in 1:self$layers_hidden){
                               self$z[[l]] = (self$a[[l]] %*% self$W[[l]] + rep(self$b[[l]], each=nrow(self$a[[l]])))
                               self$a[[l+1]] = apply(self$z[[l]], 2, self$activ_func)
                             }
                             
                             self$z[[self$layers_hidden + 1]] = (self$a[[self$layers_hidden + 1]] %*% self$W[[self$layers_hidden + 1]] +
                                                                   rep(self$b[[self$layers_hidden + 1]], each = nrow(self$a[[self$layers_hidden + 1]])))
                             
                             self$a[[self$layers_hidden + 2]] = self$out_func(self$z[[self$layers_hidden + 1]])
                             return(self$a[[self$layers_hidden + 2]])
                           },

                           backward = function(y, yhat){
                             
                             self$da[[self$layers_hidden + 2]] = self$dloss_func(y, yhat)
                             self$dz[[self$layers_hidden + 1]] = self$da[[self$layers_hidden + 2]] * self$dout_func(self$z[[self$layers_hidden + 1]])
                             self$dW[[self$layers_hidden + 1]] = t(self$a[[self$layers_hidden + 1]]) %*% self$dz[[self$layers_hidden + 1]]
                             self$db[[self$layers_hidden + 1]] = apply(self$dz[[self$layers_hidden + 1]], 2, mean)
                             self$da[[self$layers_hidden + 1]] = self$dz[[self$layers_hidden + 1]] %*% t(self$W[[self$layers_hidden + 1]])
                             
                             for (l in (self$layers_hidden):1){
                               self$dz[[l]] = self$da[[l+1]] * self$dactiv_func(self$z[[l]])
                               self$dW[[l]] = t(self$a[[l]]) %*% self$dz[[l]]
                               self$db[[l]] = apply(self$dz[[l]], 2, mean)
                               self$da[[l]] = self$dz[[l]] %*% t(self$W[[l]])
                             }
                           },
                           
                           update = function(lr){
                             for (l in 1:length(self$W)){
                               self$W[[l]] = self$W[[l]] - lr * self$dW[[l]]
                               self$b[[l]] = self$b[[l]] - lr * self$db[[l]]
                             }
                           },
                           
                           create_mini_batches = function(X, y, batch_size) {
                             mini_batches <- list()
                             data <- cbind(X, y)
                             data <- data[sample(nrow(data)), ]
                             n_minibatches <- floor(nrow(data) / batch_size)
                             
                             for (i in 0:(n_minibatches)) {
                               start <- i * batch_size + 1
                               end <- min((i + 1) * batch_size, nrow(data))
                               mini_batch <- data[start:end, ]
                               X_mini <- mini_batch[, -ncol(mini_batch)]
                               Y_mini <- matrix(mini_batch[, ncol(mini_batch)], nrow = length(start:end), ncol = 1)
                               mini_batches[[i + 1]] <- list(X_mini, Y_mini)
                             }
                             
                             if (nrow(data) %% batch_size != 0) {
                               start <- (n_minibatches) * batch_size + 1
                               end <- nrow(data)
                               mini_batch <- data[start:end, ]
                               X_mini <- mini_batch[, -ncol(mini_batch)]
                               Y_mini <- matrix(mini_batch[, ncol(mini_batch)], nrow = length(start:end), ncol = 1)
                               mini_batches[[n_minibatches + 1]] <- list(X_mini, Y_mini)
                             }
                             
                             return(mini_batches)
                           },
                           
                           train = function(X, y, epochs, lr, batch_size){
                             batches = self$create_mini_batches(X, y, batch_size)
                             for (epo in 1:epochs){
                               loss = NULL
                               for (batch in batches){
                                 yhat = self$forward(batch[[1]])
                                 self$backward(batch[[2]], yhat)
                                 self$update(lr)
                                 loss = c(loss, self$loss_func(batch[[2]], yhat))
                               }
                               print(paste("[>] epoch=", epo, ", learning_rate=", lr, ", loss=", mean(loss), sep = ""))
                             }
                           },
                           
                           predict = function(x, threshold = 0.5){
                             preds = self$forward(x)
                             if(self$problem_type == "classification"){
                               preds <- ifelse(preds>=threshold, 1, 0)
                             }
                             return(preds)
                           }
                         ))
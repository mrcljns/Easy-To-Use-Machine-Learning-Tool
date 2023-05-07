#include <Rcpp.h>
#include <vector>
#include <iostream>
using namespace Rcpp;

// Code inspired by the github repository: https://github.com/Cr4ckC4t/neural-network-from-scratch

// Neuron class
class Neuron{
public:
  std::string m_activ_func;
  
  Neuron(int num_weights, std::string activ_func){
    this -> init_weights(num_weights);
    m_num_weights = num_weights;
    m_activation = 0;
    m_output = 0;
    m_delta = 0;
    m_activ_func = activ_func;
  };
  ~Neuron(){
    
  };
  
  void activate(NumericVector inputs){
    // store the last weight as the bias
    m_activation = weights[m_num_weights-1];
    
    // store weighted inputs
    for(int i=0; i<m_num_weights-1; i++){
      m_activation += weights[i] * inputs[i];
    }
  };

  void transfer(){
    if(m_activ_func == "sigmoid"){
    // sigmoid
      m_output = 1.0f / (1.0f + std::exp(-m_activation));
    } else if(m_activ_func == "relu"){
      m_output = std::max(0.0f, m_activation);
    } else if(m_activ_func == "tanh"){
      m_output = std::tanh(m_activation);
    } else{
      m_output = m_activation;
    }

  };

  float transfer_derivative(){
    if(m_activ_func == "sigmoid"){
      return static_cast<float>(m_output * (1.0-m_output));
    } else if(m_activ_func == "relu"){
        return static_cast<float>(m_output>=0);
    } else if(m_activ_func == "tanh"){
      return static_cast<float>(1.0-std::pow(m_output, 2));
    } else{
      return 1.0;
    }
  };

  float get_output(){
    return m_output;
  };

  float get_activation(){
    return m_activation;
  };

  float get_delta(){
    return m_delta;
  };

  void set_delta(float delta){
    m_delta = delta;
  };

  NumericVector get_weights(){
    return weights;
  };
  
private:
  int m_num_weights;
  NumericVector weights;
  float m_activation;
  float m_output;
  float m_delta;
  
  void init_weights(int num_weights){
    for (int i=0; i<num_weights; i++){
      weights.push_back(static_cast<float>(std::rand()) / static_cast<float>(RAND_MAX));
    }
  };
};


// Layer class
class Layer {
public:
  Layer(int num_neurons, int num_weights, std::string activ_func){
    this -> init_neurons(num_neurons, num_weights, activ_func);
  }

  ~Layer(){
  }

  std::vector<Neuron>& get_neurons(){
    return m_neurons;
  }

private:
  void init_neurons(int num_neurons, int num_weights, std::string activ_func){
    for(int i=0; i<num_neurons;i++){
      m_neurons.push_back(Neuron(num_weights, activ_func));
    }
  }

  std::vector<Neuron> m_neurons;
};

// Neural net class
class NeuralNet {
public:
  NeuralNet(){
    num_layers = 0;
  }
  ~NeuralNet(){

  }

  void init_network(int n_inputs, int n_hidden, int n_outputs, std::string activ_func, 
                    std::string problem_type){
    
    m_problem_type = problem_type;
    
    this -> add_layer(n_hidden, n_inputs+1, activ_func);
    this -> add_layer(n_hidden, n_inputs+1, activ_func);
    
    // output layer
    if(m_problem_type == "classification"){
      this -> add_layer(n_outputs, n_hidden+1, "sigmoid");
    }
    else{
      this -> add_layer(1, n_hidden+1, "");
    }
  }

  void add_layer(int num_neurons, int num_weights, std::string activ_func){
    m_layers.push_back(Layer(num_neurons, num_weights, activ_func));
    num_layers++;
  }

  NumericVector forward_prop(NumericVector inputs){
    NumericVector new_inputs;
    for(int i=0; i<num_layers; i++){
      new_inputs.erase(new_inputs.begin(), new_inputs.end());

      std::vector<Neuron>& layer_neurons = m_layers[i].get_neurons();
      for(int j=0; j<layer_neurons.size(); j++){
        layer_neurons[j].activate(inputs);
        layer_neurons[j].transfer();
        new_inputs.push_back(layer_neurons[j].get_output());
      }
      inputs = new_inputs;
    }
    return inputs;
  }

  void backward_prop(NumericVector expected){
    for(int i=num_layers; i --> 0;){
      std::vector<Neuron>& layer_neurons = m_layers[i].get_neurons();

      for(int j=0; j<layer_neurons.size(); j++){
        float error = 0.0;

        if (i == num_layers-1){
          error = expected[j] - layer_neurons[j].get_output();
        }
        else{
          for(auto& neu : m_layers[i+1].get_neurons()){
            error += (neu.get_weights()[j] * neu.get_delta());
          }
        }
        layer_neurons[j].set_delta(error * layer_neurons[j].transfer_derivative());
      }
    }
  }
  
  void backward_prop_regression(float expected){
    for(int i=num_layers; i --> 0;){
      std::vector<Neuron>& layer_neurons = m_layers[i].get_neurons();
      
      for(int j=0; j<layer_neurons.size(); j++){
        float error = 0.0;
        
        if (i == num_layers-1){
          error = expected - layer_neurons[j].get_output();
        }
        else{
          for(auto& neu : m_layers[i+1].get_neurons()){
            error += (neu.get_weights()[j] * neu.get_delta());
          }
        }
        layer_neurons[j].set_delta(error * layer_neurons[j].transfer_derivative());
      }
    }
  }

  void update_weights(NumericVector inputs, float learning_rate){
    for(int i=0; i<num_layers; i++){
      NumericVector new_inputs(0);
      if(i!=0){
        for(auto &neuron: m_layers[i-1].get_neurons()){
          new_inputs.push_back(neuron.get_output());
        }
      } else {
        new_inputs = std::vector<float>(inputs.begin(), inputs.end() - 1);
      }

      std::vector<Neuron>& layer_neurons = m_layers[i].get_neurons();

      for(int j=0;j<layer_neurons.size(); j++){
        NumericVector neuron_weights = layer_neurons[j].get_weights();
          for(int k=0; k<new_inputs.size(); k++){
            neuron_weights[k] += learning_rate * layer_neurons[j].get_delta() * new_inputs[k];
          }
          neuron_weights[neuron_weights.size()-1] += learning_rate * layer_neurons[j].get_delta();
      }
    }
  }

  void train(NumericMatrix X_train, NumericVector y_train, float learning_rate, int epochs, 
             int num_outputs){
    if(m_problem_type == "classification"){
      for(int epoch=0; epoch<epochs; epoch++){
        float sum_error = 0;
  
        for(int i=0; i<X_train.nrow(); i++){
          NumericVector outputs = this -> forward_prop(X_train.row(i));
          NumericVector expected(num_outputs, 0.0);
          expected[static_cast<int>(X_train.row(i)[X_train.row(i).size()-1])] = 1.0;
  
          for(int x=0; x<num_outputs; x++){
            sum_error += static_cast<float>(expected[x] * std::log(outputs[x]));
          }
          this -> backward_prop(expected);
          this -> update_weights(X_train.row(i), learning_rate);
        }
        std::cout << "[>] epoch=" << epoch << ", learning_rate=" << learning_rate << ", error=" << sum_error << std::endl;
      }
    } else{
      
      NumericVector expected = y_train;
      
      for(int epoch=0; epoch<epochs; epoch++){
        float sum_error = 0;
        
        for(int i=0; i<expected.size(); i++){
          NumericVector outputs = this -> forward_prop(X_train.row(i));
          
          sum_error += static_cast<float>(std::pow((expected[i] - outputs[i]), 2));

          this -> backward_prop_regression(expected[i]);
          this -> update_weights(X_train.row(i), learning_rate);
        }
        std::cout << "[>] epoch=" << epoch << ", learning_rate=" << learning_rate << ", error=" << sum_error << std::endl;
      }
    }
  }

  float predict(NumericVector input){
    NumericVector outputs = this -> forward_prop(input);
    if(m_problem_type == "classification"){
      return std::max_element(outputs.begin(), outputs.end()) - outputs.begin();
    } else{
      return outputs[0];
    }
  }

private:
  int num_layers;
  std::string m_problem_type;
  std::vector<Layer> m_layers;
};

RCPP_MODULE(NeuralNet){
  using namespace Rcpp;
  
  class_<NeuralNet>("NeuralNet")
    .constructor()
    .method("init_network", &NeuralNet::init_network)
    .method("train", &NeuralNet::train)
    .method("predict", &NeuralNet::predict)
    .method("forward_prop", &NeuralNet::forward_prop)
    .method("backward_prop", &NeuralNet::backward_prop)
    .method("update_weights", &NeuralNet::update_weights)
    ;
}
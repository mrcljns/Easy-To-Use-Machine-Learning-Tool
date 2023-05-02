#include <Rcpp.h>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace Rcpp;

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
  ~Neuron();
  
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
    } else{
      m_output = tanhf(m_activation);
    }
    
  };
  
  float transfer_derivative(){
    return static_cast<float>(m_output * (1.0-m_output));
  };
  
  float get_output(void){
    return m_output;
  };
  
  float get_activation(void){
    return m_activation;
  };
  
  float get_delta(void){
    return m_delta;
  };
  
  void set_delta(float delta){
    m_delta = delta;
  };
  
  NumericVector get_weights(void){
    return weights;
  }
  
private:
  int m_num_weights;
  NumericVector weights;
  float m_activation;
  float m_output;
  float m_delta;
  
  void init_weights(int num_weights){
    NumericVector weights;
    for (int i=0; i<num_weights; i++){
      weights[i] = ((double) rand() / (RAND_MAX));
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
  
  std::vector<Neuron>& get_neurons(void){
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
  NeuralNet();
  ~NeuralNet();
  
  void init_network(int n_inputs, int n_hidden, int n_outputs, std::string activ_func){
    this -> add_layer(n_hidden, n_inputs+1, activ_func);
    this -> add_layer(n_outputs, n_hidden+1, activ_func);
  }
  
  void add_layer(int num_neurons, int num_weights, std::string activ_func){
    m_layers.push_back(Layer(num_neurons, num_weights, activ_func));
    num_layers++;
  }
  
  NumericVector forward_prop(NumericVector inputs){
    std::vector<float> new_inputs;
    for(int i=0; i<num_layers; i++){
      new_inputs.clear();
      
      std::vector<Neuron>& layer_neurons = m_layers[i].get_neurons();
      for(int i=0; i<layer_neurons.size(); i++){
        layer_neurons[i].activate(inputs);
        layer_neurons[i].transfer();
        new_inputs.push_back(layer_neurons[i].get_output());
      }
      inputs = new_inputs;
    }
    return inputs;
  }
  
  void backward_prop(NumericVector expected){
    for(int i=num_layers; i>0; i--){
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
        layer_neurons[j].set_delta(error* layer_neurons[j].transfer_derivative());
      }
    }
  }
  
  void update_weights(NumericVector inputs, float learning_rate){
    
  }
  
  void train(std::vector<NumericVector> X_train, float learning_rate, int epochs, int outputs){
    
  }
  
  float predict(NumericVector input){
    NumericVector outputs = this -> forward_prop(input);
    return std::max_element(outputs.begin(), outputs.end()) - outputs.begin();
  }
  
private:
  int num_layers;
  std::vector<Layer> m_layers;
};
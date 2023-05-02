#include <Rcpp.h>
#include <vector>
#include <iostream>
using namespace Rcpp;

class Neuron{
public:
  Neuron(int num_w){
    this -> init_weights(num_w);
    m_num_w = num_w;
    m_activation = 0;
    m_output = 0;
    m_delta = 0;
  };
  ~Neuron();
  
  void activate(NumericVector inputs){
    // store the last weight as the bias
    m_activation = weights[m_num_w-1];
    
    // store weighted inputs
    for(int i=0; i<m_num_w-1; i++){
      m_activation += weights[i] * inputs[i];
    };
  };
  
  void transfer(){
    // sigmoid
    m_output = 1.0f / (1.0f + std::exp(-m_activation));
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
  
private:
  int m_num_w;
  NumericVector weights;
  float m_activation;
  float m_output;
  float m_delta;
private:
  void init_weights(int num_w){
    NumericVector weights;
    for (int i=0; i<num_w; i++){
      weights[i] = ((double) rand() / (RAND_MAX));
    }
  };
};
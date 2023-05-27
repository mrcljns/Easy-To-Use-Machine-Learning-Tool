#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List create_mini_batches_cpp(NumericMatrix X, NumericVector y, int batch_size) {
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
}
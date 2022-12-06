//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=2> K;
  int<lower=0> N;
  int<lower=1> D_1;
  int<lower=1> D_2;
  array[N] row_vector[D_1] X_1;   // predictor matrix
  array[N] row_vector[D_2] X_2;
  array[N] int<lower=1, upper=K>  y;   // outcome matrix
}


parameters {
  vector[D_1] beta;
  vector[D_2] beta2;
  ordered[K - 1] c;
}


model {
  for (i in 1:N)
    y[i] ~ ordered_logistic(X_1[1] * beta + X_2[1] * beta2,  c);

}


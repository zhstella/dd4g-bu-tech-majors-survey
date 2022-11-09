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

data {
  int<lower=2> K;
  int<lower=0> N;
  int<lower=1> D;
  array[N] row_vector[D] X;   // predictor matrix
  array[N] int<lower=1, upper=K>  y;   // outcome matrix
}


parameters {
  vector[D] beta;
  ordered[K - 1] c;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (i in 1:N)
    y[i] ~ ordered_logistic(X[1] * beta, c);
    // y[i] ~ normal(alpha + beta * X[i], sigma);

}



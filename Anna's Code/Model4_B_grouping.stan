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
  int<lower=0> N_c;
  int<lower=1> D_dep;
  int<lower=1> D_lev;
  int<lower=1> D_race;
  array[N] row_vector[D_dep] X_dep;   // predictor matrix
  array[N] row_vector[D_lev] X_lev;
  array[N] row_vector[D_race] X_race;
  array[N] int<lower=1, upper=K>  y;   // outcome matrix
}

parameters {
  vector[D_dep] Dep_beta;
  vector[D_lev] Lev_beta;
  vector[D_race] Race_beta;
  ordered[N_c] course_beta;
  ordered[K - 1] c;
}


model {
  for (i in 1:N_c)
    course_beta[i] = Dep_beta[D_dep[i]] + Lev_beta_beta[D_lev[i]] + eta[i];

  for (i in 1:N)
    response[i] = ordinal(course_beta[course[i]] + Race_beta[D_race[i]]);
}


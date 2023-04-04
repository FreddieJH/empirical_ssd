


data {
  int<lower=0> N;           // number of observations
  vector[N] x;              // body size data
}

parameters {
  real<lower=0> mu;         // mean of log(body size)
  real<lower=0> sigma;      // standard deviation of log(body size)
}

model {
  mu ~ normal(0, 10);       // prior on mean
  sigma ~ cauchy(0, 2.5);   // prior on standard deviation
  x ~ lognormal(mu, sigma); // lognormal likelihood
}
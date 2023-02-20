// NormalFit.stan
// Shane.Richards@utas.edu.au
// 17/02/2023
// Fit normal distributions to fish=length distributions
 
data {
  int<lower=1> I;            // number of observations
  int<lower=1> S;            // number of species
  int<lower=1> B;            // number of length bins
  real<lower=0.0> l[B];      // upper boundary of each bin
  int<lower=1,upper=S> s[I]; // species ID
  int<lower=1,upper=B> b[I]; // bin
  int<lower=0> n[I];         // fish in the bin
}
 
parameters {
  real<lower= -1.0,upper=1.0>  ln_cv; // coef of variation; lengths across species
  vector<lower= 0.5,upper=6.0>[S] ln_mu; // mean lengths for each species
}

model {
	real mu;    // mean of normal size distribution
	real sigma; // sd of normal size distribution
	real p;     // predicted probability of being in specified bin

	// priors on model parameters
	ln_cv   ~ normal(0.0, 0.5); // log-normal prior on cv 
	ln_mu   ~ normal(3, 1);     // log-normal prior for community mean lengths
	
  for (i in 1:I) { // for each set of lengths
    mu = exp(ln_mu[s[i]]); // mean species length
    sigma = mu*exp(ln_cv); // species length sd
    if (b[i] == 1) { // special case (between 0 and 2.5cm)
       p = normal_cdf(l[1], mu, sigma); // probability in smallest bin
    } else { // all other bins
       p = normal_cdf(l[b[i]], mu, sigma) - normal_cdf(l[b[i]-1], mu, sigma); // prob
    }
    target += n[i]*log(p); // add log-likelihood term
  }
}


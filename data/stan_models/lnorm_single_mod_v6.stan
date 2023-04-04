// Freddie Heather
// lognormal fit SSD, all species(k), effect of latitude (s) and season (t)



data {
  int<lower=0> N;                // number of data points
  int<lower=1> K;                // number of body size bins
  int<lower=1> L;                // number of locations
  int<lower=1> Y;                // number of years
  int<lower=1> S;                // number of species
  int<lower=1, upper=L> loc_indx[N];  // location index for each data point
  int<lower=1, upper=Y> year_indx[N]; // year index for each data point
  int<lower=1, upper=S> species_indx[N]; // species index for each data point
  real loc[N];  // location value for each data point
  real year[N]; // year value for each data point
  int<lower=1, upper=K> bin[N];  // bin index for each data point
  real<lower=0.0> uprsize[K];    // upper body size for each bin
  int<lower=0> indivs[N];     // number of fish in the size bin
}

parameters {
  // real<lower=0.5, upper = 4> mu_0;                      // lognormal mean parameter
  real<lower=-2, upper = 2> mu_loc;               // location effects on mean
  real<lower=-2, upper = 2> mu_year;              // year effects on mean
  vector<lower=0.5, upper = 4>[S] mu_species;           // species effects on mean
  // real<lower=-1, upper = 1> mu_sigma;         // lognormal standard deviation parameter
  // real<lower=-5, upper = 5> beta_0;
  // real<lower=-2, upper = 2> beta_1;
  // real<lower=-1, upper = 1> sigma_loc;   // location effects on standard deviation
  // real<lower=-1, upper = 1> sigma_year;  // year effects on standard deviation
  vector<lower=-3, upper = 1>[S] sigma_species; // species effects on standard deviation
}

model {
  real prob;
  real log_size_mean;
  real sigma;
  
  // Priors
  // mu_0 ~ normal(3.0, 1.0);
  // mu_sigma ~ normal(0.0, 0.5);
  
  // Linear model for mean
  // for (l in 1:L){
    mu_loc ~ normal(0.0, 1.0);
    // sigma_loc ~ normal(0.0, 0.5);
  // }
  
  // for (y in 1:Y){
    mu_year ~ normal(0.0, 1.0);
    // sigma_year ~ normal(0.0, 0.5);
  // }
  
  for (s in 1:S){
    mu_species[s] ~ lognormal(3.0, 0.5);
    sigma_species[s] ~ normal(0.0, 0.5);
  }
  
  // beta_0 ~ normal(0, 2);
  // beta_1 ~ normal(0, 2);
  
  // Likelihood
  for (n in 1:N) {
    // sigma = exp(mu_sigma + sigma_loc*loc[n] + sigma_year*year[n]);
    // log_size_mean = mu_0 +  mu_loc*loc[n] + mu_year*year[n];
    log_size_mean = mu_loc*loc[n] + mu_year*year[n] + mu_species[species_indx[n]];
    sigma = exp(sigma_species[species_indx[n]]);// + beta_0 + beta_1*log_size_mean);// + sigma_loc*loc[n] + sigma_year*year[n] + sigma_species[species_indx[n]]);
    // real mu = exp(log_size_mean);
    if (bin[n] == 1) { // probability in smallest bin
    prob = lognormal_cdf(uprsize[1], log_size_mean, sigma); 
    } else { // all other bins (>2.5cm)
    prob = lognormal_cdf(uprsize[bin[n]], log_size_mean, sigma) - 
    lognormal_cdf(uprsize[bin[n]-1], log_size_mean, sigma); 
    }
    target += indivs[n]*log(prob); 
  }
}

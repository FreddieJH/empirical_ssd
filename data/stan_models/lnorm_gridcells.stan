// Freddie Heather
// lognormal fit to a single species, for multiple years and gridcells


data {
  // Integers
  // int<lower=1> n_locations;     // number of locations (i.e. latitude gridcell)
  // int<lower=1> n_years;         // number of years
  // int<lower=1> n_species;       // number of species
  int<lower=1> n_sizebins;      // number of size bins
  int<lower=1> n_observations;  // number of observations
  
  // Vectors
  // int<lower=1,upper=n_species> species_id[n_observations];    // species ID
  int<lower=1,upper=n_sizebins> sizebin_id[n_observations];   // size bin ID
  int<lower=0> n_individuals[n_observations];     // number of fish in the size bin
  real<lower=0.0> upper_sizebin[n_sizebins];         // upper boundary of each bin
  real lat[n_observations];
  real yr[n_observations];
  
}
 
parameters {
  real<lower= -1.0, upper=1.0>  ln_sigma; // coef of variation (needs to be exponentited first)
  real<lower= -1.0, upper=1.0>  ln_sigma_lat; // coef of variation (needs to be exponentited first)
  real<lower= -1.0, upper=1.0>  ln_sigma_year; // coef of variation (needs to be exponentited first)
  real<lower= 0.5, upper=4.0> ln_mu; 
  real<lower= -2, upper=2> ln_mu_lat; 
  real<lower=  -2, upper=2> ln_mu_year; 
}

model {
  
	real prob;     // predicted probability of being in specified bin
  real sigma;
  real ln_mu_new;

	// priors on model parameters
	ln_sigma ~ normal(0.0, 0.5); // log-normal prior on sigma
  ln_sigma_lat ~ normal(0.0, 0.5); // log-normal prior on sigma
  ln_sigma_year ~ normal(0.0, 0.5); // log-normal prior on sigma
	ln_mu    ~ normal(3, 1);     // log-normal prior for community lengths (median)
	ln_mu_lat    ~ normal(0, 1);     // log-normal prior for community lengths (median)
	ln_mu_year    ~ normal(0, 1);     // log-normal prior for community lengths (median)
	
  for (i in 1:n_observations) { 
    sigma = exp(ln_sigma + ln_sigma_lat*lat[i] + ln_sigma_year*yr[i]);
    ln_mu_new = ln_mu + ln_mu_lat*lat[i] + ln_mu_year*yr[i];
    // For the first size bin only (between 0 and 2.5cm)
    if (sizebin_id[i] == 1) { 
      
       // probability in smallest bin
       prob = lognormal_cdf(upper_sizebin[1], ln_mu_new, sigma); 
       
    // all other bins (>2.5cm)
    } else { 
      // calculates the probability of being less than a bin value
      // then minuses the value for the previous bin
      // equalling the probability of being within a specific bin.
       prob = lognormal_cdf(upper_sizebin[sizebin_id[i]], ln_mu_new, sigma) - 
         lognormal_cdf(upper_sizebin[sizebin_id[i]-1], ln_mu_new, sigma); 
    }
    
    // sum up log-likelihood terms
    // We multiple by the number of individuals as the probability for each of those will be the same, effectively we do not need to sum up the log-likelihood for every single size class, we can just calculate the log-likelihood for a size class once and multiply it by the abundance in that size class
    // this would be the same as having a row per observation (maybe 2.5cm rows) and then summing up the log-liklihood (log(prob) for each of those rows) 
    // we sum up the log-likelhood 
      target += n_individuals[i]*log(prob); 
  }
}


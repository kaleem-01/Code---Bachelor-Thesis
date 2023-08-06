// Simple Model for the same-side hypothesis

data { //known data

   int<lower=0> N; // number of observations in the dataset
   int y[N]; 	// an array containing all the trials
}

parameters { //unknown parameters
  real<lower=0.5, upper=1> theta; // proportion in same side
}

model { //likelihood
  target += beta_lpdf(theta | 382.65, 367.66) - beta_lccdf(0.5 | 382.65, 367.66);  // Truncating the prior 
  for(n in 1:N){
    // a product (on log scale) of the Bernoulli trials
    target += bernoulli_lpmf(y[n] | theta);
  }

}


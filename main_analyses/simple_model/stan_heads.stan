data { //known data

   int<lower=0> N; // number of observations in the dataset
   int y[N]; 	// an array containing all the trials
}

parameters { //unknown parameters
  real<lower=0.5, upper=1> theta;  // proportion of heads in the dataset
}

model { //likelihood
  target += beta_lpdf(theta | 375.82, 375.82) - beta_lccdf(0.50 | 375.82, 375.82);
  for(n in 1:N){
    // a product (on log scale) of the Bernoulli trials
    target += bernoulli_lpmf(y[n] | theta);
    }

}


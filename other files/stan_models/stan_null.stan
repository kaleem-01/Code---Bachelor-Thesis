data { //known data

   int<lower= 0> n; //successes of null
   int<lower=0>  N; // total Null
}

parameters { //unknown parameters
  real<lower=0.5, upper=1> theta_n;  // proportion
}

model { //likelihood
  target += beta_lpdf(theta_n | 375.82, 375.82) - beta_lccdf(0.51 | 375.82, 375.82);
  n ~ binomial(N, theta_n);
}


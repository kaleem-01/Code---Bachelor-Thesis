data { //known data

   int<lower=0> s; // success in same-side
   int<lower=0> S; // total trials
}

parameters { //unknown parameters
  real<lower=0.5, upper=1> theta_s; // proportion in same side
}

model { //likelihood
  target += beta_lpdf(theta_s | 382.65, 367.66) - beta_lccdf(0.51 | 382.65, 367.66);
  s ~ binomial(S, theta_s);
}


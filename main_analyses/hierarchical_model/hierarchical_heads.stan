data {
  int<lower=0> N; // number of observations
  int<lower=0> K; // total number of observations in the second level variable (coins or tossers)
  int<lower=0,upper=1> y[N];       // 1 = success, 0 = failure
  int map_k[N];   // mapping from second level variable to successes
}
parameters {
  real<lower=0.5, upper=1> theta; // probability of success
  real<lower=0> sigma;            // standard deviation of the distribution of the second level parameter (on logistic scale)
  real gamma_k[K];                // difference of each second level parameter from the probability of success (on logistic scale)
}
model {
  // prior distribution on the overall probability of success in the head-tail hypothesis
  target += beta_lpdf(theta | 375.82, 375.82) - beta_lccdf(0.5 | 375.82, 375.82);

  // hierarchical prior distribution on difference of the second level from the probability of success (on logistic scale), truncated so negative values are not possible
  target += normal_lpdf(sigma | 0, 0.01) - normal_lccdf(0 | 0, 0.01);

   // hierarchical prior distribution on difference of the second level  from the probability of success (on logistic scale)
  for(k in 1:K){
    target += normal_lpdf(gamma_k[k] | 0, sigma);
  }
  
  vector [N] sum_log;
  for(n in 1:N){
    // contruct the parameter, logit_mu_k, which is  overall probability of succcess along with the differences from  sigma
    real mu_k = inv_logit(logit(theta) + gamma_k[map_k[n]]);

    sum_log[n] = bernoulli_lpmf(y[n] | mu_k);
    // a product (on log scale) of the Bernoulli trials, including the back transformation
  }
  target += sum(sum_log);
}

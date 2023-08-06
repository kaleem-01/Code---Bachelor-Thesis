# Author: Kaleem Ullah
# Bayes Factor Design Analysis

library(ProbBayes)
# library(apaTables)

library(rempsyc)
source('trunc_dist.R')

# Expected theta values
theta_fair = 0.50
theta_same = 0.50

# Simulated data
sample_size = 134233

# Prior Specifications
fair_priors <- beta.select(list(p = 0.5, x = 0.5),
                           list(p = 0.95, x = 0.53))

same_priors <- beta.select(list(p = 0.5, x = 0.51),
                          list(p = 0.95, x = 0.54))

# Dataframe for bfda

bayes_factor <- c(3,10,30,100)
fair_coin<- data.frame("Bayes Factor" = bayes_factor)
same_side<- data.frame("Bayes Factor" = bayes_factor)


# Initiating parameters 
bf_fair = numeric(0)
bf_same= numeric(0)
simulations = 1000

######################################################################
# effect_size= 0.50
effect_size = 0.50

# BFDA
for (i in 1:simulations) {
  n = sum(rbinom(sample_size, 1, effect_size))
  s = sum(rbinom(sample_size, 1, effect_size))
  N = sample_size
  S = sample_size
  
  # Fair-Coin Hypothesis
  prior_theta_fair <- dtbeta(theta_fair, fair_priors[1], fair_priors[2], lower = 0.5, upper = 1)
  posterior_theta_fair <- dtbeta(theta_fair, fair_priors + n, fair_priors + (N-n), lower = 0.5, upper = 1)
  
  # Same-side Hypothesis
  prior_theta_same <- dtbeta(theta_same, same_priors[1], same_priors[2], lower = 0.5, upper = 1)
  posterior_theta_same <- dtbeta(theta_same, same_priors + s, same_priors + (S-s), lower = 0.5, upper = 1)
  
  # Bayes Factor
  bf_fair[i] = prior_theta_fair/posterior_theta_fair
  bf_same[i]= prior_theta_same/posterior_theta_same
}


j = 1
k = 1

ff_0.50 <- numeric(0)
ss_0.50 <- numeric(0)

# For Loop to calculate proportions
for (i in bayes_factor){
  ff_0.50[j] <- length(bf_fair[bf_fair > i]) / simulations
  ss_0.50[k] <- length(bf_same[bf_same > i]) / simulations
  cat("Fair-Coin", i, '=', length(bf_fair[bf_fair > i]), '\n') 
  cat("Same-Side", i, '=', length(bf_same[bf_same > i]), '\n')
  j = j + 1
  k = k + 1
}


fair_coin$'0.500' <- ff_0.50
same_side$'0.500' <- ss_0.50


bf_fair = numeric(0)
bf_same= numeric(0)



######################################################################
# effect_size= 0.502
effect_size = 0.502

# BFDA
for (i in 1:simulations) {
  n = sum(rbinom(sample_size, 1, effect_size))
  s = sum(rbinom(sample_size, 1, effect_size))
  N = sample_size
  S = sample_size

  # Fair-Coin Hypothesis
  prior_theta_fair <- dtbeta(theta_fair, fair_priors[1], fair_priors[2], lower = 0.5, upper = 1)
  posterior_theta_fair <- dtbeta(theta_fair, fair_priors + n, fair_priors + (N-n), lower = 0.5, upper = 1)
  
  # Same-side Hypothesis
  prior_theta_same <- dtbeta(theta_same, same_priors[1], same_priors[2], lower = 0.5, upper = 1)
  posterior_theta_same <- dtbeta(theta_same, same_priors + s, same_priors + (S-s), lower = 0.5, upper = 1)
  
  # Bayes Factor
  bf_fair[i] = prior_theta_fair/posterior_theta_fair
  bf_same[i]= prior_theta_same/posterior_theta_same
}


j = 1
k = 1

ff_0.502 <- numeric(0)
ss_0.502 <- numeric(0)

# For Loop to calculate proportions
for (i in bayes_factor){
  ff_0.502[j] <- length(bf_fair[bf_fair > i]) / simulations
  ss_0.502[k] <- length(bf_same[bf_same > i]) / simulations
  cat("Fair-Coin", i, '=', length(bf_fair[bf_fair > i]), '\n') 
  cat("Same-Side", i, '=', length(bf_same[bf_same > i]), '\n')
  j = j + 1
  k = k + 1
}

fair_coin$'0.502' <- ff_0.502
same_side$'0.502' <- ss_0.502


bf_fair = numeric(0)
bf_same= numeric(0)

############################################
# Effect size = 0.504

effect_size = 0.504

# BFDA
for (i in 1:simulations) {
  n = sum(rbinom(sample_size, 1, effect_size))
  s = sum(rbinom(sample_size, 1, effect_size))
  N = sample_size
  S = sample_size
  
  # Fair-Coin Hypothesis
  prior_theta_fair <- dtbeta(theta_fair, fair_priors[1], fair_priors[2], lower = 0.5, upper = 1)
  posterior_theta_fair <- dtbeta(theta_fair, fair_priors + n, fair_priors + (N-n), lower = 0.5, upper = 1)
  
  # Same-side Hypothesis
  prior_theta_same <- dtbeta(theta_same, same_priors[1], same_priors[2], lower = 0.5, upper = 1)
  posterior_theta_same <- dtbeta(theta_same, same_priors + s, same_priors + (S-s), lower = 0.5, upper = 1)
  
  # Bayes Factor
  bf_fair[i] = prior_theta_fair/posterior_theta_fair
  bf_same[i]= prior_theta_same/posterior_theta_same
}

j = 1
k = 1
ff_0.504 <- numeric(0)
ss_0.504 <- numeric(0)

# For Loop to calculate proportions
for (i in bayes_factor){
  ff_0.504[j] <- length(bf_fair[bf_fair > i]) / simulations
  ss_0.504[k] <- length(bf_same[bf_same > i]) / simulations
  cat("Fair-Coin", i, '=', length(bf_fair[bf_fair > i]), '\n') 
  cat("Same-Side", i, '=', length(bf_same[bf_same > i]), '\n')
  j = j + 1
  k = k + 1
}

fair_coin$'0.504' <- ff_0.504
same_side$'0.504' <- ss_0.504

bf_fair = numeric(0)
bf_same= numeric(0)

###########################################################
# Effect size = 0.506

effect_size = 0.506

# BFDA
for (i in 1:simulations) {
  n = sum(rbinom(sample_size, 1, effect_size))
  s = sum(rbinom(sample_size, 1, effect_size))
  N = sample_size
  S = sample_size
  
  # Fair-Coin Hypothesis
  prior_theta_fair <- dtbeta(theta_fair, fair_priors[1], fair_priors[2], lower = 0.5, upper = 1)
  posterior_theta_fair <- dtbeta(theta_fair, fair_priors + n, fair_priors + (N-n), lower = 0.5, upper = 1)
  
  # Same-side Hypothesis
  prior_theta_same <- dtbeta(theta_same, same_priors[1], same_priors[2], lower = 0.5, upper = 1)
  posterior_theta_same <- dtbeta(theta_same, same_priors + s, same_priors + (S-s), lower = 0.5, upper = 1)
  
  # Bayes Factor
  bf_fair[i] = prior_theta_fair/posterior_theta_fair
  bf_same[i]= prior_theta_same/posterior_theta_same
}

j = 1
k = 1
ff_0.506 <- numeric(0)
ss_0.506 <- numeric(0)

# For Loop to calculate proportions
for (i in bayes_factor){
  ff_0.506[j] <- length(bf_fair[bf_fair > i]) / simulations
  ss_0.506[k] <- length(bf_same[bf_same > i]) / simulations
  cat("Fair-Coin", i, '=', length(bf_fair[bf_fair > i]), '\n') 
  cat("Same-Side", i, '=', length(bf_same[bf_same > i]), '\n')
  j = j + 1
  k = k + 1
}

fair_coin$'0.506' <- ff_0.506
same_side$'0.506' <- ss_0.506 

bf_fair = numeric(0)
bf_same= numeric(0)
###########################################################
# Effect size = 0.508

effect_size = 0.508

# BFDA
for (i in 1:simulations) {
  n = sum(rbinom(sample_size, 1, effect_size))
  s = sum(rbinom(sample_size, 1, effect_size))
  N = sample_size
  S = sample_size
  
  # Fair-Coin Hypothesis
  prior_theta_fair <- dtbeta(theta_fair, fair_priors[1], fair_priors[2], lower = 0.5, upper = 1)
  posterior_theta_fair <- dtbeta(theta_fair, fair_priors + n, fair_priors + (N-n), lower = 0.5, upper = 1)
  
  # Same-side Hypothesis
  prior_theta_same <- dtbeta(theta_same, same_priors[1], same_priors[2], lower = 0.5, upper = 1)
  posterior_theta_same <- dtbeta(theta_same, same_priors + s, same_priors + (S-s), lower = 0.5, upper = 1)
  
  # Bayes Factor
  bf_fair[i] = prior_theta_fair/posterior_theta_fair
  bf_same[i]= prior_theta_same/posterior_theta_same
}

j = 1
k = 1
ff_0.508 <- numeric(0)
ss_0.508 <- numeric(0)

# For Loop to calculate proportions
for (i in bayes_factor){
  ff_0.508[j] <- length(bf_fair[bf_fair > i]) / simulations
  ss_0.508[k] <- length(bf_same[bf_same > i]) / simulations
  cat("Fair-Coin", i, '=', length(bf_fair[bf_fair > i]), '\n') 
  cat("Same-Side", i, '=', length(bf_same[bf_same > i]), '\n')
  j = j + 1
  k = k + 1
}

fair_coin$'0.508' <- ff_0.508
same_side$'0.508' <- ss_0.508 


bf_fair = numeric(0)
bf_same= numeric(0)
####################################################################
# Effect size = 0.51

effect_size = 0.51

# BFDA
for (i in 1:simulations) {
  n = sum(rbinom(sample_size, 1, effect_size))
  s = sum(rbinom(sample_size, 1, effect_size))
  N = sample_size
  S = sample_size
  
  # Fair-Coin Hypothesis
  prior_theta_fair <- dtbeta(theta_fair, fair_priors[1], fair_priors[2], lower = 0.5, upper = 1)
  posterior_theta_fair <- dtbeta(theta_fair, fair_priors + n, fair_priors + (N-n), lower = 0.5, upper = 1)
  
  # Same-side Hypothesis
  prior_theta_same <- dtbeta(theta_same, same_priors[1], same_priors[2], lower = 0.5, upper = 1)
  posterior_theta_same <- dtbeta(theta_same, same_priors + s, same_priors + (S-s), lower = 0.5, upper = 1)
  
  # Bayes Factor
  bf_fair[i] = prior_theta_fair/posterior_theta_fair
  bf_same[i]= prior_theta_same/posterior_theta_same
}

j = 1
k = 1
ff_0.51 <- numeric(0)
ss_0.51 <- numeric(0)

# For Loop to calculate proportions
for (i in bayes_factor){
  ff_0.51[j] <- length(bf_fair[bf_fair > i]) / simulations
  ss_0.51[k] <- length(bf_same[bf_same > i]) / simulations
  cat("Fair-Coin", i, '=', length(bf_fair[bf_fair > i]), '\n') 
  cat("Same-Side", i, '=', length(bf_same[bf_same > i]), '\n')
  j = j + 1
  k = k + 1
}


fair_coin$'0.510' <- ff_0.51
same_side$"0.510" <- ss_0.51 

nice_table(fair_coin)
nice_table(same_side)
##
# nice_table(
#   data,
#   highlight = FALSE,
#   italics,
#   col.format.p,
#   col.format.r,
#   col.format.ci,
#   format.custom,
#   col.format.custom,
#   width = NULL,
#   broom = NULL,
#   report = NULL,
#   short = FALSE,
#   title,
#   note,
#   separate.header
# )
# 


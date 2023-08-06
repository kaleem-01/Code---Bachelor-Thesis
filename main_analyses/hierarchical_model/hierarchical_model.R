# Author: Kaleem Ullah
# This file contains the model with coin effects

# Import libraries
library(rstan)
library(bridgesampling)

# Set Rstan options and files
rstan_options(auto_write=TRUE)
options(mc.cores= parallel::detectCores())


#################################################################
# Hierarchical Model with Tosser Effects

modelFile_heads <- 'hierarchical_heads.stan'
modelFile_same <- 'hierarchical_same.stan'
nIter <- 10000
nChains <- 4
nWarmup <- 1000


# Read Data
df = read.csv("all_trials.csv")

# Data Lists for both hypotheses

headsList_tosser <- list(
  y     = df$heads,
  map_k = df$person,
  N     = nrow(df),
  K     = length(unique(df$person))
)

sameList_tosser <- list(
  y     = df$same_side,
  map_k = df$person,
  N     = nrow(df),
  K     = length(unique(df$person))
)

# Stan Fit Heads-Tails
fit_heads_tosser <- stan(modelFile_heads,
                         data = headsList_tosser,
                         chains = nChains,
                         warmup = nWarmup,
                         iter = nIter,
                         init = "random",
                         seed = 5050)
                

# Stan Fit Same-Side
fit_same_tosser <- stan(modelFile_same,
                        data = sameList_tosser,
                        chains = nChains,
                        warmup = nWarmup,
                        iter = nIter,
                        init = "random",
                        seed = 5050)

# Marginal Likelihoods generated using bridge sampler
marginal_heads_tosser = bridge_sampler(fit_heads_tosser)
marginal_same_tosser = bridge_sampler(fit_same_tosser)


##############################################################################
# Hierarchical Model with Coin Effects

# Setting up the model
coinFile_heads <- 'hierarchical_heads.stan'
coinFile_same <- 'hierarchical_same.stan'


# Data Lists for both hypotheses
sameList_coin <- list(
  y     = df$same_side,
  map_k = df$coin,
  N     = nrow(df),
  K     = length(unique(df$coin))
)

headsList_coin <- list(
  y     = df$heads,
  map_k = df$coin,
  N     = nrow(df),
  K     = length(unique(df$coin))
)

# Stan Fit Heads-Tails
fit_heads_coin <- stan(coinFile_heads,
                  data = headsList_coin,
                  chains = nChains,
                  iter = nIter,
                  warmup = nWarmup,
                  init = "random",
                  seed = 5050)


# Stan Fit Same-Side
fit_same_coin <- stan(coinFile_same,
                 data = sameList_coin,
                 chains = nChains,
                 iter = nIter,
                 warmup = nWarmup,
                 init = "random",
                 seed = 5050)

# Marginal Likelihoods generated using bridge sampler
marginal_heads_coin = bridge_sampler(fit_heads_coin)
marginal_same_coin = bridge_sampler(fit_same_coin)

#################################################################################



# Bayes Factors for both hypotheses
# bf_heads = exp(marginal_heads$logml - log_marginal_heads)
# bf_same = exp(marginal_same$logml - log_marginal_same)

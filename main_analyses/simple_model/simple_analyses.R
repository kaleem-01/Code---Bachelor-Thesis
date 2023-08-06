# Author: Kaleem Ullah
# This file contains the main simple analyses without hierarchical modelling

# Import libraries
library(rstan)
library(bridgesampling)

# Set rstan options and files
rstan_options(auto_write=TRUE)
options(mc.cores=4)

modelFile_heads <- 'stan_heads.stan'
modelFile_same <- 'stan_same.stan'
nIter <- 10000
nChains <- 4
nWarmup <- 2000
nThin <- 1


# Loading data for simple model
df_simple = read.csv("all_trials.csv")

# Specifying data for the "Heads-Tails" and the "Same-Side" Hypotheses
dataList_heads <- list(
  y = df_simple$heads,
  N = nrow(df_simple))

dataList_same <- list(
  y = df_simple$same_side,
  N = nrow(df_simple))

# Log marginal likelihood under point null
log_marginal_heads <- sum(dbinom(x = df_simple$heads, size = 1, prob = 0.5, log = TRUE))
log_marginal_same <- sum(dbinom(x = df_simple$same, size = 1, prob = 0.5, log = TRUE))

# Stan Fit Heads-Tails
fit_heads_simple <- stan(modelFile_heads,
                data = dataList_heads,
                chains = nChains,
                nWarmup = nWarmup,
                iter = nIter,
                thin = nThin,
                init = "random",
                seed = 5050)

# Stan Fit Same-Side
fit_same_simple <- stan(modelFile_same,
                 data = dataList_same,
                 chains = nChains,
                 nWarmup = nWarmup,
                 iter = nIter,
                 thin = nThin,
                 init = "random",
                 seed = 5050)

# Marginal Likelihoods generated using bridge sampler
marginal_heads_simple = bridge_sampler(fit_heads_simple)
marginal_same_simple = bridge_sampler(fit_same_simple)

setwd("C:/Users/kalee/Desktop/Bachelor's Thesis/R Programming/main_analyses/simple_model/data.RData")

# Bayes Factors for both hypotheses
bf_heads_simple = exp(marginal_heads_simple$logml - log_marginal_heads)
bf_same_simple = exp(marginal_same_simple$logml - log_marginal_same)















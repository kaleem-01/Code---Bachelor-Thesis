# Author: Kaleem Ullah
# Simulated Data 

library(rstan)
library(bridgesampling)
rstan_options(auto_write=TRUE)
options(mc.cores=4)

modelFile_null <- 'stan_null.stan'
modelFile_alt <- 'stan_alt.stan'
nIter <- 2000
nChains <- 4
nWarmup <- floor(nIter/2)
nThin <- 1

sample_size = 100000


cat("Estimating", modelFile, "model...\n")
startTime = Sys.time(); print(startTime)
cat("Calling", nChains, "simulation in Stan...\n")

n = sum(rbinom(sample_size, 1, 0.5))
N = sample_size
s = sum(rbinom(sample_size, 1, 0.51))
S = sample_size

dataList_null <- list(n=n, N=N)
dataList_alt <- list(s=s, S=S)
'
# Stan Fit Null
fit_null <- stan(modelFile_null,
                 data = dataList_null,
                 chains = nChains,
                 iter = nIter,
                 thin = nThin,
                 init = "random",
                 seed = 12560081)

fit_alt <- stan(modelFile_alt,
                data = dataList_alt,
                chains = nChains,
                iter = nIter,
                thin = nThin,
                init = "random",
                seed = 12560081)

cat("Finishing model simulation")
endTime = Sys.time(); print(endTime)
print("It took", endTime - startTime)
'


model_1   <- stan_model(modelFile_null)
fit_1   <- sampling(object = model_1, data = dataList_null, control = list(adapt_delta = 0.99))
marglik_1 <- bridge_sampler(fit_1)


model_2   <- stan_model(modelFile_alt)
fit_2     <- sampling(object = model_2, data = dataList_alt, control = list(adapt_delta = 0.99))
marglik_2 <- bridge_sampler(fit_2)


'
bridge_H0 <- bridge_sampler(fit_null)
bridge_H1 <- bridge_sampler(fit_alt)

print(bf(bridge_H0, bridge_H1))
'
cat('bayes factor', exp(marglik_2$logml - marglik_1$logml))

    
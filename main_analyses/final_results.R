library(bridgesampling)


# Bayes Factor Simple
bf_heads_simple = exp(marginal_heads_simple$logml - log_marginal_heads)
bf_same_simple = exp(marginal_same_simple$logml - log_marginal_same)
print(bf_simple_heads)
print(bf_simple_same)

# Bayes Factor Tosser
bf_tosser_heads = bf(marginal_heads_tosser, marginal_same_simple)
bf_tosser_same = bf(marginal_same_tosser, marginal_same_simple)

bf_tosser_heads <- bf_tosser_heads$bf
bf_tosser_same <- bf_tosser_same$bf

# Bayes Factor Coin
bf_coin_heads = bf(marginal_heads_coin, marginal_heads_simple)
bf_coin_same = bf(marginal_same_coin, marginal_same_simple)

bf_coin_heads <- bf_coin_heads$bf
bf_coin_same <- bf_coin_same$bf

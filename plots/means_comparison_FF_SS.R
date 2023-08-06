library(ggplot2)


# Function to transform fair-coin dataset to same-side dataset
FC_to_SS = function(dataset, sample_size) {
  data_temp = append(1, dataset) # Assuming heads up in the first trial
  SS_dataset = ifelse(dataset == data_temp[-(sample_size+1)], 1, 0) # Remove the last item in comparison
  return(SS_dataset)
}



# Simulating fair-coin means by changing same-side parameter
sample_size = 1000000
mean_data = numeric(0)
mean_same_side = numeric(0)
k = 1

for (i in seq(0,1,0.001)) {
  data = numeric(0)
  data = append(data, 1)
  for (j in 1:sample_size){   
    if (data[j] == 1) {
      data[j+1] = rbinom(1, 1, i) # If the 
    } else {
      data[j+1] = rbinom(1, 1, (1-i))
    }
  }
    
  mean_data[k] = mean(data)
  mean_same_side[k] = mean(FC_to_SS(data, sample_size = sample_size))
  k = k + 1
}
x = seq(0,1,0.001)

# Data frame with simulation data
comparison = data.frame(x, mean_data, mean_same_side)
                        
# Comparison plot
comparison_plot = ggplot(comparison) + geom_line(aes(x = x, y = mean_same_side, color = "P(Same-Side)")) +
  geom_line(aes(x = x, y = mean_data, color = "P(Heads)")) + 
  labs(x = "Same-Side Bias",
       y = "Probability") + 
  scale_color_manual(values = c("P(Same-Side)" = "blue", "P(Fair-Coin)" = "red")) + 
  theme(legend.position = 'bottom')


comparison_plot + jtools::theme_apa()


# Plot with mean(data)
mean_data_plot =  ggplot(comparison) + geom_line(aes(x = x, y = mean_data), color = "red") + 
  labs(x = "P(Same-Side)",
       y = "P(Heads)") + 
  ylim(0, 1)
  
mean_data_plot + jtools::theme_apa()




################################################################

# For loop that collects data
data = numeric(0)
data = append(data, 1)
data = 1
sample_size = 1000000
i = 0.95
for (j in 1:sample_size){   
    if (data[j] == 1) {
      data[j+1] = rbinom(1, 1, i) # If the 
    } else {
      data[j+1] = rbinom(1, 1, (1-i))
    }
  }

mean(data)
x = seq(0,1,0.001)






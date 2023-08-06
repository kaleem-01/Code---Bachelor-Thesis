library(ggplot2)
library(jtools)
library(dplyr)

# Function to transform fair-coin dataset to same-side dataset
FC_to_SS = function(dataset, sample_size) {
  data_temp = append(1, dataset) # Assuming heads up in the first trial
  SS_dataset = ifelse(dataset == data_temp[-(sample_size+1)], 1, 0) # Remove the last item in comparison
  return(SS_dataset)
}

# with the mean 
sample_size = 1000000 
mean = numeric(0)
j = 1
for (i in seq(0.45, 0.55, 0.001)){
  data = rbinom(sample_size, 1, i)
  data_same = FC_to_SS(data, sample_size)
  
  mean[j] = (sum(data_same))/sample_size
  j = j + 1
}
x = seq(0.45, 0.55, 0.001)

# Data.frame Mean
mean.data = data.frame(x, mean)

# Plot
plot_mean <- ggplot(mean.data, mapping = aes(x, mean)) + 
  geom_point() + 
  labs(x= "P(Heads)", 
       y= "P(Same-Side)")

plot_mean + theme_apa()

# # Subsetting data from mean_data
# lower_b = 0.40
# upper_b = 0.60
# subsetted_data = filter(mean_data, x > lower_b & x < upper_b)
# 
# mean[551]
# 
# 
# # Fitting a polynomial model
# x_2 <- x^2
# quad_fit <- lm(mean ~ x + x_2, data = mean.data)
# summary(quad_fit)
# 
# 
# predictions = predict(quad_fit)
# 
# mean.data["predictions"] = predictions
# mean.data["diff"] = mean.data$mean - mean.data$predictions
# 
# df_with_pred <- cbind(predictions)
# 
# # Conversion using quad_fit
# convert_ss <- function(x) {
#   y = 2*(x^2) - 2*(x) + 1
#   return (y)
# }
# convert_ss(0.55)

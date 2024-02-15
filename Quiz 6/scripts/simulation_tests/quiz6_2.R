##SIMULATION

set.seed(123) # Ensure reproducibility

# Number of days in a year
days <- 1:365

# Simulate daily snowfall data for three cities
city1 <- runif(365, min = 0, max = 10) # City 1
city2 <- runif(365, min = 0, max = 15) # City 2
city3 <- runif(365, min = 0, max = 8)  # City 3

# Combine into a data frame
snow_data <- data.frame(day = days, city1 = city1, city2 = city2, city3 = city3)


##TESTS

#Basic Descriptive Stats
# Calculate mean, median, and standard deviation for each city
stats_city1 <- with(snow_data, c(mean = mean(city1), median = median(city1), sd = sd(city1)))
stats_city2 <- with(snow_data, c(mean = mean(city2), median = median(city2), sd = sd(city2)))
stats_city3 <- with(snow_data, c(mean = mean(city3), median = median(city3), sd = sd(city3)))

# Print results
print(stats_city1)
print(stats_city2)
print(stats_city3)


#Boostrap Sampling Test

set.seed(123) # Ensure reproducibility

# Function for bootstrap sampling test
bootstrap_test <- function(data1, data2, n_bootstrap = 1000) {
  diff_means <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample1 <- sample(data1, replace = TRUE)
    sample2 <- sample(data2, replace = TRUE)
    diff_means[i] <- mean(sample1) - mean(sample2)
  }
  
  # Calculate observed difference
  observed_diff <- mean(data1) - mean(data2)
  
  # Plot distribution of bootstrapped differences
  hist(diff_means, breaks = 30, main = "Bootstrap Distribution of Difference in Means", xlab = "Difference in Means")
  abline(v = observed_diff, col = "red", lwd = 2)
  
  # P-value calculation
  p_value <- mean(abs(diff_means) >= abs(observed_diff))
  
  return(list(p_value = p_value, diff_means = diff_means))
}

# Apply bootstrap test between City 1 and City 2
result <- bootstrap_test(snow_data$city1, snow_data$city2)

# Print p-value
print(result$p_value)


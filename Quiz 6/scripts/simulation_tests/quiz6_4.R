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


##GRAPH

#Clean Data

library(dplyr)
library(ggplot2)
library(tidyr)

# Add a month column to the data
snow_data$month <- ceiling(snow_data$day / 30.44) # Approximate conversion assuming 365 days in a year

# Aggregate snowfall data by month
monthly_snowfall <- snow_data %>%
  group_by(month) %>%
  summarise(city1 = sum(city1), city2 = sum(city2), city3 = sum(city3))

# Convert from wide to long format
monthly_snowfall_long <- pivot_longer(monthly_snowfall, cols = c(city1, city2, city3), names_to = "city", values_to = "snowfall")

# Ensure month is ordered correctly if not already
monthly_snowfall_long$month <- factor(monthly_snowfall_long$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#Draw Graph

ggplot(monthly_snowfall, aes(x = month)) +
  geom_line(aes(y = city1, color = "City 1")) +
  geom_line(aes(y = city2, color = "City 2")) +
  geom_line(aes(y = city3, color = "City 3")) +
  labs(title = "Monthly Snowfall in Three Cities", x = "Month", y = "Total Snowfall (inches)") +
  theme_minimal() +
  scale_color_manual(values = c("City 1" = "blue", "City 2" = "red", "City 3" = "green"))


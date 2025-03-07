#!/usr/bin/env Rscript

library(ggplot2)
library(dplyr)
library(ggcorrplot)

# Load the dataset
df <- read.csv("coffee_shop_revenue.csv")

# Select relevant numeric columns for correlation analysis
df_numeric <- df %>% select_if(is.numeric)

# Compute correlation matrix
cor_matrix <- cor(df_numeric, use = "complete.obs")

# Plot the correlation matrix
corr_plot <- ggcorrplot(cor_matrix, method = "circle", type = "lower", lab = TRUE,
                         title = "Correlation Matrix of Coffee Shop Data")

# Display the plot
print(corr_plot)

# Perform linear regression between two significant variables
lm_model <- lm(Daily_Revenue ~ Number_of_Customers_Per_Day, data = df)
summary(lm_model)

lm_model_order_value <- lm(Daily_Revenue ~ Average_Order_Value , data = df)
summary(lm_model_order_value)

lm_customer_foot_traffic <- lm(Number_of_Customers_Per_Day ~ Location_Foot_Traffic, data = df)
summary(lm_customer_foot_traffic)

# Plot regression line for number of customers vs daily revenue
ggplot(df, aes(x = Number_of_Customers_Per_Day, y = Daily_Revenue)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression: Customers vs Revenue",
       x = "Number of Customers Per Day",
       y = "Daily Revenue")

# Plot regression line for average order value vs daily revenue
ggplot(df, aes(x = Average_Order_Value, y = Daily_Revenue)) +
  geom_point() + 
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression: Order Value vs Revenue", 
       x = "Average Order Value",
       y = "Daily Revenue")


# Perform t-test for Number of Customers (high vs low) on Daily Revenue
df$Customers_High_Low <- ifelse(df$Number_of_Customers_Per_Day > median(df$Number_of_Customers_Per_Day), "High", "Low")
t_test_result <- t.test(Daily_Revenue ~ Customers_High_Low, data = df)
t_test_result

# Perform t-test for average order value (high vs low) on daily revenue
df$Order_High_Low <- ifelse(df$Average_Order_Value > median(df$Average_Order_Value), "High", "Low")
t_test_result_orders <- t.test(Daily_Revenue ~ Order_High_Low, data = df)
t_test_result_orders 

ggplot(df, aes(x = Number_of_Customers_Per_Day, y = Location_Foot_Traffic)) + geom_point() + geom_smooth(method="lm", col="blue") +
  labs(title="Linear Regression: Number of Customers vs. Location Foot Traffic")

ggplot(df, aes(x = Daily_Revenue, fill = Customers_High_Low)) +
  geom_density(alpha = 0.5) +
  labs(title = "Revenue Distribution: High vs. Low Customer Count",
       x = "Daily Revenue",
       y = "Density")

ggplot(df, aes(x = Daily_Revenue, fill = Order_High_Low)) +
geom_density(alpha = 0.5) +
labs(title = "Revenue Distribution: High vs. Low Order Value",
      x = "Daily Revenue",
      y = "Density")

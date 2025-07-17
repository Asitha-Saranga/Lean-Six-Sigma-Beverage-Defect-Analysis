# Load necessary libraries
library(dplyr)
library(ggplot2)
library(qcc)

# Read dataset
df <- read.csv(file.choose(),header = T)

# Create a time identifier
df$DATE <- paste(df$YEAR, sprintf("%02d", df$MONTH), sep = "-")

# Group by Date and summarize total Warehouse Sales
monthly_sales <- df %>%
  group_by(DATE) %>%
  summarise(Total_Warehouse_Sales = sum(WAREHOUSE.SALES, na.rm = TRUE))

# Print basic stats
summary(monthly_sales$Total_Warehouse_Sales)

# Plot Warehouse Sales over time
ggplot(monthly_sales, aes(x = as.Date(paste0(DATE, "-01")), y = Total_Warehouse_Sales)) +
  geom_line() +
  labs(title = "Monthly Warehouse Sales Trend", x = "Date", y = "Total Warehouse Sales") +
  theme_minimal()

# Create X-bar and R chart
# Create subgroups of size 5 (you can adjust this depending on count)
subgroups <- matrix(monthly_sales$Total_Warehouse_Sales, ncol = 5, byrow = TRUE)

# X̄ and R Chart
qcc(subgroups, type = "xbar", title = "X-bar Chart for Warehouse Sales")
qcc(subgroups, type = "R", title = "R Chart for Warehouse Sales")

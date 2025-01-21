
# Loading necessary libraries
install.packages("dplyr")
library(dplyr)

data_2016 <- read.csv("C:/Users/Huawei/Documents/2016_Financial_Data.csv", stringsAsFactors = FALSE)
data_2017 <- read.csv("C:/Users/Huawei/Documents/2017_Financial_Data.csv", stringsAsFactors = FALSE)
data_2018 <- read.csv("C:/Users/Huawei/Documents/2018_Financial_Data.csv", stringsAsFactors = FALSE)


# Checking column names are identical or not
identical(names(data_2016), names(data_2017)) # TRUE or FALSE
identical(names(data_2017), names(data_2018)) # TRUE or FALSE

colnames(data_2016)[colnames(data_2016) == "X2017.PRICE.VAR...."] <- "PRICE_VAR"
colnames(data_2017)[colnames(data_2017) == "X2018.PRICE.VAR...."] <- "PRICE_VAR"
colnames(data_2018)[colnames(data_2018) == "X2019.PRICE.VAR...."] <- "PRICE_VAR"

# Displaying the first few rows of the dataset
head(data_2016)
head(data_2017)
head(data_2018)

# Removing first column from each dataset
data_2016 <- data_2016[, -1]
data_2017 <- data_2017[, -1]
data_2018 <- data_2018[, -1]

# Checking if the first column was removed
head(data_2016)

# Counting missing values for each column in dataset
colSums(is.na(data_2016)) 

# Check for missing values in data_2016
sum(is.na(data_2016))  # Total number of missing values

# Function to identify column types
get_column_types <- function(df) {
  sapply(df, function(col) {
    if (is.numeric(col)) {
      return("numeric")
    } else if (is.factor(col) || is.character(col)) {
      return("categorical")
    } else {
      return("other")
    }
  })
}

# Applying the function to the dataframe
column_types <- get_column_types(data_2016)

# Displaying the column types
print(column_types)

# Counting the number of each type
type_counts <- table(column_types)

# Displaying the column types and their counts
print(type_counts)

# Convert only categoric column to numeric one
data_2016$Sector <- as.numeric(as.factor(data_2016$Sector))
data_2017$Sector <- as.numeric(as.factor(data_2017$Sector))
data_2018$Sector <- as.numeric(as.factor(data_2018$Sector))

# Function to fill missing values in numeric columns with the median
impute_median <- function(df) {
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      # Replace missing values with median
      df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
    }
  }
  return(df)
}

# Applying the function to each dataset
data_2016 <- impute_median(data_2016)
data_2017 <- impute_median(data_2017)
data_2018 <- impute_median(data_2018)

# Checking for remaining missing values
colSums(is.na(data_2016))  
colSums(is.na(data_2017))  
colSums(is.na(data_2018)) 

# Function to detect outliers using IQR
detect_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(which(column < lower_bound | column > upper_bound))
}


# Cap outliers to the nearest valid value
cap_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  column[column < lower_bound] <- lower_bound
  column[column > upper_bound] <- upper_bound
  return(column)
}

# Example: Cap outliers for Revenue
data_2016$Revenue <- cap_outliers(data_2016$Revenue)

# Check summary statistics to ensure outliers are capped
summary(data_2016)
summary(data_2017)
summary(data_2018)

# Visualize the distribution of a numeric column after handling outliers
boxplot(data_2016$Revenue, main = "Boxplot of Revenue (2016 - After Outlier Handling)")
boxplot(data_2017$Revenue, main = "Boxplot of Revenue (2017 - After Outlier Handling)")
boxplot(data_2018$Revenue, main = "Boxplot of Revenue (2018 - After Outlier Handling)")

# Combining all datasets into a single dataframe
combined_data <- bind_rows(data_2016, data_2017, data_2018)

# Checking structure of the combined dataset
str(combined_data)

# Preview the combined dataset
head(combined_data)

# Check column names
colnames(combined_data)

# Check data types
str(combined_data)

# Count missing values in each column
colSums(is.na(combined_data))


#Feature Engineering for Logistic Regression

library(caret)  # For feature importance 
# Load necessary libraries
library(Information)

# Calculate Information Value (IV) for all variables
iv_summary <- create_infotables(data = combined_data, y = "Class", bins = 10, parallel = FALSE)

# Display IV summary
print(iv_summary$Summary)

# Filter variables with 0.1 ≤ IV ≤ 0.5
selected_vars <- iv_summary$Summary$Variable[iv_summary$Summary$IV >= 0.1 & iv_summary$Summary$IV <= 0.5]

# Include the Class variable in the final dataset
final_model_data <- combined_data[, c(selected_vars, "Class")]

# Display selected variables
cat("Selected Variables Based on IV (0.1 ≤ IV ≤ 0.5):\n")
print(selected_vars)

# Calculate Correlation Matrix for the dataset
correlation_matrix <- cor(final_model_data, use = "complete.obs")

# Identify Highly Correlated Pairs (|correlation| > 0.8)
high_corr <- which(abs(correlation_matrix) > 0.8, arr.ind = TRUE)

# Remove self-correlations
high_corr <- high_corr[high_corr[, 1] != high_corr[, 2], ]

# Create a data frame of correlated pairs
correlated_pairs <- data.frame(
  Variable1 = rownames(correlation_matrix)[high_corr[, 1]],
  Variable2 = colnames(correlation_matrix)[high_corr[, 2]],
  Correlation = correlation_matrix[high_corr]
)

print(correlated_pairs)

# Remove variable with the least Information Value (IV) in each correlated pair
variables_to_remove <- c()

for (i in seq_len(nrow(correlated_pairs))) {
  var1 <- correlated_pairs$Variable1[i]
  var2 <- correlated_pairs$Variable2[i]
  
  iv_var1 <- iv_summary$Summary$IV[iv_summary$Summary$Variable == var1]
  iv_var2 <- iv_summary$Summary$IV[iv_summary$Summary$Variable == var2]
  
  if (iv_var1 < iv_var2) {
    variables_to_remove <- c(variables_to_remove, var1)
  } else {
    variables_to_remove <- c(variables_to_remove, var2)
  }
}

# Remove duplicate entries
variables_to_remove <- unique(variables_to_remove)

# Final dataset after handling correlations
final_cleaned_data <- final_model_data[, !names(final_model_data) %in% variables_to_remove]

# Output results
cat("\nVariables removed due to multicollinearity (least IV):\n")
print(variables_to_remove)

cat("\nFinal dataset summary:\n")
print(summary(final_cleaned_data))


# Modelling

set.seed(123)  # For reproducibility

# Class variable is a factor
final_cleaned_data$Class <- as.factor(final_cleaned_data$Class)

# Split the data into training and testing sets (70% train, 30% test)
train_indices <- sample(seq_len(nrow(final_cleaned_data)), size = 0.7 * nrow(final_cleaned_data))
train_data <- final_cleaned_data[train_indices, ]
test_data <- final_cleaned_data[-train_indices, ]

# Building the Logistic Regression Model
logistic_model <- glm(Class ~ ., data = train_data, family = binomial)

# Summary of the model
cat("\nLogistic Regression Model Summary:\n")
print(summary(logistic_model))

# Predictions and Evaluation
predicted_probs <- predict(logistic_model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)  # Threshold = 0.5
predicted_classes <- as.factor(predicted_classes)


cat("\nConfusion Matrix:\n")
confusion <- confusionMatrix(predicted_classes, test_data$Class)
print(confusion)

# Calculate and Plot AUC-ROC Curve
library(pROC)
roc_curve <- roc(test_data$Class, predicted_probs)

cat("\nAUC Value:\n")
print(auc(roc_curve))

# Plot ROC Curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)











# Load necessary libraries
library(xgboost)
library(caret)
library(pROC)

# Convert the target variable to numeric for xgboost
final_cleaned_data$Class <- as.numeric(as.factor(final_cleaned_data$Class)) - 1

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(final_cleaned_data), 0.7 * nrow(final_cleaned_data))
train_data <- final_cleaned_data[train_indices, ]
test_data <- final_cleaned_data[-train_indices, ]

# Separate features and target variable
train_matrix <- as.matrix(train_data[, !colnames(train_data) %in% "Class"])
train_labels <- train_data$Class
test_matrix <- as.matrix(test_data[, !colnames(test_data) %in% "Class"])
test_labels <- test_data$Class

# Create DMatrix for xgboost
train_dmatrix <- xgb.DMatrix(data = train_matrix, label = train_labels)
test_dmatrix <- xgb.DMatrix(data = test_matrix, label = test_labels)

# Set xgboost parameters
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.3,
  nthread = 2
)

# Train the xgboost model
xgb_model <- xgb.train(
  params = params,
  data = train_dmatrix,
  nrounds = 100,
  watchlist = list(train = train_dmatrix, test = test_dmatrix),
  verbose = 1
)

# Predict probabilities on the test data
predicted_probs <- predict(xgb_model, newdata = test_matrix)

# Convert probabilities to binary predictions
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Calculate accuracy
accuracy <- sum(predicted_classes == test_labels) / length(test_labels)
cat("Accuracy of XGBoost Model:", round(accuracy * 100, 2), "%\n")

# Generate the ROC curve
roc_curve <- roc(test_labels, predicted_probs)

# Load necessary library
library(pROC)

# Plot the ROC curve with improved aesthetics
plot(roc_curve, 
     main = "ROC Curve for XGBoost Model", 
     col = "red", 
     lwd = 3,               # Make the line thicker
     cex.main = 1.5,        # Increase the title size
     cex.axis = 1.2,        # Increase axis label size
     cex.lab = 1.3,         # Increase axis title size
     font.main = 2)         # Make the title bold

# Calculate AUC value
auc_value <- auc(roc_curve)

# Add AUC value as a legend with bold and larger text
legend("bottomright", 
       legend = paste("AUC =", round(auc_value, 2)), 
       col = "red", 
       lwd = 3, 
       cex = 1.3,         # Increase legend text size
       text.font = 2,     # Make text bold
       bty = "n")         # Remove box around the legend





actual_classes <- test_data$Class  # Replace 'Class' with your target column
predicted_classes <- predicted_classes  # Replace with your predicted class variable





#Sector-wise average Debt ratio 
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Define sector name mapping
sector_mapping <- data.frame(
  Sector = as.character(1:11),  # Ensure sector codes are characters
  SectorName = c(
    "Technology", "Healthcare", "Finance", "Energy", "Consumer Goods",
    "Industrial", "Utilities", "Communication", "Real Estate",
    "Basic Materials", "Consumer Services"
  )
)

# Convert Sector column to character for merging
combined_data$Sector <- as.character(combined_data$Sector)

# Merge with mapping
combined_data <- left_join(combined_data, sector_mapping, by = "Sector")


# Group data by sector and calculate the average debt ratio
sector_debt_ratio <- combined_data %>%
  group_by(SectorName) %>%
  summarise(AverageDebtRatio = mean(debtRatio, na.rm = TRUE))

# visualization
ggplot(sector_debt_ratio, aes(x = reorder(SectorName, -AverageDebtRatio), 
                              y = AverageDebtRatio, 
                              fill = AverageDebtRatio)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = round(AverageDebtRatio, 2)), 
            vjust = -0.5, size = 4.5, fontface = "bold") + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Sector-wise Average Debt Ratio", 
       x = "Sector", 
       y = "Average Debt Ratio (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))






#Efficient Frontier: Risk vs. Return (Scatter Plot)

# Calculate Expected Return & Risk (Standard Deviation) per Sector
risk_return_data <- combined_data %>%
  group_by(SectorName) %>%
  summarise(
    Expected_Return = mean(PRICE_VAR, na.rm = TRUE),  # Average return
    Risk = sd(PRICE_VAR, na.rm = TRUE)  # Standard deviation (volatility)
  ) %>%
  arrange(Risk)  # Sort for better visualization


# Efficient Frontier Visualization
ggplot(risk_return_data, aes(x = Risk, y = Expected_Return, color = SectorName)) +
  geom_point(size = 5, alpha = 0.9) +  # Increase point size for better visibility
  geom_smooth(method = "loess", color = "black", linetype = "dashed", fill = "gray", alpha = 0.15) +  # Smoother curve
  labs(title = "Efficient Frontier: Risk vs Return",
       x = "Risk (Standard Deviation)",
       y = "Expected Return") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "right",
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "gray90")
  ) +
  guides(color = guide_legend(override.aes = list(size = 5)))  # Increase legend point sizes



#Profitability Trends by Sector Visualization without outliers

# Select profitability metrics
profit_data <- combined_data %>%
  group_by(SectorName) %>%
  summarise(
    Profit.Margin = mean(Profit.Margin, na.rm = TRUE),
    EBITDA.Margin = mean(EBITDA.Margin, na.rm = TRUE),
    ROE = mean(returnOnEquity, na.rm = TRUE)
  ) %>%
  melt(id.vars = "SectorName")  # Convert to long format

# Apply log transformation to prevent extreme scaling
profit_data$value <- log10(abs(profit_data$value) + 1)

# Create a refined grouped bar chart
ggplot(profit_data, aes(x = reorder(SectorName, -value), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  
  labs(
    title = "Profitability Trends by Sector",
    x = "Sector",
    y = "Log-Scaled Percentage (%)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Profit.Margin" = "#E74C3C", "EBITDA.Margin" = "#3498DB", "ROE" = "#2ECC71")) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1, size = 12, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_blank()
  )



#Debt-to-Equity Ratio vs. Interest Coverage Visualization 

# Load required libraries
library(ggplot2)
library(dplyr)

# Filter and clean data (remove extreme outliers)
filtered_data <- combined_data %>%
  filter(debtEquityRatio > 0 & debtEquityRatio < 200,   # Remove extreme debt-to-equity outliers
         interestCoverage > -50 & interestCoverage < 500)  # Remove extreme interest coverage values

# Create scatter plot for solvency analysis
ggplot(filtered_data, aes(x = debtEquityRatio, y = interestCoverage, size = Market.Cap, color = SectorName)) +
  geom_point(alpha = 0.7) +  # Use transparency to avoid overplotting
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +  # Add trend line
  scale_x_log10() +  # Log scale to handle large variations
  scale_y_log10() +  # Log scale for better readability
  labs(
    title = "Debt-to-Equity Ratio vs. Interest Coverage by Sector",
    subtitle = "Higher leverage with low interest coverage may indicate financial risk",
    x = "Debt-to-Equity Ratio (log scale)",
    y = "Interest Coverage Ratio (log scale)",
    size = "Market Cap",
    color = "Sector"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )



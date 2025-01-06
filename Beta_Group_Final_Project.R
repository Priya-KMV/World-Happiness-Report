# STEP 1: Install Required Packages
# The following commands install necessary packages for data analysis and visualization.
install.packages('dplyr')        # For data manipulation
install.packages('ggplot2')      # For creating visualizations
install.packages('summarytools') # For generating descriptive statistics summaries
install.packages('corrplot')     # For creating correlation heatmaps

# STEP 2: Load Libraries
# Load the installed libraries to use their functions in the analysis.
library(dplyr)        # Data manipulation functions like mutate() and select()
library(ggplot2)      # Data visualization for plots and charts
library(summarytools) # Generate detailed descriptive statistics summaries
library(corrplot)     # Create visually appealing correlation heatmaps

# Set locale to handle non-ASCII characters correctly
Sys.setlocale("LC_ALL", "English_United States.UTF-8")

# STEP 3: Load the Dataset
# Use the `file.choose()` function to manually select the dataset file from your computer.
data <- read.csv(file.choose(), header = TRUE, fileEncoding = "latin1")

# STEP 4: Explore the Dataset
str(data)      # Displays the structure of the dataset (e.g., column names, data types)
summary(data)  # Provides basic descriptive statistics for numeric columns
head(data)     # Displays the first 6 rows of the dataset for a quick preview

# STEP 5: Check for Missing Values
missing_summary <- sapply(data, function(x) sum(is.na(x))) # Count missing values for each column
print(missing_summary) # Print the count of missing values in each column

# STEP 6: Handle Missing Values (If Any)
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# STEP 7: Generate Descriptive Statistics
descriptive_stats <- dfSummary(data)
print(descriptive_stats) # Display the descriptive statistics summary in the console

# Check column names
colnames(data)  # Check the actual column names

# STEP 8: Visualize the Distribution of the Life Ladder Scores
ggplot(data, aes(x = Life.Ladder)) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(
    title = "Distribution of Life Ladder Scores",
    x = "Life Ladder Score",
    y = "Frequency"
  ) +
  theme_minimal()

# STEP 9: Explore Relationships Between Variables Using a Correlation Heatmap
numeric_cols <- data %>%
  select(where(is.numeric))

cor_matrix <- cor(numeric_cols, use = "complete.obs")
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.7)

# STEP 10: Preliminary Analysis with Linear Regression
model <- lm(Life.Ladder ~ Log.GDP.per.capita + Social.support, data = data)
summary(model)

# STEP 11: Visualize the Relationship Between GDP per Capita and Happiness Score
ggplot(data, aes(x = Log.GDP.per.capita, y = Life.Ladder)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = TRUE) +
  labs(
    title = "Happiness Score vs GDP per Capita",
    x = "GDP per Capita",
    y = "Happiness Score"
  ) +
  theme_minimal()

# Additional Analysis: Box Plot by Year (if applicable)
if("year" %in% colnames(data)) {
  ggplot(data, aes(x = as.factor(year), y = Life.Ladder)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Distribution of Life Ladder Scores by Year",
         x = "Year", y = "Life Ladder Score")
}

# Additional Analysis: Time Series Analysis (if applicable)
if("year" %in% colnames(data)) {
  yearly_avg <- data %>%
    group_by(year) %>%
    summarise(avg_happiness = mean(Life.Ladder, na.rm = TRUE))
  
  ggplot(yearly_avg, aes(x = year, y = avg_happiness)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Average Happiness Score Over Time",
         x = "Year", y = "Average Life Ladder Score")
}

###############################################################################################

# Load necessary libraries, install them if not already installed
required_packages <- c("GGally", "reshape2", "ggplot2", "glmnet", "forecast")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# STEP 12: Advanced Visualization - Scatter Plot Matrix
# Visualizes pairwise relationships between selected variables. GGally extends ggplot2
# to make advanced visualizations like scatter plot matrices more accessible.
ggpairs(
  data[, c("Life.Ladder", "Log.GDP.per.capita", "Social.support", "Healthy.life.expectancy.at.birth")],
  upper = list(continuous = wrap("cor", size = 5)),   # Show correlations with adjusted size
  lower = list(continuous = wrap("points", alpha = 0.5, size = 0.8)),  # Add transparency and smaller points
  diag = list(continuous = wrap("densityDiag", alpha = 0.7, color = "blue"))  # Use smoother density plots
) +
  theme_minimal() +  # Apply a minimalistic theme for clarity
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10),  # Adjust axis text
        axis.text.y = element_text(size = 10))  # Adjust y-axis text size



# STEP 13: Advanced Visualization - Heatmap of Correlations
# Creates a heatmap to represent correlations between variables. Uses reshape2 to
# "melt" the correlation matrix into a format suitable for ggplot2.
library(reshape2)

# Compute the correlation matrix
cor_matrix <- cor(data[, c("Life.Ladder", "Log.GDP.per.capita", 
                           "Social.support", "Healthy.life.expectancy.at.birth")])

# Melt the correlation matrix and preserve variable names
melted_cor <- melt(cor_matrix, varnames = c("Variable1", "Variable2"))

ggplot(melted_cor, aes(Variable1, Variable2, fill = value)) +
  geom_tile() +  # Create the heatmap tiles
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Add correlation values
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +  # Custom color scale
  labs(title = "Correlation Heatmap", x = "", y = "", fill = "Correlation") +  # Add title and legend label
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Rotate and adjust x-axis text
        axis.text.y = element_text(size = 10),  # Adjust y-axis text size
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))  # Center and style the title

# STEP 14: Advanced Regression Analysis
# Constructs a multiple linear regression model to understand the relationships
# between Life Ladder and other predictors. Includes diagnostics to validate assumptions.
advanced_model <- lm(Life.Ladder ~ Log.GDP.per.capita + Social.support + 
                       Healthy.life.expectancy.at.birth + Freedom.to.make.life.choices +
                       Generosity + Perceptions.of.corruption, data = data)
summary(advanced_model)  # Displays model statistics
plot(advanced_model)     # Diagnostic plots to check assumptions of linear regression


# Step 15: Regularization - Lasso Regression

# Step 15.1: Debug the data preparation process
# Summarize the independent variables (predictors) matrix 'x' to ensure all values are valid
summary(x)

# Summarize the dependent variable (response) vector 'y' to check for validity
summary(y)

# Check the dimensions of the predictor matrix 'x'
dim(x)

# Check the length of the response variable 'y' to ensure it matches the number of rows in 'x'
length(y)

# Step 15.2: Remove missing values
# Convert specified columns to a numeric matrix for predictors and remove rows with missing values
x <- na.omit(as.matrix(data[, c("Log.GDP.per.capita", "Social.support", "Healthy.life.expectancy.at.birth",
                                "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption")]))

# Remove missing values in the response variable
y <- na.omit(data$Life.Ladder)

# Step 15.3: Ensure correct data types
# Convert selected columns into a numeric matrix for predictors
x <- as.matrix(data[, c("Log.GDP.per.capita", "Social.support", "Healthy.life.expectancy.at.birth",
                        "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption")])

# Convert the response variable to numeric
y <- as.numeric(data$Life.Ladder)

# Step 15.4: Handle rows with NA values
# Identify rows with complete data (no missing values in predictors or response)
valid_rows <- complete.cases(x, y)

# Subset the predictor matrix 'x' to include only rows with complete data
x <- x[valid_rows, ]

# Subset the response vector 'y' to include only rows with complete data
y <- y[valid_rows]

# Step 15.5: Prepare the data for modeling
# Convert specified columns into a numeric matrix for Lasso regression predictors
x <- as.matrix(data[, c("Log.GDP.per.capita", "Social.support", "Healthy.life.expectancy.at.birth",
                        "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption")])

# Assign the dependent variable (Life Ladder scores) for regression
y <- data$Life.Ladder

# Remove rows with NA values in both 'x' and 'y'
valid_rows <- complete.cases(x, y)
x <- x[valid_rows, ]
y <- y[valid_rows]

# Step 15.6: Fit the Lasso regression model
# Load the glmnet library for Lasso regression
library(glmnet)

# Perform cross-validated Lasso regression (alpha = 1 specifies Lasso regression)
lasso_model <- cv.glmnet(x, y, alpha = 1)

# Step 15.7: Plot the cross-validation curve
# Visualize the cross-validation error for different values of lambda
plot(lasso_model)

# Step 15.8: Extract coefficients for the optimal lambda
# Display the model coefficients for the lambda that minimizes cross-validation error
coef(lasso_model, s = "lambda.min")


# STEP 16: Time Series Analysis
# Analyzes Life Ladder trends over time and forecasts future values. The forecast package
# simplifies time series modeling and forecasting using ARIMA.
library(forecast)
ts_data <- ts(data$Life.Ladder, start = min(data$year), end = max(data$year), frequency = 1)
fit <- auto.arima(ts_data)  # Selects the best ARIMA model
forecast_result <- forecast(fit, h = 5)  # Forecasts Life Ladder for the next 5 years
plot(forecast_result)  # Visualizes the forecast with confidence intervals

# STEP 17: Hypothesis Testing
# Conducts a t-test comparing Life Ladder means for groups based on whether GDP
# exceeds the median. Tests if high GDP correlates with significantly different happiness scores.
t.test(Life.Ladder ~ I(Log.GDP.per.capita > median(Log.GDP.per.capita)), data = data)

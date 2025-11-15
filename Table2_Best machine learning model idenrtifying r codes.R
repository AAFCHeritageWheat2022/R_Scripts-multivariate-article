

# Chapter1 article 1 ML xGBoost SHAP figures

setwd("")
getwd()

# Step 1: Load your data (replace this with your actual data loading code)
# Assuming your data is in a CSV file named "grain_yield_data.csv"
data <- read.csv("grain_yield_data.csv")

# Step 2: Data preprocessing
# Convert factor variables
data$year <- as.factor(data$year)
data$variety <- as.factor(data$variety)
data$group <- as.factor(data$group)

# Identify predictor variables
predictor_vars <- c("TGW", "GW", "Pupper", "Plower", "DTH", "DTA", "DTM", "PHT", "FLA", "FLL", 
                    "E_day_anth", "N_2WBH", "N_1WBH", "N_WH", "CCI_anth", 
                    "GCL_ad", "APL_ad", "GCL_ab", "APL_ab", "FD_ab")



# Load required libraries
library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(e1071)
library(car)
library(corrplot)
library(igraph)
library(ggplot2)
library(shapr)
library(cowplot)

# 3. Modeling Function
perform_modeling <- function(train_data, test_data, model_type) {
  if (model_type == "svr") {
    model <- svm(GY ~ ., data = train_data[, c("GY", predictor_vars)])
  } else if (model_type == "rf") {
    model <- randomForest(GY ~ ., data = train_data[, c("GY", predictor_vars)])
  } else if (model_type == "xgb") {
    train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, predictor_vars]), label = train_data$GY)
    model <- xgboost(data = train_matrix, nrounds = 100, objective = "reg:squarederror")
  }
  
  if (model_type == "xgb") {
    test_matrix <- xgb.DMatrix(data = as.matrix(test_data[, predictor_vars]))
    predictions <- predict(model, test_matrix)
  } else {
    predictions <- predict(model, newdata = test_data[, predictor_vars])
  }
  
  rmse <- sqrt(mean((test_data$GY - predictions)^2))
  return(list(model = model, rmse = rmse))
}

# 4. Three-way Modeling
years <- levels(data$year)
models <- c("svr", "rf", "xgb")
results <- list()

for (test_year in years) {
  train_data <- data[data$year != test_year, ]
  test_data <- data[data$year == test_year, ]
  
  for (model_type in models) {
    model_results <- perform_modeling(train_data, test_data, model_type)
    results[[paste(test_year, model_type, sep = "_")]] <- model_results
  }
}

# 5. Model Performance Comparison
performance_summary <- data.frame(
  test_year = rep(years, each = length(models)),
  model = rep(models, times = length(years)),
  rmse = sapply(results, function(x) x$rmse)
)

print("Model Performance Summary:")
print(performance_summary)

library(dplyr)
library(dplyr)  # Add this at the beginning of the script with other library() calls


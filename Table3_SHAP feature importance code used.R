
# Chapter1 article 1 ML xGBoost SHAP figures

setwd("")
getwd()
# Load required libraries
library(tidyverse)
library(xgboost)
library(ggplot2)
library(cowplot)

# Read data
data <- read.csv("grain_yield_data.csv")

# Convert factor variables
data$year <- as.factor(data$year)
data$variety <- as.factor(data$variety)
data$group <- as.factor(data$group)

# Identify predictor variables
predictor_vars <- c("TGW", "GW", "Pupper", "Plower", "DTH", "DTA", "DTM", "PHT", "FLA", "FLL", 
                    "E_day_anth", "N_2WBH", "N_1WBH", "N_WH", "CCI_anth", 
                    "GCL_ad", "APL_ad", "GCL_ab", "APL_ab", "FD_ab")

# Create directory for plots
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Initialize lists to store plots
feature_plots <- list()

# Define years
years <- c("2021", "2022", "2023")

# Create individual plots for each test year using YOUR original parameters
for (i in 1:length(years)) {
  test_year <- years[i]
  cat("Processing test year:", test_year, "\n")
  
  # Split data (EXACTLY like your original code)
  train_years <- setdiff(years, test_year)
  train_idx <- data$year %in% train_years
  test_idx <- data$year == test_year
  
  X_train <- as.matrix(data[train_idx, predictor_vars])
  y_train <- data$GY[train_idx]
  X_test <- as.matrix(data[test_idx, predictor_vars])
  y_test <- data$GY[test_idx]
  
  # Train XGBoost model with YOUR original parameters
  train_matrix <- xgb.DMatrix(data = X_train, label = y_train)
  xgb_model <- xgboost(
    data = train_matrix,
    objective = "reg:squarederror",
    nrounds = 500,        # YOUR original parameter
    eta = 0.1,            # YOUR original parameter  
    max_depth = 3,        # YOUR original parameter
    verbose = 0
  )
  
  # Get feature importance (EXACTLY like your original code)
  importance_matrix <- xgb.importance(model = xgb_model)
  
  # Select top 15 features
  top_features <- importance_matrix %>% head(15)
  
  # Create the plot with exact styling from your example
  p <- ggplot(top_features, aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_bar(stat = "identity", fill = "black", width = 0.7) +
    coord_flip() +
    labs(
      title = paste("Feature importance", test_year),
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, face = "plain"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
      plot.margin = unit(c(10, 10, 10, 10), "pt")
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  
  # Store plot
  feature_plots[[i]] <- p
  
  # Save individual plot as PDF
  ggsave(paste0("plots/feature_importance_", test_year, ".pdf"), 
         p, width = 6, height = 8, units = "in")
  
  # Print the top features to verify they match your original results
  cat(paste("Top features for", test_year, ":\n"))
  print(head(importance_matrix, 10))
  cat("\n")
}

# Create the multi-panel figure
final_figure <- plot_grid(
  feature_plots[[1]] + labs(title = "A") + theme(plot.title = element_text(size = 14, face = "bold")),
  feature_plots[[2]] + labs(title = "B") + theme(plot.title = element_text(size = 14, face = "bold")),
  feature_plots[[3]] + labs(title = "C") + theme(plot.title = element_text(size = 14, face = "bold")),
  ncol = 3,
  align = "h",
  rel_widths = c(1, 1, 1)
)

# Save the final multi-panel figure as PDF
ggsave("Figure_S5_PDF.pdf", final_figure, width = 18, height = 8, units = "in")

# Also save as high-resolution PNG
ggsave("Figure_S5_PNG.png", final_figure, width = 18, height = 8, units = "in", dpi = 300)

# Print performance summary using YOUR original approach
cat("\n=== Performance Summary ===\n")
for (test_year in years) {
  train_years <- setdiff(years, test_year)
  train_idx <- data$year %in% train_years
  test_idx <- data$year == test_year
  
  X_train <- as.matrix(data[train_idx, predictor_vars])
  y_train <- data$GY[train_idx]
  X_test <- as.matrix(data[test_idx, predictor_vars])
  y_test <- data$GY[test_idx]
  
  train_matrix <- xgb.DMatrix(data = X_train, label = y_train)
  xgb_model <- xgboost(
    data = train_matrix,
    objective = "reg:squarederror",
    nrounds = 500,
    eta = 0.1,
    max_depth = 3,
    verbose = 0
  )
  
  test_matrix <- xgb.DMatrix(data = X_test)
  predictions <- predict(xgb_model, test_matrix)
  rmse <- sqrt(mean((y_test - predictions)^2))
  r2 <- cor(y_test, predictions)^2
  
  cat(paste("Test year", test_year, "- RMSE:", round(rmse, 2), "R²:", round(r2, 3), "\n"))
}

cat("\nAnalysis complete!\n")
cat("Feature importance plots created with YOUR original XGBoost parameters\n")


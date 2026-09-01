

setwd()
getwd()


# ============================================================
# FIGURE: COMPLETE REGRESSION ANALYSIS - INTEGRATED
# MLR, SVM, MERF, XGBoost with SHAP Analysis and Feature Importance
# Dataset: Figure7c_for regression analyses.csv
# ALL PREDICTORS (STOMATAL TRAITS) (VIF >= 10 REMOVED)
# GY as Dependent Variable - Leave-One-Year-Out CV (2021, 2022, 2023)
# ============================================================


# ============================================================
# PART 0: LOAD ALL REQUIRED LIBRARIES
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(car)
library(olsrr)
library(patchwork)
library(gridExtra)
library(caret)
library(randomForest)
library(e1071)
library(xgboost)
library(viridis)
library(ggrepel)
library(cowplot)
library(LongituRF)
library(shapr)
library(knitr)
library(kableExtra)
library(ggpubr)

# ============================================================
# PART 1: UNIFIED THEME FUNCTIONS
# ============================================================

unified_theme <- function(base_size = 12, base_family = "sans") {
  theme_bw() +
    theme(
      text = element_text(family = base_family, size = base_size, color = "black"),
      
      plot.title = element_text(
        size = base_size + 2, 
        face = "bold", 
        hjust = 0.5,
        margin = ggplot2::margin(b = 10)
      ),
      
      plot.subtitle = element_text(
        size = base_size,
        hjust = 0.5,
        margin = ggplot2::margin(b = 10)
      ),
      
      axis.title = element_text(
        size = base_size,
        face = "bold"
      ),
      axis.title.x = element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y = element_text(margin = ggplot2::margin(r = 8)),
      
      axis.text = element_text(
        size = base_size - 1,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_text(face = "bold"),
      
      axis.line = element_line(color = "black", size = 0.5),
      axis.ticks = element_line(color = "black", size = 0.5),
      
      panel.border = element_rect(color = "black", size = 0.8, fill = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray80", size = 0.3),
      panel.grid.minor.y = element_blank(),
      
      strip.text = element_text(size = base_size, face = "bold"),
      strip.background = element_rect(fill = "lightgray", color = "black"),
      
      legend.title = element_text(face = "bold", size = base_size - 1),
      legend.text = element_text(size = base_size - 1),
      legend.position = "bottom",
      legend.box.spacing = unit(0.2, "cm"),
      
      plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
}

barplot_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      axis.text.y = element_text(angle = 0, hjust = 1, face = "bold"),
      panel.grid.major.y = element_line(color = "gray80", size = 0.3),
      panel.grid.major.x = element_blank(),
      legend.position = "right"
    )
}

# ============================================================
# PART 2: SAVE FUNCTION
# ============================================================

save_publication_formats <- function(plot_obj, base_name, width = 12, height = 6) {
  
  tiff_filename <- paste0(base_name, ".tiff")
  ggsave(tiff_filename, plot = plot_obj, 
         width = width, height = height, units = "in",
         dpi = 600, compression = "lzw")
  cat("  Saved:", tiff_filename, "\n")
  
  pdf_filename <- paste0(base_name, ".pdf")
  ggsave(pdf_filename, plot = plot_obj, 
         width = width, height = height, units = "in")
  cat("  Saved:", pdf_filename, "\n")
  
  png_filename <- paste0(base_name, ".png")
  ggsave(png_filename, plot = plot_obj, 
         width = width, height = height, units = "in",
         dpi = 600)
  cat("  Saved:", png_filename, "\n")
  
  jpeg_filename <- paste0(base_name, ".jpeg")
  ggsave(jpeg_filename, plot = plot_obj, 
         width = width, height = height, units = "in",
         dpi = 600, quality = 1.0)
  cat("  Saved:", jpeg_filename, "\n")
}

# ============================================================
# PART 3: THREE-COLOR SCHEME (Same as Figures 3, 4, 5)
# ============================================================

group_colors <- c(
  "High STI GY" = "#2E8B57",        # Green
  "Intermediate STI GY" = "#6A0DAD", # Purple
  "Low STI GY" = "#B22222"          # Brick Red
)

# ============================================================
# PART 4: LOAD AND PREPARE DATA
# ============================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("FIGURE 7: INTEGRATED REGRESSION ANALYSIS\n")
cat("Dataset: Figure7c_for regression analyses.csv\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

df <- read.csv("Figure7c_for regression analyses.csv", header = TRUE)

# Clean column names
names(df) <- gsub("\\.\\.\\.", "_", names(df))
names(df) <- gsub("\\.\\.", "_", names(df))
names(df) <- gsub("\\.$", "", names(df))
names(df) <- gsub("-", "_", names(df))
names(df) <- gsub(" ", "_", names(df))
names(df) <- gsub("\\/", "_", names(df))
names(df) <- gsub("\\(", "", names(df))
names(df) <- gsub("\\)", "", names(df))
names(df) <- gsub("°", "", names(df))

cat("\nData loaded successfully. Columns:", ncol(df), "| Rows:", nrow(df), "\n")

# Convert group to factor with proper levels (using "group" column, not "newspecgroup")
df$group <- factor(df$group, 
                   levels = c("High STI GY", "Intermediate STI GY", "Low STI GY"))

# ============================================================
# PART 5: IDENTIFY PREDICTORS
# ============================================================

dependent_var <- "GY"

# Get all numeric columns
numeric_cols <- names(df)[sapply(df, is.numeric)]

# Exclude dependent variable and metadata columns
# Note: Using "group" instead of "newspecgroup" and "specgroup" instead of "uavspecgroup"
metadata_cols <- c("year", "group", "specgroup", "variety")
predictor_cols <- setdiff(numeric_cols, c(dependent_var, metadata_cols))

# Remove columns with all NA
predictor_cols <- predictor_cols[sapply(predictor_cols, function(x) sum(!is.na(df[[x]])) > 0)]

cat("\n===== PREDICTOR VARIABLES =====\n")
cat("  Total predictors identified:", length(predictor_cols), "\n")
cat("  Predictors:\n")
print(predictor_cols)

# ============================================================
# PART 6: DATA CLEANING
# ============================================================

# Remove rows with all NA in predictor columns
df_clean <- df[!apply(is.na(df[, predictor_cols]), 1, all), ]

# Remove rows where GY is NA
df_clean <- df_clean[!is.na(df_clean$GY), ]

cat("\nData after cleaning:", nrow(df_clean), "rows\n")

# ============================================================
# PART 7: MULTICOLLINEARITY REMOVAL (VIF >= 10)
# ============================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("MULTICOLLINEARITY DIAGNOSTIC (VIF >= 10 REMOVED)\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("\nVIF INTERPRETATION GUIDE:\n")
cat("  VIF = 1-5:     Low multicollinearity (acceptable)\n")
cat("  VIF = 5-10:    Moderate multicollinearity (may need attention)\n")
cat("  VIF >= 10:     High-Severe multicollinearity (REMOVED)\n")

cat("\nWHY VIF THRESHOLD = 10?\n")
cat("  1. Standard threshold used in agricultural and ecological research\n")
cat("  2. VIF > 10 indicates that 90% of predictor variance is explained by others\n")
cat("  3. Coefficient estimates become unstable and unreliable\n")
cat("  4. Standard errors are inflated, leading to false non-significance\n")
cat("  5. Model interpretation becomes compromised\n")

remove_multicollinearity <- function(data, predictors, target = "GY", vif_threshold = 10) {
  
  cat("\nInitial predictors:", length(predictors), "\n")
  
  complete_data <- data[complete.cases(data[, c(target, predictors)]), ]
  
  if(nrow(complete_data) < 10) {
    cat("Insufficient complete cases for VIF analysis.\n")
    return(list(predictors = predictors, removed = c()))
  }
  
  current_predictors <- predictors
  removed_vars <- c()
  iteration <- 1
  
  while(length(current_predictors) > 2) {
    formula <- as.formula(paste(target, "~", paste(current_predictors, collapse = " + ")))
    model <- tryCatch({
      lm(formula, data = complete_data)
    }, error = function(e) {
      return(NULL)
    })
    
    if(is.null(model)) {
      if(length(current_predictors) > 1) {
        remove_var <- current_predictors[1]
        current_predictors <- current_predictors[current_predictors != remove_var]
        removed_vars <- c(removed_vars, remove_var)
        iteration <- iteration + 1
        next
      } else {
        break
      }
    }
    
    vif_values <- tryCatch({
      vif(model)
    }, error = function(e) {
      return(NULL)
    })
    
    if(is.null(vif_values)) {
      if(length(current_predictors) > 1) {
        avg_cor <- colMeans(abs(cor(complete_data[, current_predictors])), na.rm = TRUE)
        remove_var <- names(which.max(avg_cor))
        current_predictors <- current_predictors[current_predictors != remove_var]
        removed_vars <- c(removed_vars, remove_var)
        iteration <- iteration + 1
        next
      } else {
        break
      }
    }
    
    max_vif <- max(vif_values)
    if(max_vif < vif_threshold) {
      cat("\n  All VIF values <", vif_threshold, "- stopping removal.\n")
      break
    }
    
    remove_var <- names(which.max(vif_values))
    cat("\n  Iteration", iteration, ": Removing", remove_var, "(VIF =", round(max_vif, 2), ">= 10)")
    current_predictors <- current_predictors[current_predictors != remove_var]
    removed_vars <- c(removed_vars, remove_var)
    iteration <- iteration + 1
  }
  
  cat("\n\n  Final predictors:", length(current_predictors), "\n")
  
  return(list(
    predictors = current_predictors, 
    removed = removed_vars
  ))
}

multi_results <- remove_multicollinearity(df_clean, predictor_cols, "GY", vif_threshold = 10)

if(length(multi_results$predictors) == 0) {
  selected_predictors <- predictor_cols
} else {
  selected_predictors <- multi_results$predictors
}

cat("\n  Final selected predictors:", length(selected_predictors), "\n")
cat("  Removed predictors:", length(multi_results$removed), "\n")
if(length(multi_results$removed) > 0) {
  cat("  Removed predictors:", paste(multi_results$removed, collapse = ", "), "\n")
}

# ============================================================
# PART 8: PERFORMANCE METRICS FUNCTION
# ============================================================

calculate_performance <- function(observed, predicted) {
  cor_test <- cor.test(observed, predicted)
  rmse <- sqrt(mean((observed - predicted)^2))
  mae <- mean(abs(observed - predicted))
  r2 <- cor_test$estimate^2
  
  return(data.frame(
    R = cor_test$estimate,
    R2 = r2,
    RMSE = rmse,
    MAE = mae,
    P_value = cor_test$p.value
  ))
}

# ============================================================
# PART 9: PREDICTION PLOT (WITHOUT DOTTED LINES)
# ============================================================

create_prediction_plot <- function(observed, predicted, model_name, test_year, train_years, color_var) {
  plot_df <- data.frame(
    Observed = observed,
    Predicted = predicted,
    Group = color_var
  )
  
  cor_test <- cor.test(observed, predicted)
  rmse <- sqrt(mean((observed - predicted)^2))
  r2 <- cor_test$estimate^2
  
  ggplot(plot_df, aes(x = Observed, y = Predicted, color = Group)) +
    geom_point(size = 3, alpha = 0.8) +
    labs(title = paste(model_name, "- Train:", paste(train_years, collapse = ",")),
         subtitle = paste0("R = ", round(cor_test$estimate, 3), 
                           ", R² = ", round(r2, 3),
                           ", RMSE = ", round(rmse, 1)),
         x = "Observed GY (kg/ha)",
         y = "Predicted GY (kg/ha)") +
    scale_color_manual(values = group_colors, name = "STI Group") +
    unified_theme() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9, hjust = 0.5)
    )
}

# ============================================================
# PART 10: TRAIN ALL MODELS FUNCTION
# ============================================================

train_all_models <- function(train_data, test_data, predictors, target = "GY") {
  results <- list()
  
  X_train <- train_data[, predictors, drop = FALSE]
  y_train <- train_data[[target]]
  X_test <- test_data[, predictors, drop = FALSE]
  y_test <- test_data[[target]]
  
  # ---------- 1. MLR (Stepwise Backward) ----------
  cat("\n  Training MLR (Stepwise Backward)...\n")
  full_formula <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))
  full_model <- lm(full_formula, data = train_data)
  
  tryCatch({
    stepwise_result <- ols_step_backward_p(full_model, prem = 0.05)
    mlr_model <- stepwise_result$model
    mlr_pred <- predict(mlr_model, test_data)
    mlr_vars <- names(coef(mlr_model))[-1]
    mlr_summary <- summary(mlr_model)
  }, error = function(e) {
    cat("    Stepwise failed:", e$message, "\n")
    mlr_model <- full_model
    mlr_pred <- predict(mlr_model, test_data)
    mlr_vars <- predictors
    mlr_summary <- summary(full_model)
  })
  
  # Calculate VIF for MLR
  mlr_vif <- tryCatch({
    vif(mlr_model)
  }, error = function(e) {
    return(NULL)
  })
  
  results$MLR <- list(
    model = mlr_model,
    predictions = mlr_pred,
    selected_vars = mlr_vars,
    summary = mlr_summary,
    vif = mlr_vif
  )
  
  # ---------- 2. SVM ----------
  cat("  Training SVM...\n")
  tryCatch({
    svm_model <- svm(x = X_train, y = y_train, 
                     kernel = "radial", 
                     scale = TRUE,
                     cost = 10,
                     gamma = 0.1)
    svm_pred <- predict(svm_model, X_test)
    results$SVM <- list(model = svm_model, predictions = svm_pred)
  }, error = function(e) {
    cat("    SVM failed:", e$message, "\n")
    results$SVM <- NULL
  })
  
  # ---------- 3. MERF (Mixed Effects Random Forest) ----------
  cat("  Training MERF...\n")
  tryCatch({
    X_train_mat <- as.matrix(X_train)
    X_test_mat <- as.matrix(X_test)
    train_id <- as.numeric(factor(train_data$year))
    test_id <- as.numeric(factor(test_data$year))
    train_time <- train_data$year - min(train_data$year) + 1
    test_time <- test_data$year - min(test_data$year) + 1
    Z_train <- matrix(1, nrow = nrow(train_data), ncol = 1)
    Z_test <- matrix(1, nrow = nrow(test_data), ncol = 1)
    
    merf_model <- LongituRF::MERF(
      X = X_train_mat,
      Y = y_train,
      id = train_id,
      Z = Z_train,
      time = train_time,
      ntree = 500,
      mtry = max(1, floor(length(predictors)/3)),
      sto = "none",
      iter = 50,
      delta = 0.001
    )
    
    merf_pred <- predict(merf_model, X = X_test_mat, Z = Z_test, 
                         id = test_id, time = test_time)
    
    # Extract MERF importance
    merf_importance <- tryCatch({
      if(!is.null(merf_model$forest)) {
        imp_vals <- randomForest::importance(merf_model$forest)
        if(!is.null(imp_vals) && nrow(imp_vals) > 0) {
          data.frame(
            Feature = rownames(imp_vals),
            Importance = imp_vals[, "%IncMSE"],
            stringsAsFactors = FALSE
          ) %>% arrange(desc(Importance))
        } else { NULL }
      } else { NULL }
    }, error = function(e) { NULL })
    
    results$MERF <- list(
      model = merf_model,
      predictions = merf_pred,
      importance = merf_importance
    )
  }, error = function(e) {
    cat("    MERF failed:", e$message, "\n")
    results$MERF <- NULL
  })
  
  # ---------- 4. XGBoost ----------
  cat("  Training XGBoost...\n")
  tryCatch({
    # Convert to matrix
    X_train_mat <- as.matrix(X_train)
    X_test_mat <- as.matrix(X_test)
    
    # Create DMatrix objects
    dtrain <- xgb.DMatrix(data = X_train_mat, label = y_train)
    dtest <- xgb.DMatrix(data = X_test_mat)
    
    # Set parameters
    params <- list(
      objective = "reg:squarederror",
      learning_rate = 0.1,
      max_depth = 3,
      subsample = 0.8,
      colsample_bytree = 0.8
    )
    
    # Train model
    xgb_model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = 500,
      verbose = 0
    )
    
    # Make predictions
    xgb_pred <- predict(xgb_model, dtest)
    
    # Get feature importance
    xgb_importance <- xgb.importance(model = xgb_model, feature_names = predictors)
    
    if("Gain" %in% names(xgb_importance)) {
      names(xgb_importance)[names(xgb_importance) == "Gain"] <- "Importance"
    }
    
    results$XGBoost <- list(
      model = xgb_model,
      predictions = xgb_pred,
      importance = xgb_importance
    )
  }, error = function(e) {
    cat("    XGBoost failed:", e$message, "\n")
    results$XGBoost <- NULL
  })
  
  return(results)
}

# ============================================================
# PART 11: SHAP ANALYSIS FUNCTION
# ============================================================

perform_shap_analysis <- function(model, X_data, model_type = "xgb", predictors) {
  tryCatch({
    if(model_type == "xgb") {
      pred_func <- function(model, newdata) {
        predict(model, xgb.DMatrix(data = as.matrix(newdata)))
      }
    } else if(model_type == "merf") {
      pred_func <- function(model, newdata) {
        predict(model$forest, newdata)
      }
    } else {
      pred_func <- function(model, newdata) {
        predict(model, newdata)
      }
    }
    
    n_samples <- min(50, nrow(X_data))
    X_sample <- X_data[1:n_samples, ]
    
    explainer <- shapr::shapr(X_sample, model = model, predict_function = pred_func)
    shap_values <- shapr::explain(X_sample, explainer, approach = "empirical")
    
    shap_importance <- data.frame(
      Feature = predictors,
      Importance = colMeans(abs(shap_values$dt), na.rm = TRUE),
      stringsAsFactors = FALSE
    ) %>% arrange(desc(Importance))
    
    return(list(shap_values = shap_values, shap_importance = shap_importance))
    
  }, error = function(e) {
    cat("  SHAP failed:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================
# PART 12: CREATE FEATURE IMPORTANCE PLOT
# ============================================================

create_feature_importance_plot <- function(importance_data, model_name, test_year, perf_data, top_n = 15) {
  if(is.null(importance_data) || nrow(importance_data) == 0) return(NULL)
  
  top_features <- importance_data %>% head(top_n)
  
  total_imp <- sum(top_features$Importance, na.rm = TRUE)
  if(total_imp > 0) {
    top_features$Contribution <- (top_features$Importance / total_imp) * 100
  } else {
    top_features$Contribution <- 0
  }
  
  # Get performance metrics
  perf <- perf_data[perf_data$Year == as.numeric(test_year) & 
                      perf_data$Model == model_name, ]
  
  rmse_val <- if(nrow(perf) > 0) round(perf$RMSE, 1) else "NA"
  r2_val <- if(nrow(perf) > 0) round(perf$R2, 3) else "NA"
  
  p <- ggplot(top_features, aes(x = reorder(Feature, Contribution), y = Contribution)) +
    geom_bar(stat = "identity", fill = "#2E86AB", alpha = 0.8, color = "black", linewidth = 0.3) +
    coord_flip() +
    labs(
      title = paste("Validation Year:", test_year),
      subtitle = paste0("RMSE: ", rmse_val, " kg/ha | R²: ", r2_val),
      x = "Features",
      y = "Feature Importance (%)"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    geom_text(aes(label = sprintf("%.1f%%", Contribution)), 
              hjust = -0.2, size = 3.2, fontface = "bold", color = "black") +
    barplot_theme()
  
  return(p)
}

# ============================================================
# PART 13: CREATE SHAP PLOT
# ============================================================

create_shap_plot <- function(shap_importance, model_name, test_year, top_n = 10) {
  if(is.null(shap_importance) || nrow(shap_importance) == 0) return(NULL)
  
  top_features <- shap_importance %>% head(top_n)
  
  ggplot(top_features, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "#D55E00", alpha = 0.8, color = "black", linewidth = 0.3) +
    geom_text(aes(label = sprintf("%.4f", Importance)), 
              hjust = -0.1, size = 3.2, fontface = "bold", color = "black") +
    coord_flip() +
    labs(
      title = paste("SHAP -", model_name, "- Test Year:", test_year),
      x = "Features",
      y = "Mean |SHAP Value|"
    ) +
    unified_theme() +
    theme(
      axis.text.y = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )
}

# ============================================================
# PART 14: FUNCTION TO EXPORT MLR TABLES
# ============================================================

export_mlr_table <- function(mlr_model, test_year, train_years, model_name = "MLR") {
  if(is.null(mlr_model)) return(NULL)
  
  summary_obj <- summary(mlr_model)
  coef_table <- summary_obj$coefficients
  
  # Create coefficient table
  df_table <- data.frame(
    Predictor = rownames(coef_table),
    Estimate = coef_table[, 1],
    Std_Error = coef_table[, 2],
    t_value = coef_table[, 3],
    P_value = coef_table[, 4],
    stringsAsFactors = FALSE
  )
  
  # Remove intercept for clarity in feature list
  df_table_clean <- df_table[df_table$Predictor != "(Intercept)", ]
  
  # Add significance stars
  df_table_clean$Significance <- ifelse(df_table_clean$P_value < 0.001, "***",
                                        ifelse(df_table_clean$P_value < 0.01, "**",
                                               ifelse(df_table_clean$P_value < 0.05, "*", "ns")))
  
  # Calculate VIF if possible
  vif_vals <- tryCatch({
    vif(mlr_model)
  }, error = function(e) {
    return(NULL)
  })
  
  if(!is.null(vif_vals) && length(vif_vals) > 0) {
    if(length(vif_vals) == nrow(df_table_clean)) {
      df_table_clean$VIF <- round(vif_vals, 2)
    } else {
      # Match VIF values to predictors
      vif_df <- data.frame(Predictor = names(vif_vals), VIF = round(vif_vals, 2))
      df_table_clean <- merge(df_table_clean, vif_df, by = "Predictor", all.x = TRUE)
    }
  }
  
  # Model statistics
  r2 <- summary_obj$r.squared
  adj_r2 <- summary_obj$adj.r.squared
  f_stat <- summary_obj$fstatistic
  f_value <- f_stat[1]
  f_p <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  rmse <- summary_obj$sigma
  
  # Create a model summary row
  model_stats <- data.frame(
    Predictor = "MODEL STATISTICS",
    Estimate = NA,
    Std_Error = NA,
    t_value = NA,
    P_value = NA,
    Significance = "",
    VIF = NA
  )
  
  # Add statistics rows
  stats_rows <- data.frame(
    Predictor = c("R²", "Adjusted R²", "F-statistic", "F p-value", "RMSE"),
    Estimate = c(round(r2, 4), round(adj_r2, 4), round(f_value, 2), round(f_p, 6), round(rmse, 2)),
    Std_Error = rep(NA, 5),
    t_value = rep(NA, 5),
    P_value = rep(NA, 5),
    Significance = rep("", 5),
    VIF = rep(NA, 5),
    stringsAsFactors = FALSE
  )
  
  # Combine tables
  final_table <- rbind(df_table_clean, model_stats, stats_rows)
  
  # Write to CSV
  write.csv(final_table, paste0("Figure7_MLR_Detailed_Table_Test", test_year, ".csv"), row.names = FALSE)
  
  # Print table to console in kable format
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("OPTIMIZED MLR MODEL - Test Year:", test_year, "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("Training years:", paste(train_years, collapse = ", "), "\n")
  cat("Predictors retained in final model:", nrow(df_table_clean), "\n")
  cat(paste(rep("-", 80), collapse = ""), "\n")
  
  # Print coefficient table (without stats rows for clarity)
  print(kable(df_table_clean, 
              format = "simple",
              digits = 4,
              col.names = c("Predictor", "Estimate", "Std. Error", "t value", "Pr(>|t|)", "Signif.", "VIF")))
  
  cat("\n", paste(rep("-", 80), collapse = ""), "\n")
  cat("MODEL FIT STATISTICS:\n")
  cat(sprintf("  R² = %.4f, Adjusted R² = %.4f\n", r2, adj_r2))
  cat(sprintf("  F(%d, %d) = %.2f, p = %.4e\n", f_stat[2], f_stat[3], f_value, f_p))
  cat(sprintf("  RMSE = %.3f\n", rmse))
  cat("\nSignificance codes: *** p < 0.001, ** p < 0.01, * p < 0.05, ns = non-significant\n")
  
  return(final_table)
}

# ============================================================
# PART 15: RUN ALL ANALYSES
# ============================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("RUNNING MODEL ANALYSES (MLR, SVM, MERF, XGBoost)\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

years <- c(2021, 2022, 2023)
all_results <- list()
performance_summary <- data.frame()
shap_summary <- data.frame()

# Storage for plots
prediction_plots <- list()
importance_plots <- list(MLR = list(), SVM = list(), MERF = list(), XGBoost = list())
shap_plots <- list(MERF = list(), XGBoost = list())
importance_tables <- list(MLR = list(), SVM = list(), MERF = list(), XGBoost = list())
mlr_summary_tables <- list()
mlr_detailed_tables <- list()

for(yr in years) {
  train_years <- setdiff(years, yr)
  test_year <- yr
  
  cat("\n\n--- Year", test_year, "as Test ---\n")
  cat("  Training years:", paste(train_years, collapse = ", "), "\n")
  
  train_data <- df_clean[df_clean$year %in% train_years, ]
  test_data <- df_clean[df_clean$year == test_year, ]
  
  train_data <- train_data[complete.cases(train_data[, c("GY", selected_predictors)]), ]
  test_data <- test_data[complete.cases(test_data[, c("GY", selected_predictors)]), ]
  
  if(nrow(train_data) < 10 || nrow(test_data) < 3) {
    cat("  Insufficient data for year", test_year, "\n")
    next
  }
  
  cat("  Training data:", nrow(train_data), "| Test data:", nrow(test_data), "\n")
  
  # Train models
  models <- train_all_models(train_data, test_data, selected_predictors, "GY")
  all_results[[as.character(test_year)]] <- models
  
  X_test_mat <- as.matrix(test_data[, selected_predictors])
  
  # Process each model
  for(model_name in names(models)) {
    if(!is.null(models[[model_name]])) {
      preds <- models[[model_name]]$predictions
      
      perf <- calculate_performance(test_data$GY, preds)
      perf$Year <- test_year
      perf$Model <- model_name
      perf$Train_Years <- paste(train_years, collapse = ", ")
      performance_summary <- rbind(performance_summary, perf)
      
      # Store prediction plot (without dotted lines)
      plot <- create_prediction_plot(test_data$GY, preds, model_name, test_year, 
                                     train_years, test_data$group)  # Using "group" instead of "newspecgroup"
      prediction_plots[[paste(model_name, test_year, sep = "_")]] <- plot
      
      # Store feature importance
      if(!is.null(models[[model_name]]$importance)) {
        imp <- models[[model_name]]$importance
        imp$Year <- test_year
        imp$Model <- model_name
        
        if("Gain" %in% names(imp) && !("Importance" %in% names(imp))) {
          names(imp)[names(imp) == "Gain"] <- "Importance"
        }
        
        importance_tables[[model_name]][[as.character(test_year)]] <- imp
        
        imp_plot <- create_feature_importance_plot(imp, model_name, test_year, 
                                                   performance_summary, top_n = 15)
        if(!is.null(imp_plot)) {
          importance_plots[[model_name]][[as.character(test_year)]] <- imp_plot
        }
      }
      
      # Export detailed MLR table
      if(model_name == "MLR" && !is.null(models[[model_name]]$model)) {
        mlr_table <- export_mlr_table(models[[model_name]]$model, test_year, train_years)
        mlr_detailed_tables[[as.character(test_year)]] <- mlr_table
      }
      
      # SHAP Analysis for MERF and XGBoost
      if(model_name %in% c("MERF", "XGBoost")) {
        cat("  SHAP for", model_name, "...\n")
        model_type <- ifelse(model_name == "XGBoost", "xgb", "merf")
        shap_result <- perform_shap_analysis(models[[model_name]]$model, 
                                             X_test_mat, model_type, selected_predictors)
        
        if(!is.null(shap_result) && !is.null(shap_result$shap_importance)) {
          shap_result$shap_importance$Model <- model_name
          shap_result$shap_importance$Year <- test_year
          shap_summary <- rbind(shap_summary, shap_result$shap_importance)
          
          shap_plot <- create_shap_plot(shap_result$shap_importance, model_name, test_year)
          if(!is.null(shap_plot)) {
            shap_plots[[model_name]][[as.character(test_year)]] <- shap_plot
          }
        }
      }
    }
  }
}

# ============================================================
# PART 16: CREATE COMBINED PREDICTION PLOTS (NO DOTTED LINES)
# ============================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("CREATING COMBINED PREDICTION PLOTS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

for(model_name in c("MLR", "SVM", "MERF", "XGBoost")) {
  year_plots <- list()
  for(yr in c("2021", "2022", "2023")) {
    key <- paste(model_name, yr, sep = "_")
    if(!is.null(prediction_plots[[key]])) {
      label <- ifelse(yr == "2021", "A", ifelse(yr == "2022", "B", "C"))
      year_plots[[yr]] <- prediction_plots[[key]] + 
        labs(tag = label) +
        theme(plot.tag = element_text(size = 14, face = "bold"))
    }
  }
  
  if(length(year_plots) == 3) {
    combined <- wrap_plots(year_plots, ncol = 3, guides = 'collect') +
      plot_annotation(
        title = paste(model_name, "- Prediction Performance Across Years"),
        theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
      ) &
      theme(legend.position = 'bottom')
    
    print(combined)
    save_publication_formats(combined, paste0("Figure7_", model_name, "_Combined_NoDotted"), 
                             width = 15, height = 5)
  }
}

# ============================================================
# PART 17: CREATE FEATURE IMPORTANCE PANELS
# ============================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("CREATING FEATURE IMPORTANCE PANELS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

for(model_name in c("MLR", "SVM", "MERF", "XGBoost")) {
  year_plots <- importance_plots[[model_name]]
  
  if(length(year_plots) > 0) {
    for(yr in c("2021", "2022", "2023")) {
      if(!is.null(year_plots[[yr]])) {
        save_publication_formats(year_plots[[yr]], 
                                 paste0("Figure7_FeatureImportance_", model_name, "_", yr), 
                                 width = 10, height = 8)
      }
    }
    
    if(length(year_plots) == 3) {
      plot_list <- list()
      for(yr in c("2021", "2022", "2023")) {
        if(!is.null(year_plots[[yr]])) {
          p <- year_plots[[yr]] + 
            labs(tag = ifelse(yr == "2021", "A", ifelse(yr == "2022", "B", "C"))) +
            theme(plot.tag = element_text(size = 14, face = "bold"))
          plot_list[[yr]] <- p
        }
      }
      
      if(length(plot_list) == 3) {
        model_perf <- performance_summary[performance_summary$Model == model_name, ]
        avg_rmse <- if(nrow(model_perf) > 0) round(mean(model_perf$RMSE), 1) else "NA"
        avg_r2 <- if(nrow(model_perf) > 0) round(mean(model_perf$R2), 3) else "NA"
        
        multi_panel <- plot_grid(plotlist = plot_list, ncol = 3, align = 'h')
        
        title_panel <- ggplot() + 
          labs(
            title = paste("Feature Importance (", model_name, " Model)", sep = ""),
            subtitle = paste("Top 15 features | Avg RMSE:", avg_rmse, "kg/ha | R²:", avg_r2)
          ) +
          theme_void() +
          theme(
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 5)),
            plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40")
          )
        
        final_figure <- plot_grid(title_panel, multi_panel, ncol = 1, rel_heights = c(0.1, 1))
        
        print(final_figure)
        save_publication_formats(final_figure, paste0("Figure7_FeatureImportance_", model_name, "_Panel"), 
                                 width = 18, height = 8)
      }
    }
  }
}

# ============================================================
# PART 18: CREATE SHAP PANELS
# ============================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("CREATING SHAP PANELS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

for(model_name in c("MERF", "XGBoost")) {
  year_plots <- shap_plots[[model_name]]
  
  if(length(year_plots) > 0) {
    for(yr in c("2021", "2022", "2023")) {
      if(!is.null(year_plots[[yr]])) {
        save_publication_formats(year_plots[[yr]], 
                                 paste0("Figure7_SHAP_", model_name, "_", yr), 
                                 width = 10, height = 8)
      }
    }
    
    if(length(year_plots) == 3) {
      plot_list <- list()
      for(yr in c("2021", "2022", "2023")) {
        if(!is.null(year_plots[[yr]])) {
          p <- year_plots[[yr]] + 
            labs(tag = ifelse(yr == "2021", "A", ifelse(yr == "2022", "B", "C"))) +
            theme(plot.tag = element_text(size = 14, face = "bold"))
          plot_list[[yr]] <- p
        }
      }
      
      if(length(plot_list) == 3) {
        multi_panel <- plot_grid(plotlist = plot_list, ncol = 3, align = 'h')
        
        title_panel <- ggplot() + 
          labs(
            title = paste("SHAP Feature Importance (", model_name, " Model)", sep = ""),
            subtitle = "Top 10 features ranked by SHAP importance"
          ) +
          theme_void() +
          theme(
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 5)),
            plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40")
          )
        
        final_figure <- plot_grid(title_panel, multi_panel, ncol = 1, rel_heights = c(0.1, 1))
        
        print(final_figure)
        save_publication_formats(final_figure, paste0("Figure7_SHAP_", model_name, "_Panel"), 
                                 width = 18, height = 8)
      }
    }
  }
}

# ============================================================
# PART 19: PREDICTION ACCURACY BAR PLOT
# ============================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("CREATING PREDICTION ACCURACY PLOT\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

if(nrow(performance_summary) > 0) {
  accuracy_plot <- ggplot(performance_summary, aes(x = Model, y = R2, fill = as.factor(Year))) +
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
    geom_text(aes(label = sprintf("%.3f", R2)),
              position = position_dodge(0.8), vjust = -0.5, size = 3.5, fontface = "bold") +
    labs(
      title = "Prediction Accuracy by Model and Year",
      x = "Model", 
      y = expression(R^2),
      fill = "Test Year"
    ) +
    scale_fill_manual(values = c("#2E8B57", "#6A0DAD", "#B22222")) +
    unified_theme() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  print(accuracy_plot)
  save_publication_formats(accuracy_plot, "Figure7_Prediction_Accuracy", width = 12, height = 8)
}

# ============================================================
# PART 20: EXPORT ALL RESULTS
# ============================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("EXPORTING RESULTS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Export performance summary
if(nrow(performance_summary) > 0) {
  write.csv(performance_summary, "Figure7_Performance_Summary_Full.csv", row.names = FALSE)
  
  model_summary <- performance_summary %>%
    group_by(Model) %>%
    summarise(
      Avg_R2 = mean(R2, na.rm = TRUE),
      Avg_RMSE = mean(RMSE, na.rm = TRUE),
      Avg_MAE = mean(MAE, na.rm = TRUE),
      SD_R2 = sd(R2, na.rm = TRUE)
    ) %>%
    arrange(desc(Avg_R2))
  
  cat("\nMODEL PERFORMANCE SUMMARY:\n")
  print(model_summary)
  write.csv(model_summary, "Figure7_Model_Summary_Avg.csv", row.names = FALSE)
}

# Export feature importance tables
for(model_name in c("MLR", "SVM", "MERF", "XGBoost")) {
  if(length(importance_tables[[model_name]]) > 0) {
    all_imp <- data.frame()
    for(yr in c("2021", "2022", "2023")) {
      if(!is.null(importance_tables[[model_name]][[yr]])) {
        imp <- importance_tables[[model_name]][[yr]]
        imp$Year <- yr
        all_imp <- rbind(all_imp, imp)
      }
    }
    
    if(nrow(all_imp) > 0) {
      if("Gain" %in% names(all_imp) && !("Importance" %in% names(all_imp))) {
        names(all_imp)[names(all_imp) == "Gain"] <- "Importance"
      }
      
      if("Importance" %in% names(all_imp)) {
        write.csv(all_imp, paste0("Figure7_FeatureImportance_", model_name, "_Full.csv"), row.names = FALSE)
        
        cat("\n=== ", model_name, " Top 5 Features ===\n")
        top5 <- all_imp %>%
          group_by(Feature) %>%
          summarise(Avg_Importance = mean(Importance, na.rm = TRUE)) %>%
          arrange(desc(Avg_Importance)) %>%
          head(5)
        print(top5)
      }
    }
  }
}

# Export SHAP summary
if(nrow(shap_summary) > 0) {
  write.csv(shap_summary, "Figure7_SHAP_Detailed_Results.csv", row.names = FALSE)
  
  cat("\n=== SHAP Top 5 Features ===\n")
  shap_top <- shap_summary %>%
    group_by(Feature) %>%
    summarise(Avg_Importance = mean(Importance, na.rm = TRUE)) %>%
    arrange(desc(Avg_Importance)) %>%
    head(5)
  print(shap_top)
}

# ============================================================
# PART 21: SUMMARY OF MLR TABLES EXPORTED
# ============================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("MLR DETAILED TABLES EXPORTED\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("\nThe following MLR detailed tables have been exported:\n")
cat("  Figure7_MLR_Detailed_Table_Test2021.csv\n")
cat("  Figure7_MLR_Detailed_Table_Test2022.csv\n")
cat("  Figure7_MLR_Detailed_Table_Test2023.csv\n")
cat("\nThese tables include:\n")
cat("  - Predictor names with coefficients, standard errors, t-values, p-values\n")
cat("  - Significance codes (***, **, *, ns)\n")
cat("  - VIF values for each predictor\n")
cat("  - Model statistics (R², Adjusted R², F-statistic, RMSE)\n")

# ============================================================
# PART 22: FINAL SUMMARY
# ============================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("FIGURE 7 - ANALYSIS COMPLETED SUCCESSFULLY\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("\nFILES CREATED:\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("  Figure7_MLR_Combined_NoDotted.{tiff,pdf,png,jpeg}\n")
cat("  Figure7_SVM_Combined_NoDotted.{tiff,pdf,png,jpeg}\n")
cat("  Figure7_MERF_Combined_NoDotted.{tiff,pdf,png,jpeg}\n")
cat("  Figure7_XGBoost_Combined_NoDotted.{tiff,pdf,png,jpeg}\n")
cat("  Figure7_FeatureImportance_*_Panel.{tiff,pdf,png,jpeg}\n")
cat("  Figure7_SHAP_*_Panel.{tiff,pdf,png,jpeg}\n")
cat("  Figure7_Prediction_Accuracy.{tiff,pdf,png,jpeg}\n")
cat("  Figure7_MLR_Detailed_Table_Test2021.csv\n")
cat("  Figure7_MLR_Detailed_Table_Test2022.csv\n")
cat("  Figure7_MLR_Detailed_Table_Test2023.csv\n")
cat("  Figure7_Performance_Summary_Full.csv\n")
cat("  Figure7_Model_Summary_Avg.csv\n")
cat("  Figure7_FeatureImportance_*_Full.csv\n")
cat("  Figure7_SHAP_Detailed_Results.csv\n")

cat("\nKEY FEATURES:\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("\nDATASET: Figure7c_for regression analyses.csv (Stomatal traits only)\n")
cat("\nPREDICTED VS OBSERVED PLOTS: No dotted 1:1 lines (clean plots)\n")
cat("\nMLR TABLES: Complete coefficient tables with VIF for each test year\n")
cat("\nVIF THRESHOLD: >= 10 (Variables with VIF >= 10 removed)\n")
cat("\nVIF INTERPRETATION:\n")
cat("  VIF = 1-5:     Low multicollinearity (acceptable)\n")
cat("  VIF = 5-10:    Moderate multicollinearity (monitor)\n")
cat("  VIF >= 10:     Severe multicollinearity (REMOVED)\n")
cat("\nMODELS EVALUATED: MLR, SVM, MERF, XGBoost\n")
cat("\nPREDICTORS: All stomatal traits (Ab_*, Ad_*, Ad_Ab_SD_ratio)\n")
cat("\nCOLOR SCHEME: High STI GY (#2E8B57), Intermediate (#6A0DAD), Low (#B22222)\n")

cat("\nAll outputs saved with 600 DPI resolution!\n")

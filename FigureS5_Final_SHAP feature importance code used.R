setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd()

library(tidyverse)
library(xgboost)
library(ggplot2)
library(cowplot)
library(viridis)

unified_theme <- function(base_size = 12, base_family = "sans") {
  theme_bw() +
    theme(
      text = element_text(family = base_family, size = base_size, color = "black"),

      plot.title = element_text(
        size = base_size + 2, 
        face = "bold", 
        hjust = 0.5,
        margin = margin(b = 10)
      ),

      axis.title = element_text(
        size = base_size,
        face = "bold"
      ),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8)),

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
      legend.position = "right",

      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
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


save_publication_formats <- function(plot_obj, base_name, width = 12, height = 6) {

  tiff_filename <- paste0(base_name, ".tiff")
  ggsave(tiff_filename, plot = plot_obj, 
         width = width, height = height, units = "in",
         dpi = 600, compression = "lzw")
  cat("Saved:", tiff_filename, "\n")
  
  pdf_filename <- paste0(base_name, ".pdf")
  ggsave(pdf_filename, plot = plot_obj, 
         width = width, height = height, units = "in",
         device = cairo_pdf)
  cat("Saved:", pdf_filename, "\n")

  png_filename <- paste0(base_name, ".png")
  ggsave(png_filename, plot = plot_obj, 
         width = width, height = height, units = "in",
         dpi = 600)
  cat("Saved:", png_filename, "\n")

  jpeg_filename <- paste0(base_name, ".jpeg")
  ggsave(jpeg_filename, plot = plot_obj, 
         width = width, height = height, units = "in",
         dpi = 600, quality = 1.0)
  cat("Saved:", jpeg_filename, "\n")
}

data <- read.csv("FigureS5_grain_yield_data.csv")

data$year <- as.factor(data$year)
data$variety <- as.factor(data$variety)
data$group <- as.factor(data$group)

predictor_vars <- c("TGW", "GW", "Pupper", "Plower", "DTH", "DTA", "DTM", "PHT", "FLA", "FLL", 
                    "E_day_anth", "N_2WBH", "N_1WBH", "N_WH", "CCI_anth", 
                    "Ad_GCL", "Ad_APL", "Ab_GCL", "Ab_APL", "FD_ab")


if (!dir.exists("final_figures")) {
  dir.create("final_figures")
}


feature_plots <- list()
performance_results <- data.frame()
years <- c("2021", "2022", "2023")


for (i in 1:length(years)) {
  test_year <- years[i]
  cat("Processing:", test_year, "\n")
  

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
  

  importance_matrix <- xgb.importance(model = xgb_model)
  top_features <- importance_matrix %>% head(15) %>% mutate(Contribution = Gain * 100)
  

  predictions <- predict(xgb_model, xgb.DMatrix(data = X_test))
  rmse <- sqrt(mean((y_test - predictions)^2))
  r2 <- cor(y_test, predictions)^2
  mae <- mean(abs(y_test - predictions))
  
  performance_results <- rbind(performance_results, 
                               data.frame(Year = test_year, RMSE = rmse, R2 = r2, MAE = mae))
  

  p <- ggplot(top_features, aes(x = reorder(Feature, Contribution), y = Contribution)) +
    geom_bar(stat = "identity", fill = "#2E86AB", alpha = 0.8, color = "black", linewidth = 0.3) +
    coord_flip() +
    labs(
      title = paste("Validation Year:", test_year),
      x = "Features",
      y = "Feature Importance (%)",
      subtitle = paste("RMSE:", round(rmse, 1), "kg/ha | R²:", round(r2, 3), " | MAE:", round(mae, 1), "kg/ha")
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    # Add value labels on bars
    geom_text(aes(label = sprintf("%.1f%%", Contribution)), 
              hjust = -0.2, size = 3.2, fontface = "bold", color = "black") +
    barplot_theme()  # Apply unified bar plot theme
  
  feature_plots[[i]] <- p
  
  save_publication_formats(p, paste0("final_figures/Feature_Importance_", test_year), width = 10, height = 8)
}

multi_panel <- plot_grid(
  feature_plots[[1]] + labs(tag = "A") + 
    theme(plot.tag = element_text(size = 16, face = "bold"),
          plot.tag.position = c(0.05, 0.95)),
  feature_plots[[2]] + labs(tag = "B") + 
    theme(plot.tag = element_text(size = 16, face = "bold"),
          plot.tag.position = c(0.05, 0.95)),
  feature_plots[[3]] + labs(tag = "C") + 
    theme(plot.tag = element_text(size = 16, face = "bold"),
          plot.tag.position = c(0.05, 0.95)),
  ncol = 3, align = 'h', labels = NULL
)

title_panel <- ggplot() + 
  labs(title = "Feature Importance Analysis for Grain Yield Prediction (XGBoost Model)",
       subtitle = "Top 15 features ranked by relative importance across independent validation years") +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40")
  )

final_figure <- plot_grid(
  title_panel,
  multi_panel,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

save_publication_formats(final_figure, "final_figures/Figure_S5_Feature_Importance", width = 18, height = 8)

ggsave("final_figures/Figure_S5_Feature_Importance.eps", final_figure, 
       width = 18, height = 8)
ggsave("final_figures/Figure_S5_Feature_Importance.svg", final_figure, 
       width = 18, height = 8)

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("FEATURE IMPORTANCE ANALYSIS COMPLETE\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n\n")

cat("MODEL PERFORMANCE SUMMARY:\n")
cat(paste(rep("-", 40), collapse = ""))
cat("\n")
for (i in 1:nrow(performance_results)) {
  yr <- performance_results$Year[i]
  cat(sprintf("Year %s: RMSE = %6.1f kg/ha | R² = %5.3f | MAE = %6.1f kg/ha\n",
              yr, performance_results$RMSE[i], performance_results$R2[i], performance_results$MAE[i]))
}

cat("\n")
cat(paste(rep("-", 40), collapse = ""))
cat("\n")
cat(sprintf("Average Performance: RMSE = %6.1f kg/ha | R² = %5.3f\n",
            mean(performance_results$RMSE), mean(performance_results$R2)))

cat("\nTOP CONSISTENT FEATURES ACROSS YEARS:\n")
cat(paste(rep("-", 40), collapse = ""))
cat("\n")

feature_ranks <- data.frame()
for (test_year in years) {
  train_years <- setdiff(years, test_year)
  train_idx <- data$year %in% train_years
  
  X_train <- as.matrix(data[train_idx, predictor_vars])
  y_train <- data$GY[train_idx]
  
  train_matrix <- xgb.DMatrix(data = X_train, label = y_train)
  xgb_model <- xgboost(
    data = train_matrix,
    objective = "reg:squarederror",
    nrounds = 500,
    eta = 0.1,
    max_depth = 3,
    verbose = 0
  )
  
  importance_matrix <- xgb.importance(model = xgb_model)
  top_10 <- head(importance_matrix, 10)
  
  for (j in 1:nrow(top_10)) {
    feature_ranks <- rbind(feature_ranks,
                           data.frame(Year = test_year,
                                      Feature = top_10$Feature[j],
                                      Rank = j,
                                      Gain = top_10$Gain[j]))
  }
}

consistent_features <- feature_ranks %>%
  group_by(Feature) %>%
  summarise(
    Years_Appeared = n(),
    Average_Rank = round(mean(Rank), 2),
    Average_Gain = round(mean(Gain), 4),
    Rank_Consistency = round(sd(Rank, na.rm = TRUE), 2)
  ) %>%
  arrange(Average_Rank)

print(consistent_features %>% head(10))

write.csv(performance_results, "final_figures/performance_metrics.csv", row.names = FALSE)
write.csv(consistent_features, "final_figures/feature_consistency.csv", row.names = FALSE)

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("OUTPUT FILES GENERATED:\n")
cat(paste(rep("-", 60), collapse = ""))
cat("\n")
cat("• Figure_S5_Feature_Importance.pdf (Publication quality)\n")
cat("• Figure_S5_Feature_Importance.tiff (High-resolution, 600 DPI)\n")
cat("• Figure_S5_Feature_Importance.png (Presentation quality)\n")
cat("• Figure_S5_Feature_Importance.jpeg (High quality)\n")
cat("• Figure_S5_Feature_Importance.eps (Vector format)\n")
cat("• Figure_S5_Feature_Importance.svg (Scalable vector)\n")
cat("• Individual year plots (all formats)\n")
cat("• performance_metrics.csv (Statistical results)\n")
cat("• feature_consistency.csv (Feature analysis)\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

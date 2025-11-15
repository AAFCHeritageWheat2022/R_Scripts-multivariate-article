setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd()

library(ggplot2)
library(dplyr)
library(car)
library(olsrr)
library(patchwork)
library(gridExtra)

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
      legend.position = "bottom",
      legend.box.spacing = unit(0.2, "cm"),

      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
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

df <- read.csv("Figure7_for regression analyses.csv")

df$newspecgroup <- factor(df$newspecgroup, levels = c("1", "2", "3", "4", "5"))
newspecgroup_colors <- c("1" = "orange", "2" = "brown", "3" = "#0072B2", 
                         "4" = "purple", "5" = "darkgreen")
newspecgroup_labels <- c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5")


create_regression_plot <- function(output_data, plot_title) {
  ggplot(output_data, aes(x = GY, y = predictions, color = newspecgroup)) +
    geom_point(size = 3, alpha = 0.8, stroke = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 0.8) +
    labs(title = plot_title,
         x = "Observed GY (kg/ha)",
         y = "Predicted GY (kg/ha)",
         color = "Groups") +
    scale_color_manual(values = newspecgroup_colors, labels = newspecgroup_labels) +
    unified_theme() +  # Apply unified theme
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )
}


perform_regression_analysis <- function(train_years, test_year, plot_label) {
  train_data <- subset(df, year %in% train_years)
  test_data <- subset(df, year == test_year)

  full_model <- lm(GY ~ TGW + GW + EPdL + CPdL + DTH + DTA + DTM + PHT + FLA + 
                     FLL + E_day.anth.. + NDVI_2WBH + NDVI_1WBH + NDVI_WH + 
                     CCI.anth.. + SL.ad.. + PL.ad.. + SL.ab.. + PL.ab.. + FD.ab.., 
                   data = train_data)

  stepwise_result <- ols_step_both_p(full_model, penter = 0.05, prem = 0.1)
  final_model <- stepwise_result$model

  selected_vars <- names(coef(final_model))[-1]

  if(length(selected_vars) > 0) {
    formula <- as.formula(paste("GY ~", paste(selected_vars, collapse = " + ")))
    op_model <- lm(formula, data = train_data)
  } else {
    op_model <- final_model
  }

  vif_values <- vif(op_model)

  predictions <- predict(op_model, test_data)
  output <- cbind(test_data, predictions)
  

  output$newspecgroup <- factor(output$newspecgroup, levels = c("1", "2", "3", "4", "5"))
  

  cor_test <- cor.test(output$GY, output$predictions)
  

  plot <- create_regression_plot(output, 
                                 paste0(plot_label, "\nR = ", 
                                        round(cor_test$estimate, 3),
                                        ", p = ", round(cor_test$p.value, 4)))
  
  return(list(plot = plot, 
              output = output, 
              cor_test = cor_test,
              vif_values = vif_values,
              selected_vars = selected_vars))
}

result_2023 <- perform_regression_analysis(c(2021, 2022), 2023, "Train: 2021-2022, Test: 2023")
 
result_2022 <- perform_regression_analysis(c(2021, 2023), 2022, "Train: 2021,2023, Test: 2022")

result_2021 <- perform_regression_analysis(c(2022, 2023), 2021, "Train: 2022-2023, Test: 2021")

combined_plot <- wrap_plots(
  A = result_2023$plot, 
  B = result_2022$plot, 
  C = result_2021$plot,
  ncol = 3,
  guides = 'collect'
) + 
  plot_annotation(tag_levels = 'A', 
                  tag_prefix = '(', tag_suffix = ')',
                  theme = theme(plot.title = element_text(size = 14, face = "bold"))) &
  theme(legend.position = 'bottom',
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 10))


print(combined_plot)

save_publication_formats(combined_plot, "regression_analysis_combined", width = 15, height = 5.5)

save_publication_formats(result_2023$plot, "regression_2023_test", width = 5, height = 4.5)
save_publication_formats(result_2022$plot, "regression_2022_test", width = 5, height = 4.5)
save_publication_formats(result_2021$plot, "regression_2021_test", width = 5, height = 4.5)

# Print summary statistics
cat("REGRESSION ANALYSIS SUMMARY\n")
cat("==========================\n\n")

cat("Analysis A (Test: 2023):\n")
cat("Selected variables:", paste(result_2023$selected_vars, collapse = ", "), "\n")
cat("Correlation: r =", round(result_2023$cor_test$estimate, 3), 
    ", p =", round(result_2023$cor_test$p.value, 4), "\n\n")

cat("Analysis B (Test: 2022):\n")
cat("Selected variables:", paste(result_2022$selected_vars, collapse = ", "), "\n")
cat("Correlation: r =", round(result_2022$cor_test$estimate, 3), 
    ", p =", round(result_2022$cor_test$p.value, 4), "\n\n")

cat("Analysis C (Test: 2021):\n")
cat("Selected variables:", paste(result_2021$selected_vars, collapse = ", "), "\n")
cat("Correlation: r =", round(result_2021$cor_test$estimate, 3), 
    ", p =", round(result_2021$cor_test$p.value, 4), "\n\n")

cat("All plots created and saved in publication formats!\n")




setwd()
getwd()


library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(ggrepel)
library(multcompView)

# ============================================================
# UNIFIED THEME FUNCTIONS
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
      legend.position = "right",
      legend.key = element_rect(fill = "white", color = NA),
      
      plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
}

# ============================================================
# SAVE FUNCTION
# ============================================================

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

# ============================================================
# PART 1: LOAD DATA
# ============================================================

dataforplot <- read.csv("FigureS4_graph of si ranking.csv", header = TRUE)

# Remove rows with NA in STI_GY
dataforplot <- dataforplot[!is.na(dataforplot$STI_GY), ]

# Ensure factors are set properly
dataforplot$variety <- factor(dataforplot$variety)
dataforplot$group <- factor(dataforplot$group)
dataforplot$ecozone <- factor(dataforplot$ecozone)

# ============================================================
# PART 2: DEFINE ECOZONE COLORS
# ============================================================

# Define colors for ecozones
ecozone_colors <- c(
  "Founder" = "#2E8B57",        # Green
  "Western" = "#DC143C",         # Light Red / Crimson
  "Eastern" = "#87CEEB"          # Light Blue / Sky Blue
)

# Filter to only available ecozones in data
available_ecozones <- unique(dataforplot$ecozone)
ecozone_colors <- ecozone_colors[names(ecozone_colors) %in% available_ecozones]

# ============================================================
# PART 3: STI_GY BY ECOZONE - ANOVA and Tukey
# ============================================================

cat("\n=== GENERATING STI_GY BY ECOZONE BOXPLOT ===\n")

# Perform ANOVA
anova_result <- aov(STI_GY ~ ecozone, data = dataforplot)
tukey_result <- TukeyHSD(anova_result)

# Get Tukey letters
tukey_pvalues <- tukey_result$ecozone[,4]
tukey_letters_raw <- multcompLetters(tukey_pvalues)

# Get means for each ecozone - USING BASE R INSTEAD OF DPLYR TO AVOID n() ISSUE
ecozone_means <- aggregate(STI_GY ~ ecozone, data = dataforplot, FUN = mean)
names(ecozone_means)[2] <- "mean_STI"
ecozone_means <- ecozone_means[order(ecozone_means$mean_STI, decreasing = TRUE), ]

# Prepare data for significance letters
group_max <- aggregate(STI_GY ~ ecozone, data = dataforplot, FUN = max)

# Create Tukey letter dataframe
tukey_df <- data.frame(
  ecozone = names(tukey_letters_raw$Letters),
  letters = as.character(tukey_letters_raw$Letters),
  stringsAsFactors = FALSE
)

# Merge with group_max for y positioning
tukey_df <- merge(tukey_df, group_max, by = "ecozone")
tukey_df$y_pos <- tukey_df$STI_GY + 0.02

# Add mean values - USE SIMPLE MERGE
tukey_df <- merge(tukey_df, ecozone_means, by = "ecozone")
tukey_df <- tukey_df[order(tukey_df$mean_STI, decreasing = TRUE), ]

cat("\nTukey Letters Assignment (by mean STI):\n")
print(tukey_df[, c("ecozone", "letters", "mean_STI")])

# ============================================================
# PART 4: STI_GY BY ECOZONE BOXPLOT WITH COLORS
# ============================================================

stigy_boxplot_colored <- ggplot(dataforplot, aes(x = ecozone, y = STI_GY, fill = ecozone)) +
  geom_boxplot(alpha = 0.85, outlier.size = 1.5, linewidth = 0.7, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2.5, color = "gray30") +
  
  # Add Tukey letters
  geom_text(data = tukey_df, 
            aes(x = ecozone, y = y_pos, label = letters), 
            size = 6, fontface = "bold", vjust = 0) +
  
  # Add mean values inside plot
  geom_text(data = tukey_df,
            aes(x = ecozone, y = mean_STI, 
                label = paste0("Mean = ", round(mean_STI, 3))),
            vjust = 2.5, size = 3.5, fontface = "italic", color = "black") +
  
  scale_fill_manual(values = ecozone_colors, name = "Ecozone") +
  
  labs(title = "Stress Tolerance Index (STI) for Grain Yield by Ecozone",
       subtitle = paste0("Different letters indicate significant differences (Tukey HSD, p < 0.05)\n",
                         "a = Highest mean, followed by b, c"),
       x = "Ecozone", 
       y = "Stress Tolerance Index (STI) for Grain Yield") +
  
  unified_theme(base_size = 12) +
  
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.box.background = element_rect(color = "black", linewidth = 0.5),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40")
  )

print(stigy_boxplot_colored)
save_publication_formats(stigy_boxplot_colored, "FigureS4_STI_GY_by_Ecozone_Colored", 
                         width = 10, height = 8)

# ============================================================
# PART 5: CLEAR TUKEY HSD DIFFERENCES TABLE
# ============================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("TUKEY HSD MULTIPLE COMPARISONS - CLEAR SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Extract Tukey results in a clear format
tukey_clear <- data.frame(
  Comparison = rownames(tukey_result$ecozone),
  Difference = round(tukey_result$ecozone[,1], 4),
  Lower_CI = round(tukey_result$ecozone[,2], 4),
  Upper_CI = round(tukey_result$ecozone[,3], 4),
  P_value = round(tukey_result$ecozone[,4], 6)
)

# Add significance stars
tukey_clear$Significance <- ifelse(tukey_clear$P_value < 0.001, "***",
                                   ifelse(tukey_clear$P_value < 0.01, "**",
                                          ifelse(tukey_clear$P_value < 0.05, "*", "ns")))

cat("\nTUKEY HSD RESULTS:\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
print(tukey_clear)
cat(paste(rep("-", 70), collapse = ""), "\n")
cat("\nSignificance codes: *** p < 0.001, ** p < 0.01, * p < 0.05, ns = non-significant\n")

# ============================================================
# PART 6: GROUP MEANS SUMMARY (USING BASE R)
# ============================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("GROUP MEANS SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Calculate group means using base R instead of dplyr
group_means_summary <- aggregate(STI_GY ~ ecozone, data = dataforplot, 
                                 FUN = function(x) c(n = length(x), Mean = mean(x), 
                                                     SD = sd(x), Min = min(x), Max = max(x)))
group_means_summary <- data.frame(
  ecozone = group_means_summary$ecozone,
  n = group_means_summary$STI_GY[, "n"],
  Mean = group_means_summary$STI_GY[, "Mean"],
  SD = group_means_summary$STI_GY[, "SD"],
  Min = group_means_summary$STI_GY[, "Min"],
  Max = group_means_summary$STI_GY[, "Max"]
)
group_means_summary$SE <- group_means_summary$SD / sqrt(group_means_summary$n)

# Add Tukey letters to group means
group_means_summary <- merge(group_means_summary, 
                             tukey_df[, c("ecozone", "letters")], 
                             by = "ecozone")

# Reorder by Mean
group_means_summary <- group_means_summary[order(group_means_summary$Mean, decreasing = TRUE), ]

cat("\nECOZONE MEANS WITH TUKEY LETTERS:\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
print(group_means_summary)
cat(paste(rep("-", 70), collapse = ""), "\n")

# Save group means summary
write.csv(group_means_summary, "FigureS4_STI_GY_Group_Means_with_Tukey.csv", row.names = FALSE)

# ============================================================
# PART 7: COMPLETE ANOVA AND TUKEY RESULTS
# ============================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("COMPLETE ANOVA RESULTS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("\nANOVA SUMMARY:\n")
print(summary(anova_result))

cat("\nCOMPLETE TUKEY HSD RESULTS:\n")
print(tukey_result)

# ============================================================
# PART 8: SAVE FULL RESULTS TO FILE
# ============================================================

sink("FigureS4_Ecozone_STI_Analysis_Results.txt")

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("STI_GY BY ECOZONE - COMPLETE ANALYSIS\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("ECOZONE MEANS WITH TUKEY LETTERS:\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
print(group_means_summary)
cat(paste(rep("-", 70), collapse = ""), "\n\n")

cat("ANOVA SUMMARY:\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
print(summary(anova_result))
cat(paste(rep("-", 70), collapse = ""), "\n\n")

cat("TUKEY HSD RESULTS:\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
print(tukey_result)
cat(paste(rep("-", 70), collapse = ""), "\n\n")

cat("CLEAR TUKEY COMPARISONS:\n")
cat(paste(rep("-", 70), collapse = ""), "\n")
print(tukey_clear)
cat(paste(rep("-", 70), collapse = ""), "\n\n")

cat("\nSignificance codes: *** p < 0.001, ** p < 0.01, * p < 0.05, ns = non-significant\n")

sink()

# ============================================================
# PART 9: VISUAL SUMMARY OF TUKEY DIFFERENCES
# ============================================================

# Create a bar plot of means with Tukey letters
stigy_barplot <- ggplot(group_means_summary, aes(x = ecozone, y = Mean, fill = ecozone)) +
  geom_bar(stat = "identity", alpha = 0.85, color = "black", linewidth = 0.5) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.2, size = 0.6) +
  geom_text(aes(label = letters, y = Mean + SE + 0.02), 
            size = 6, fontface = "bold", vjust = 0) +
  geom_text(aes(label = round(Mean, 3), y = Mean/2), 
            size = 4.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = ecozone_colors, name = "Ecozone") +
  labs(title = "Mean STI_GY by Ecozone with Tukey Letters",
       subtitle = "Error bars indicate ± Standard Error",
       x = "Ecozone", 
       y = "Mean Stress Tolerance Index (STI) for Grain Yield") +
  unified_theme(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40")
  )

print(stigy_barplot)
save_publication_formats(stigy_barplot, "FigureS4_STI_GY_Means_Barplot", 
                         width = 8, height = 7)

# ============================================================
# PART 10: FINAL SUMMARY
# ============================================================

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("FIGURE S4 - STI_GY BY ECOZONE ANALYSIS COMPLETED SUCCESSFULLY\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("\nFILES CREATED:\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("  FigureS4_STI_GY_by_Ecozone_Colored.{tiff,pdf,png,jpeg}\n")
cat("  FigureS4_STI_GY_Means_Barplot.{tiff,pdf,png,jpeg}\n")
cat("  FigureS4_STI_GY_Group_Means_with_Tukey.csv\n")
cat("  FigureS4_Ecozone_STI_Analysis_Results.txt\n")

cat("\nCOLOR SCHEME USED:\n")
cat(paste(rep("-", 40), collapse = ""), "\n")
cat("  Founder: #2E8B57 (Green)\n")
cat("  Western: #DC143C (Light Red / Crimson)\n")
cat("  Eastern: #87CEEB (Light Blue / Sky Blue)\n")

cat("\nTUKEY LETTERS CONVENTION:\n")
cat(paste(rep("-", 40), collapse = ""), "\n")
cat("  'a' = Highest mean value\n")
cat("  'b' = Second highest mean value\n")
cat("  'c' = Third highest mean value\n")

cat("\nSTATISTICAL TESTS PERFORMED:\n")
cat(paste(rep("-", 40), collapse = ""), "\n")
cat("  ANOVA: STI_GY ~ Ecozone\n")
cat("  Tukey HSD Post-hoc test for multiple comparisons\n")

# Print summary means
cat("\nECOZONE MEANS SUMMARY:\n")
cat(paste(rep("-", 40), collapse = ""), "\n")
for(i in 1:nrow(group_means_summary)) {
  cat(sprintf("  %s: n = %d, Mean = %.4f, Letters = %s\n", 
              group_means_summary$ecozone[i],
              group_means_summary$n[i],
              group_means_summary$Mean[i],
              group_means_summary$letters[i]))
}

cat("\nAll plots use unified theme matching Figures 3, 4, and 5!\n")
cat("All outputs saved with 600 DPI resolution!\n")


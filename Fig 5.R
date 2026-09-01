setwd()
getwd()


# ============================================================
# FIGURE 5: Superiority Index Ranking by Variety
# ============================================================


library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(ggrepel)

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
        margin = margin(b = 10)
      ),
      
      plot.subtitle = element_text(
        size = base_size,
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
      legend.key = element_rect(fill = "white", color = NA),
      
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
}

barplot_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
      axis.text.y = element_text(size = 10),
      panel.grid.major.y = element_line(color = "gray80", size = 0.3),
      panel.grid.major.x = element_blank()
    )
}

horizontal_bar_theme <- function() {
  unified_theme() +
    theme(
      axis.text.y = element_text(size = 9, face = "bold"),
      axis.text.x = element_text(size = 10),
      panel.grid.major.x = element_line(color = "gray80", size = 0.3),
      panel.grid.major.y = element_blank()
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

dataforplot <- read.csv("Figure5_graph of si ranking.csv", header = TRUE)

# Remove rows with NA in Superiority index
dataforplot <- dataforplot[!is.na(dataforplot$Superiority.index), ]

# Ensure factors are set properly
dataforplot$variety <- factor(dataforplot$variety)
dataforplot$group <- factor(dataforplot$group)

# ============================================================
# PART 2: DEFINE THREE-COLOR SCHEME
# ============================================================

# Define consistent three colors
group_colors <- c(
  "High STI GY" = "#2E8B57",        # Green
  "Intermediate STI GY" = "#6A0DAD", # Purple
  "Low STI GY" = "#B22222"          # Brick Red
)

# Filter to only available groups in data
available_groups <- unique(dataforplot$group)
group_colors <- group_colors[names(group_colors) %in% available_groups]

# If Founder, Western, Eastern etc. are present, add them with additional colors
extra_groups <- setdiff(available_groups, names(group_colors))
if(length(extra_groups) > 0) {
  extra_colors <- c(
    "Founder" = "#8B4513",
    "Western" = "#1f77b4",
    "Eastern" = "#ff7f0e"
  )
  extra_colors <- extra_colors[names(extra_colors) %in% extra_groups]
  group_colors <- c(group_colors, extra_colors)
}

# ============================================================
# PART 3: CALCULATE GROUP MEANS
# ============================================================

group_means <- dataforplot %>%
  group_by(group) %>%
  summarise(
    Superiority.index_mean = mean(Superiority.index, na.rm = TRUE),
    n = n(),
    sd = sd(Superiority.index, na.rm = TRUE)
  )

# ============================================================
# PART 4: MAIN VERTICAL BAR PLOT
# ============================================================

cat("\n=== GENERATING MAIN VERTICAL BAR PLOT ===\n")

# Identify varieties to highlight (Superb and Red Fife)
highlight_varieties <- c("Superb", "Red Fife")
dataforplot$highlight <- ifelse(dataforplot$variety %in% highlight_varieties, "Highlight", "Other")

superiority_plot <- ggplot(dataforplot, aes(x = reorder(variety, -Superiority.index), y = Superiority.index)) +
  # Add bars
  geom_bar(aes(fill = group), stat = "identity", width = 0.7, alpha = 0.85) +
  
  # Add group mean lines
  geom_hline(data = group_means, 
             aes(yintercept = Superiority.index_mean, color = group), 
             linetype = "dashed", linewidth = 1, alpha = 0.8) +
  
  # Add value labels on bars
  geom_text(aes(label = round(Superiority.index, 2)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.2, color = "black", fontface = "bold") +
  
  # Highlight Superb and Red Fife with borders
  geom_bar(data = dataforplot %>% filter(highlight == "Highlight"),
           aes(x = reorder(variety, -Superiority.index), y = Superiority.index),
           stat = "identity", width = 0.7, 
           color = "black", fill = NA, size = 1.5) +
  
  # Add labels for highlighted varieties
  geom_text(data = dataforplot %>% filter(highlight == "Highlight"),
            aes(x = reorder(variety, -Superiority.index), 
                y = Superiority.index, 
                label = "*"),
            vjust = -0.8, size = 8, color = "black", fontface = "bold") +
  
  # Scale colors
  scale_fill_manual(values = group_colors, name = "STI Group") +
  scale_color_manual(values = group_colors, guide = "none") +
  
  # Labels
  labs(
    title = "Superiority Index Ranking by Variety",
    subtitle = "Group means shown as dashed lines; * indicates highlighted varieties (Superb, Red Fife)",
    x = "Variety",
    y = "Superiority Index"
  ) +
  
  barplot_theme() +
  
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.box.background = element_rect(color = "black", linewidth = 0.5)
  )

print(superiority_plot)
save_publication_formats(superiority_plot, "Figure5_Superiority_Index_Ranking", width = 16, height = 10)

# ============================================================
# PART 5: HORIZONTAL BAR PLOT
# ============================================================

cat("\n=== GENERATING HORIZONTAL BAR PLOT ===\n")

superiority_plot_horizontal <- ggplot(dataforplot, aes(x = reorder(variety, Superiority.index), y = Superiority.index)) +
  # Add bars
  geom_bar(aes(fill = group), stat = "identity", width = 0.7, alpha = 0.85) +
  
  # Add group mean lines
  geom_vline(data = group_means, 
             aes(xintercept = Superiority.index_mean, color = group), 
             linetype = "dashed", linewidth = 1, alpha = 0.8) +
  
  # Add value labels
  geom_text(aes(label = round(Superiority.index, 2)), 
            hjust = ifelse(dataforplot$Superiority.index >= 0, -0.2, 1.2), 
            size = 3.2, color = "black", fontface = "bold") +
  
  # Highlight Superb and Red Fife with borders
  geom_bar(data = dataforplot %>% filter(highlight == "Highlight"),
           aes(x = reorder(variety, Superiority.index), y = Superiority.index),
           stat = "identity", width = 0.7, 
           color = "black", fill = NA, size = 1.5) +
  
  # Scale colors
  scale_fill_manual(values = group_colors, name = "STI Group") +
  scale_color_manual(values = group_colors, guide = "none") +
  
  # Labels
  labs(
    title = "Superiority Index Ranking by Variety",
    subtitle = "Horizontal view with group means; Highlighted varieties: Superb, Red Fife",
    x = "Superiority Index",
    y = "Variety"
  ) +
  
  coord_flip() +
  horizontal_bar_theme() +
  
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.box.background = element_rect(color = "black", linewidth = 0.5)
  )

print(superiority_plot_horizontal)
save_publication_formats(superiority_plot_horizontal, "Figure5_Superiority_Index_Ranking_Horizontal", 
                         width = 14, height = 12)

# ============================================================
# PART 6: FACETED PLOT BY GROUP
# ============================================================

cat("\n=== GENERATING FACETED PLOT ===\n")

superiority_plot_faceted <- ggplot(dataforplot, aes(x = reorder(variety, -Superiority.index), y = Superiority.index)) +
  # Add bars
  geom_bar(aes(fill = group), stat = "identity", alpha = 0.85) +
  
  # Add group mean lines
  geom_hline(data = group_means, 
             aes(yintercept = Superiority.index_mean), 
             color = "red", linetype = "dashed", linewidth = 1) +
  
  # Add value labels
  geom_text(aes(label = round(Superiority.index, 2)), 
            vjust = -0.5, size = 2.8, color = "black", fontface = "bold") +
  
  # Highlight Superb and Red Fife
  geom_bar(data = dataforplot %>% filter(highlight == "Highlight"),
           aes(x = reorder(variety, -Superiority.index), y = Superiority.index),
           stat = "identity", 
           color = "black", fill = NA, size = 1.5) +
  
  # Facet by group
  facet_wrap(~ group, scales = "free_x", ncol = 1) +
  
  # Scale colors
  scale_fill_manual(values = group_colors, name = "STI Group") +
  
  # Labels
  labs(
    title = "Superiority Index Ranking by STI Group",
    subtitle = "Separated by STI classification; Highlighted varieties: Superb, Red Fife",
    x = "Variety",
    y = "Superiority Index"
  ) +
  
  unified_theme() +
  
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "none"
  )

print(superiority_plot_faceted)
save_publication_formats(superiority_plot_faceted, "Figure5_Superiority_Index_Ranking_Faceted", 
                         width = 12, height = 14)

# ============================================================
# PART 7: DOT PLOT VARIANT
# ============================================================

cat("\n=== GENERATING DOT PLOT ===\n")

superiority_dot_plot <- ggplot(dataforplot, aes(x = reorder(variety, -Superiority.index), y = Superiority.index)) +
  # Add points
  geom_point(aes(color = group), size = 4, alpha = 0.9) +
  
  # Add segments from 0 to point
  geom_segment(aes(xend = reorder(variety, -Superiority.index), yend = 0, color = group), 
               alpha = 0.4, size = 0.8) +
  
  # Add group mean lines
  geom_hline(data = group_means, 
             aes(yintercept = Superiority.index_mean, color = group), 
             linetype = "dashed", linewidth = 1, alpha = 0.8) +
  
  # Add value labels
  geom_text(aes(label = round(Superiority.index, 2)), 
            vjust = -0.8, size = 3, color = "black", fontface = "bold") +
  
  # Highlight Superb and Red Fife
  geom_point(data = dataforplot %>% filter(highlight == "Highlight"),
             aes(x = reorder(variety, -Superiority.index), y = Superiority.index),
             color = "black", size = 6, shape = 1, stroke = 1.5) +
  
  # Scale colors
  scale_color_manual(values = group_colors, name = "STI Group") +
  
  # Labels
  labs(
    title = "Superiority Index - Dot Plot",
    subtitle = "Group means shown as dashed lines; Highlighted: Superb, Red Fife (circled)",
    x = "Variety",
    y = "Superiority Index"
  ) +
  
  barplot_theme() +
  
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.box.background = element_rect(color = "black", linewidth = 0.5)
  )

print(superiority_dot_plot)
save_publication_formats(superiority_dot_plot, "Figure5_Superiority_Index_Dot_Plot", 
                         width = 16, height = 10)

# ============================================================
# PART 8: SUMMARY STATISTICS
# ============================================================

cat("\n=== STATISTICAL SUMMARY BY STI GROUP ===\n")
summary_stats <- dataforplot %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = mean(Superiority.index, na.rm = TRUE),
    sd = sd(Superiority.index, na.rm = TRUE),
    min = min(Superiority.index, na.rm = TRUE),
    max = max(Superiority.index, na.rm = TRUE)
  )

print(summary_stats)
write.csv(summary_stats, "Figure5_Superiority_Index_Summary_Stats.csv", row.names = FALSE)

# ============================================================
# PART 9: ANOVA AND POST-HOC TESTS
# ============================================================

if(length(unique(dataforplot$group)) > 1) {
  anova_result <- aov(Superiority.index ~ group, data = dataforplot)
  cat("\n=== ANOVA RESULTS ===\n")
  print(summary(anova_result))
  
  tukey_result <- TukeyHSD(anova_result)
  cat("\n=== TUKEY HSD POST-HOC TEST ===\n")
  print(tukey_result)
  
  capture.output(summary(anova_result), file = "Figure5_ANOVA_Results.txt")
  capture.output(tukey_result, file = "Figure5_Tukey_HSD_Results.txt")
}

# ============================================================
# PART 10: SUMMARY OF OUTPUT
# ============================================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("FIGURE 5 - ANALYSIS COMPLETED SUCCESSFULLY\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\nFiles created:\n")
cat("  Figure5_Superiority_Index_Ranking.{tiff,pdf,png,jpeg}\n")
cat("  Figure5_Superiority_Index_Ranking_Horizontal.{tiff,pdf,png,jpeg}\n")
cat("  Figure5_Superiority_Index_Ranking_Faceted.{tiff,pdf,png,jpeg}\n")
cat("  Figure5_Superiority_Index_Dot_Plot.{tiff,pdf,png,jpeg}\n")
cat("  Figure5_Superiority_Index_Summary_Stats.csv\n")
cat("  Figure5_ANOVA_Results.txt\n")
cat("  Figure5_Tukey_HSD_Results.txt\n")

cat("\nColor scheme used (same as Figure 3):\n")
cat("  High STI GY: #2E8B57 (Green)\n")
cat("  Intermediate STI GY: #6A0DAD (Purple)\n")
cat("  Low STI GY: #B22222 (Brick Red)\n")

cat("\nHighlighted varieties:\n")
cat("  Superb - High STI GY\n")
cat("  Red Fife - Low STI GY\n")

cat("\nAll plots use unified theme matching Figures 3 and 4!\n")
cat("All outputs saved with 600 DPI resolution!\n")


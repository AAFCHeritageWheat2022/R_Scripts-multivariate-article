
setwd("//Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd()

library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)

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

dataforplot <- read.csv("Figure5_graph of si ranking.csv", header = TRUE)

dataforplot$variety <- factor(dataforplot$variety)
dataforplot$group <- factor(dataforplot$group)

group_means <- dataforplot %>%
  group_by(group) %>%
  summarise(Superiority.index_mean = mean(Superiority.index, na.rm = TRUE))

dataforplot <- dataforplot %>%
  left_join(group_means, by = "group")

cat("Available groups in your data:\n")
print(unique(dataforplot$group))


group_colors <- c("1" = "#0072B2", 
                  "2" = "darkgreen", 
                  "3" = "darkred",
                  "4" = "purple",
                  "5" = "orange",
                  "Founder" = "brown",
                  "Western" = "blue",
                  "Eastern" = "green")

available_groups <- unique(dataforplot$group)
group_colors <- group_colors[names(group_colors) %in% available_groups]

superiority_plot <- ggplot(dataforplot, aes(x = reorder(variety, -Superiority.index), y = Superiority.index)) +
  geom_bar(aes(fill = group), stat = "identity", width = 0.7, alpha = 0.8) +
  geom_hline(data = group_means, 
             aes(yintercept = Superiority.index_mean, color = group), 
             linetype = "dashed", linewidth = 1, alpha = 0.8) +
  geom_text(aes(label = round(Superiority.index, 2)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.2, color = "black", fontface = "bold") +
  geom_text(data = group_means, 
            aes(x = Inf, y = Superiority.index_mean, 
                label = paste("Group", group, "Mean:", round(Superiority.index_mean, 2)),
                color = group),
            hjust = 1.1, vjust = -0.5, size = 3.5, fontface = "bold",
            show.legend = FALSE) +
  scale_fill_manual(values = group_colors, name = "Group") +
  scale_color_manual(values = group_colors, guide = "none") +
  labs(
    title = "Superiority Index Ranking by Variety",
    subtitle = "Group means shown as dashed lines",
    x = "Variety",
    y = "Superiority Index"
  ) +
  barplot_theme()

print(superiority_plot)

save_publication_formats(superiority_plot, "Superiority_Index_Ranking_Group", width = 14, height = 8)

superiority_plot_horizontal <- ggplot(dataforplot, aes(x = reorder(variety, Superiority.index), y = Superiority.index)) +
  geom_bar(aes(fill = group), stat = "identity", width = 0.7, alpha = 0.8) +
  geom_hline(data = group_means, 
             aes(yintercept = Superiority.index_mean, color = group), 
             linetype = "dashed", linewidth = 1, alpha = 0.8) +
  geom_text(aes(label = round(Superiority.index, 2)), 
            hjust = ifelse(dataforplot$Superiority.index >= 0, -0.1, 1.1), 
            size = 3.2, color = "black", fontface = "bold") +
  scale_fill_manual(values = group_colors, name = "Group") +
  scale_color_manual(values = group_colors, guide = "none") +
  labs(
    title = "Superiority Index Ranking by Variety",
    subtitle = "Horizontal view with group means",
    x = "Superiority Index",
    y = "Variety"
  ) +
  coord_flip() +
  horizontal_bar_theme()

print(superiority_plot_horizontal)
save_publication_formats(superiority_plot_horizontal, "Superiority_Index_Ranking_Horizontal_Group", width = 12, height = 10)

superiority_plot_faceted <- ggplot(dataforplot, aes(x = reorder(variety, -Superiority.index), y = Superiority.index)) +
  geom_bar(aes(fill = group), stat = "identity", alpha = 0.8) +
  geom_hline(data = group_means, 
             aes(yintercept = Superiority.index_mean), 
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_text(aes(label = round(Superiority.index, 2)), 
            vjust = -0.5, size = 2.8, color = "black", fontface = "bold") +
  facet_wrap(~ group, scales = "free_x", ncol = 1) +
  scale_fill_manual(values = group_colors, name = "Group") +
  labs(
    title = "Superiority Index Ranking by Group",
    subtitle = "Separated by genetic groups",
    x = "Variety",
    y = "Superiority Index"
  ) +
  unified_theme() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    strip.text = element_text(face = "bold", size = 11)
  )

print(superiority_plot_faceted)
save_publication_formats(superiority_plot_faceted, "Superiority_Index_Ranking_Faceted_Group", width = 12, height = 12)

superiority_dot_plot <- ggplot(dataforplot, aes(x = reorder(variety, -Superiority.index), y = Superiority.index)) +
  geom_point(aes(color = group), size = 3, alpha = 0.8) +
  geom_segment(aes(xend = reorder(variety, -Superiority.index), yend = 0, color = group), 
               alpha = 0.6) +
  geom_hline(data = group_means, 
             aes(yintercept = Superiority.index_mean, color = group), 
             linetype = "dashed", linewidth = 1) +
  geom_text(aes(label = round(Superiority.index, 2)), 
            vjust = -0.8, size = 3, color = "black", fontface = "bold") +
  scale_color_manual(values = group_colors, name = "Group") +
  labs(
    title = "Superiority Index - Dot Plot",
    subtitle = "Group means shown as dashed lines",
    x = "Variety",
    y = "Superiority Index"
  ) +
  barplot_theme()

print(superiority_dot_plot)
save_publication_formats(superiority_dot_plot, "Superiority_Index_Dot_Plot_Group", width = 14, height = 8)

cat("=== STATISTICAL SUMMARY BY GROUP ===\n")
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
write.csv(summary_stats, "Superiority_Index_Summary_Stats_Group.csv", row.names = FALSE)

if(length(unique(dataforplot$group)) > 1) {
  anova_result <- aov(Superiority.index ~ group, data = dataforplot)
  cat("\n=== ANOVA RESULTS (by Group) ===\n")
  print(summary(anova_result))

  tukey_result <- TukeyHSD(anova_result)
  cat("\n=== TUKEY HSD POST-HOC TEST ===\n")
  print(tukey_result)

  capture.output(summary(anova_result), file = "ANOVA_Results_Group.txt")
  capture.output(tukey_result, file = "Tukey_HSD_Results_Group.txt")
}

cat("\n=== ANALYSIS COMPLETED SUCCESSFULLY ===\n")
cat("Files created:\n")
cat("- Superiority_Index_Ranking_Group.{tiff,pdf,png,jpeg}\n")
cat("- Superiority_Index_Ranking_Horizontal_Group.{tiff,pdf,png,jpeg}\n")
cat("- Superiority_Index_Ranking_Faceted_Group.{tiff,pdf,png,jpeg}\n")
cat("- Superiority_Index_Dot_Plot_Group.{tiff,pdf,png,jpeg}\n")
cat("- Superiority_Index_Summary_Stats_Group.csv\n")
cat("- ANOVA_Results_Group.txt\n")
cat("- Tukey_HSD_Results_Group.txt\n")
cat("\nAll plots use unified theme matching Figure 1 and are saved with 600 DPI resolution!\n")


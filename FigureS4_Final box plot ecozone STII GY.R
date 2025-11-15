setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd() 

library(ggplot2)
library(dplyr)
library(multcompView)


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
      legend.position = "none", # Default to no legend like Figure S9

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

dataforplot <- read.csv("FigureS4_graph of si ranking.csv", header = TRUE)


dataforplot$variety <- factor(dataforplot$variety)
dataforplot$group <- factor(dataforplot$group)
dataforplot$ecozone <- factor(dataforplot$ecozone, levels = c("Founder", "Eastern", "Western"))


ecozone_colors <- c("Founder" = "darkgreen", "Eastern" = "#0072B2", "Western" = "darkred")


anova_result <- aov(STI_GY ~ ecozone, data = dataforplot)
tukey_result <- TukeyHSD(anova_result)


tukey_pvalues <- tukey_result$ecozone[,4]


tukey_letters <- multcompLetters(tukey_pvalues)


group_means <- aggregate(STI_GY ~ ecozone, data = dataforplot, FUN = mean)
group_max <- aggregate(STI_GY ~ ecozone, data = dataforplot, FUN = max)

tukey_df <- data.frame(
  ecozone = names(tukey_letters$Letters),
  letters = as.character(tukey_letters$Letters)
)

tukey_df <- merge(tukey_df, group_max, by = "ecozone")
tukey_df$y_pos <- tukey_df$STI_GY + 0.03


p <- ggplot(dataforplot, aes(x = ecozone, y = STI_GY, fill = ecozone)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1.5, linewidth = 0.5) +
  geom_text(data = tukey_df, aes(x = ecozone, y = y_pos, label = letters), 
            size = 5, fontface = "bold", vjust = 0) +
  scale_fill_manual(values = ecozone_colors) +
  labs(title = "Stress Tolerance Index (STI) for Grain Yield by Ecozone",
       x = "Ecozones", 
       y = "Stress Tolerance Index (STI) Grain Yield") +
  unified_theme(base_size = 12, base_family = "sans") +  
  ylim(0.3, 0.65)


print(p)


save_publication_formats(p, "STI_GY_ecozone_comparison", width = 8, height = 6)


cat("ANOVA Results:\n")
print(summary(anova_result))
cat("\nTukey HSD Results:\n")
print(tukey_result)
cat("\nSignificance Letters:\n")
print(tukey_df)

cat("Plot created and saved in all publication formats!\n")


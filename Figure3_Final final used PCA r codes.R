setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd()

library(FactoMineR)
library(factoextra)
library(tidyverse)
library(ggpubr)
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
      panel.grid.major.x = element_line(color = "gray80", size = 0.3),
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

pca_theme <- function() {
  unified_theme() +
    theme(
      panel.grid.major = element_line(color = "gray80", size = 0.3),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
}


save_publication_formats <- function(plot_obj, base_name, width = 10, height = 8) {
  
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

df <- read.csv("Figure3_Fig3_for pca v2 3yravg.csv", header = TRUE)

df1 <- select(df, newspecgroup, variety, GY:FD_ab)

pca <- PCA(data.frame(df1[,-c(1:2)], row.names = df1$variety), 
           ncp = 7, graph = FALSE, scale.unit = TRUE)

pca_plot <- fviz_pca_biplot(pca, 
                            geom = c("point", "text"), 
                            addEllipses = TRUE, 
                            ellipse.level = 0.7,
                            ellipse.type = "confidence",
                            ellipse.alpha = 0.1,
                            mean.point = FALSE,
                            pointsize = 2.5,
                            labelsize = 3.5,
                            habillage = as.factor(df1$newspecgroup),
                            palette = c("1" = "#FF6B00", 
                                        "2" = "#8B4513", 
                                        "3" = "#0072B2", 
                                        "4" = "#6A0DAD", 
                                        "5" = "#2E8B57"),
                            col.var = "darkred",
                            alpha.var = 0.8,
                            repel = TRUE,
                            ggtheme = pca_theme(),
                            title = "Principal Component Analysis of Wheat Traits",
                            subtitle = "Grouped by specific clusters") +
  labs(color = "Cluster Group", fill = "Cluster Group",
       x = "Dim1 (%)", y = "Dim2 (%)") +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  xlab(paste0("Dim1 (", round(get_eigenvalue(pca)[1,2], 1), "%)")) +
  ylab(paste0("Dim2 (", round(get_eigenvalue(pca)[2,2], 1), "%)"))

print(pca_plot)

save_publication_formats(pca_plot, "PCA_Biplot_Publication_Quality", width = 12, height = 9)

pca_enhanced <- fviz_pca_biplot(pca, 
                                geom = c("point", "text"), 
                                addEllipses = TRUE, 
                                ellipse.level = 0.7,
                                ellipse.type = "norm",
                                ellipse.linewidth = 0.8,
                                mean.point = FALSE,
                                pointsize = 3,
                                labelsize = 4,
                                habillage = as.factor(df1$newspecgroup),
                                palette = c("1" = "#FF6B00", 
                                            "2" = "#8B4513", 
                                            "3" = "#0072B2", 
                                            "4" = "#6A0DAD", 
                                            "5" = "#2E8B57"),
                                col.var = "#D32F2F",
                                alpha.var = 0.9,
                                repel = TRUE,
                                ggtheme = pca_theme(),
                                title = "Principal Component Analysis - Biplot",
                                subtitle = "Showing genotypes and traits colored by cluster groups") +
  labs(color = "Cluster Group", fill = "Cluster Group",
       x = paste0("Dim1 (", round(get_eigenvalue(pca)[1,2], 1), "%)"), 
       y = paste0("Dim2 (", round(get_eigenvalue(pca)[2,2], 1), "%)")) +
  theme(
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.box.background = element_rect(color = "black", linewidth = 0.5)
  )

print(pca_enhanced)
save_publication_formats(pca_enhanced, "PCA_Biplot_Enhanced", width = 12, height = 9)

ind_plot <- fviz_pca_ind(pca,
                         geom = c("point", "text"),
                         habillage = as.factor(df1$newspecgroup),
                         palette = c("1" = "#FF6B00", 
                                     "2" = "#8B4513", 
                                     "3" = "#0072B2", 
                                     "4" = "#6A0DAD", 
                                     "5" = "#2E8B57"),
                         addEllipses = TRUE,
                         ellipse.level = 0.7,
                         pointsize = 2.5,
                         labelsize = 3.5,
                         repel = TRUE,
                         ggtheme = pca_theme(),
                         title = "PCA - Individual Factor Map",
                         subtitle = "Colored by cluster groups") +
  labs(color = "Cluster Group", fill = "Cluster Group",
       x = paste0("Dim1 (", round(get_eigenvalue(pca)[1,2], 1), "%)"), 
       y = paste0("Dim2 (", round(get_eigenvalue(pca)[2,2], 1), "%)"))

var_plot <- fviz_pca_var(pca,
                         col.var = "contrib",
                         gradient.cols = viridis(3, option = "D"),
                         repel = TRUE,
                         ggtheme = pca_theme(),
                         title = "PCA - Variable Factor Map",
                         subtitle = "Variables colored by contribution") +
  labs(x = paste0("Dim1 (", round(get_eigenvalue(pca)[1,2], 1), "%)"), 
       y = paste0("Dim2 (", round(get_eigenvalue(pca)[2,2], 1), "%)")) +
  scale_color_viridis_c(name = "Contribution", option = "D")

print(ind_plot)
print(var_plot)

save_publication_formats(ind_plot, "PCA_Individuals_Plot", width = 10, height = 8)
save_publication_formats(var_plot, "PCA_Variables_Plot", width = 10, height = 8)

cat("\n=== PCA SUMMARY ===\n")
print(summary(pca))

variance_explained <- get_eigenvalue(pca)
cat("\n=== VARIANCE EXPLAINED ===\n")
print(variance_explained)

cat("\nAll PCA plots created and saved in publication formats with unified theme!\n")

setwd()
getwd()



library(FactoMineR)
library(factoextra)
library(tidyverse)
library(ggpubr)
library(viridis)
library(ggrepel)

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

# Save function remains the same
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

# Read the data
df <- read.csv("Figure3_Fig3_for pca v2 3yravg.csv", header = TRUE)

# Select only the relevant columns (excluding group and variety for PCA)
df1 <- select(df, newspecgroup, variety, GY:FD_ab)

# Remove rows with all NA values in trait columns (metadata rows)
df1 <- df1[!apply(is.na(df1[, -c(1:2)]), 1, all), ]

# Perform PCA
pca <- PCA(data.frame(df1[,-c(1:2)], row.names = df1$variety), 
           ncp = 7, graph = FALSE, scale.unit = TRUE)

# Define the three consistent colors
group_colors <- c(
  "High STI GY" = "#2E8B57",      # Green
  "Intermediate STI GY" = "#6A0DAD", # Purple
  "Low STI GY" = "#B22222"        # Brick Red
)

# Get group means for centroids
group_means <- aggregate(pca$ind$coord[,1:2], 
                         by = list(Group = df1$newspecgroup), 
                         FUN = mean)

# Extract coordinates for plotting
ind_coord <- as.data.frame(pca$ind$coord)
var_coord <- as.data.frame(pca$var$coord)

# Calculate variable contribution and scaling factor
var_contrib <- get_pca_var(pca)$contrib
var_quality <- rowSums(var_contrib[,1:2])
var_coord_scaled <- var_coord * 3.8  # Increased scaling to 3.8x

# Create individual plot with points colored by group
ind_plot_data <- cbind(ind_coord, Group = df1$newspecgroup)

# Function to create biplot with expanded variables
create_biplot <- function(data, var_coord_scaled, group_means, group_colors, 
                          title, subtitle, xlab, ylab, point_size = 3, 
                          var_label_size = 4.5, variety_size = 3.5) {
  
  # Create the base plot
  p <- ggplot() +
    # Add individual points
    geom_point(data = data,
               aes(x = Dim.1, y = Dim.2, color = Group),
               size = point_size, alpha = 0.8) +
    # Add variety labels in bold black with repel
    geom_text_repel(data = data,
                    aes(x = Dim.1, y = Dim.2, label = rownames(data)),
                    color = "black", size = variety_size, fontface = "bold",
                    box.padding = 0.3, point.padding = 0.2,
                    segment.color = 'grey50', segment.size = 0.2,
                    max.overlaps = Inf) +
    # Add variable vectors with solid lines and expanded coordinates
    geom_segment(data = var_coord_scaled,
                 aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
                 arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
                 color = "gray30", size = 0.8, linetype = "solid") +
    # Add variable labels in pink with repel to avoid overlap
    geom_text_repel(data = var_coord_scaled,
                    aes(x = Dim.1, y = Dim.2, label = rownames(var_coord_scaled)),
                    color = "#FF69B4", size = var_label_size, fontface = "bold",
                    box.padding = 0.5, point.padding = 0.3,
                    segment.color = '#FF69B4', segment.size = 0.2,
                    max.overlaps = Inf) +
    # Add group means as larger points
    geom_point(data = group_means, 
               aes(x = Dim.1, y = Dim.2, color = Group),
               size = 7, shape = 18, show.legend = FALSE) +
    # Add group mean labels
    geom_text_repel(data = group_means,
                    aes(x = Dim.1, y = Dim.2, label = Group, color = Group),
                    size = 5, fontface = "bold",
                    box.padding = 0.5, point.padding = 0.3,
                    nudge_y = 0.3, show.legend = FALSE) +
    # Scale colors
    scale_color_manual(values = group_colors, name = "STI Group") +
    # Theme and labels
    pca_theme() +
    labs(title = title,
         subtitle = subtitle,
         x = xlab,
         y = ylab) +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11),
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      legend.box.background = element_rect(color = "black", linewidth = 0.5),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    ) +
    coord_fixed(ratio = 1)
  
  return(p)
}

# MAIN PCA BIPLOT - With expanded variables (3.8x) and solid lines
pca_plot <- create_biplot(
  data = ind_plot_data,
  var_coord_scaled = var_coord_scaled,
  group_means = group_means,
  group_colors = group_colors,
  title = "Principal Component Analysis of Wheat Traits",
  subtitle = "Grouped by STI Classification",
  xlab = paste0("Dim1 (", round(get_eigenvalue(pca)[1,2], 1), "%)"),
  ylab = paste0("Dim2 (", round(get_eigenvalue(pca)[2,2], 1), "%)"),
  point_size = 3,
  var_label_size = 5,
  variety_size = 3.5
)

print(pca_plot)
save_publication_formats(pca_plot, "PCA_Biplot_Publication_Quality", width = 14, height = 10)

# ENHANCED PCA BIPLOT - With expanded variables (3.8x)
pca_enhanced <- create_biplot(
  data = ind_plot_data,
  var_coord_scaled = var_coord_scaled,
  group_means = group_means,
  group_colors = group_colors,
  title = "Principal Component Analysis - Biplot",
  subtitle = "Showing genotypes and traits colored by STI groups",
  xlab = paste0("Dim1 (", round(get_eigenvalue(pca)[1,2], 1), "%)"),
  ylab = paste0("Dim2 (", round(get_eigenvalue(pca)[2,2], 1), "%)"),
  point_size = 3.5,
  var_label_size = 5.5,
  variety_size = 4
)

print(pca_enhanced)
save_publication_formats(pca_enhanced, "PCA_Biplot_Enhanced", width = 14, height = 10)

# INDIVIDUAL FACTOR MAP
ind_plot <- ggplot() +
  # Add individual points
  geom_point(data = ind_plot_data,
             aes(x = Dim.1, y = Dim.2, color = Group),
             size = 3, alpha = 0.8) +
  # Add variety labels in bold black with repel
  geom_text_repel(data = ind_plot_data,
                  aes(x = Dim.1, y = Dim.2, label = rownames(ind_plot_data)),
                  color = "black", size = 3.5, fontface = "bold",
                  box.padding = 0.3, point.padding = 0.2,
                  segment.color = 'grey50', segment.size = 0.2,
                  max.overlaps = Inf) +
  # Add group means as larger points
  geom_point(data = group_means, 
             aes(x = Dim.1, y = Dim.2, color = Group),
             size = 6, shape = 18, show.legend = FALSE) +
  # Add group mean labels
  geom_text_repel(data = group_means,
                  aes(x = Dim.1, y = Dim.2, label = Group, color = Group),
                  size = 4.5, fontface = "bold",
                  box.padding = 0.5, point.padding = 0.3,
                  nudge_y = 0.3, show.legend = FALSE) +
  # Scale colors
  scale_color_manual(values = group_colors, name = "STI Group") +
  # Theme and labels
  pca_theme() +
  labs(title = "PCA - Individual Factor Map",
       subtitle = "Colored by STI groups",
       x = paste0("Dim1 (", round(get_eigenvalue(pca)[1,2], 1), "%)"),
       y = paste0("Dim2 (", round(get_eigenvalue(pca)[2,2], 1), "%)")) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.box.background = element_rect(color = "black", linewidth = 0.5),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  coord_fixed(ratio = 1)

print(ind_plot)
save_publication_formats(ind_plot, "PCA_Individuals_Plot", width = 10, height = 8)

# VARIABLE FACTOR MAP - With expanded variables (3.8x) and solid lines
var_coord_scaled_var <- var_coord * 3.8

var_plot <- ggplot() +
  # Add variable vectors with solid lines and expanded coordinates
  geom_segment(data = var_coord_scaled_var,
               aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
               color = "gray30", size = 0.8, linetype = "solid") +
  # Add variable labels in pink with repel to avoid overlap
  geom_text_repel(data = var_coord_scaled_var,
                  aes(x = Dim.1, y = Dim.2, label = rownames(var_coord_scaled_var)),
                  color = "#FF69B4", size = 5.5, fontface = "bold",
                  box.padding = 0.5, point.padding = 0.3,
                  segment.color = '#FF69B4', segment.size = 0.2,
                  max.overlaps = Inf) +
  # Theme and labels
  pca_theme() +
  labs(title = "PCA - Variable Factor Map",
       subtitle = "Variables shown in pink (scaled 3.8x for clarity)",
       x = paste0("Dim1 (", round(get_eigenvalue(pca)[1,2], 1), "%)"),
       y = paste0("Dim2 (", round(get_eigenvalue(pca)[2,2], 1), "%)")) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  coord_fixed(ratio = 1)

print(var_plot)
save_publication_formats(var_plot, "PCA_Variables_Plot", width = 10, height = 8)

# PCA SUMMARY
cat("\n=== PCA SUMMARY ===\n")
print(summary(pca))

variance_explained <- get_eigenvalue(pca)
cat("\n=== VARIANCE EXPLAINED ===\n")
print(variance_explained)

cat("\nAll PCA plots created and saved in publication formats with unified theme!\n")
cat("\nColor scheme used:\n")
cat("  High STI GY: #2E8B57 (Green)\n")
cat("  Intermediate STI GY: #6A0DAD (Purple)\n")
cat("  Low STI GY: #B22222 (Brick Red)\n")
cat("\nText formatting:\n")
cat("  Variety names: Black (bold) - with repel to avoid overlap\n")
cat("  Variable names: Pink (#FF69B4, bold) - scaled 3.8x for clarity\n")
cat("  Variable lines: Solid lines for clean visualization\n")
cat("  Group means: Diamond shapes (◈) with group colors\n")
cat("  Labels: Using geom_text_repel to prevent overlapping\n")


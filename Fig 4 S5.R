# ============================================================
# FIGURE 4: PCA Analyses for All Genotypes with STI Grouping
# ============================================================
setwd()
getwd()


# Load required libraries
library(tidyverse)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(ggpubr)
library(cowplot)
library(viridis)
library(ggrepel)
library(gridExtra)

# ============================================================
# UNIFIED THEME FUNCTION
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
      
      plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm")
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

# ============================================================
# SAVE FUNCTION (Same as Figure 3) - with NULL check
# ============================================================

save_publication_formats <- function(plot_obj, base_name, width = 10, height = 8) {
  
  if(is.null(plot_obj)) {
    cat("Warning: plot_obj is NULL for", base_name, "- skipping save\n")
    return()
  }
  
  # Try to save
  tryCatch({
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
  }, error = function(e) {
    cat("Error saving", base_name, ":", e$message, "\n")
  })
}

# ============================================================
# FUNCTION TO VALIDATE AND FIX PLOT
# ============================================================

validate_plot <- function(p) {
  if(is.null(p)) return(NULL)
  
  # Try to render the plot to check if it's valid
  tryCatch({
    # Create a temporary file to test rendering
    temp_file <- tempfile(fileext = ".png")
    ggsave(temp_file, plot = p, width = 5, height = 4, dpi = 72)
    unlink(temp_file)
    return(p)
  }, error = function(e) {
    cat("  Plot validation failed:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================
# PART 1: LOAD DATA AND PREPARE
# ============================================================

data <- read.csv("Figure4_dataset for modeling codes run corrected.csv", header = TRUE)

# Remove rows with all NA values in trait columns
trait_cols <- c("Ab_AoV", "Ab_SF", "Ab_GCW", "Ab_APL", "Ab_GCL", "Ab_SD", 
                "Ad_BoV", "Ad_AoV", "Ad_SF", "Ad_GCW", "Ad_APL", "Ad_GCL", 
                "Ad_SD", "Ad_Ab_SD_ratio")

data_clean <- data[!apply(is.na(data[, trait_cols]), 1, all), ]

# Define three consistent colors (same as Figure 3)
group_colors <- c(
  "High STI GY" = "#2E8B57",      # Green
  "Intermediate STI GY" = "#6A0DAD", # Purple
  "Low STI GY" = "#B22222"        # Brick Red
)

# Define target varieties to highlight
highlight_varieties <- c("Superb", "Red Fife")

# ============================================================
# PART 2: PCA FOR EACH YEAR SEPARATELY - with validation
# ============================================================

perform_yearly_pca <- function(data, year_val, trait_cols, highlight_varieties) {
  
  cat("\nProcessing year:", year_val, "\n")
  
  # Subset data for specific year
  year_data <- data %>% filter(year == year_val)
  
  # Remove rows with all NA
  year_data <- year_data[!apply(is.na(year_data[, trait_cols]), 1, all), ]
  
  if(nrow(year_data) < 5) {
    cat("  Skipping year", year_val, "- insufficient data (n =", nrow(year_data), ")\n")
    return(NULL)
  }
  
  cat("  Data rows:", nrow(year_data), "\n")
  
  # Prepare PCA data
  pca_data <- year_data[, trait_cols]
  rownames(pca_data) <- year_data$variety
  
  # Perform PCA
  pca <- PCA(pca_data, ncp = 7, graph = FALSE, scale.unit = TRUE)
  
  # Extract coordinates
  ind_coord <- as.data.frame(pca$ind$coord)
  var_coord <- as.data.frame(pca$var$coord)
  
  # Scale variables for better visibility (3.8x as in Figure 3)
  var_coord_scaled <- var_coord * 3.8
  
  # Prepare individual plot data
  ind_plot_data <- cbind(ind_coord, 
                         Variety = rownames(ind_coord),
                         Group = year_data$group,
                         Highlight = ifelse(rownames(ind_coord) %in% highlight_varieties, 
                                            "Highlight", "Other"))
  
  # Get group means for centroids
  group_means <- aggregate(ind_coord[,1:2], 
                           by = list(Group = year_data$group), 
                           FUN = mean)
  
  # Create biplot
  p <- ggplot() +
    # Add individual points
    geom_point(data = ind_plot_data,
               aes(x = Dim.1, y = Dim.2, color = Group),
               size = 3, alpha = 0.7) +
    
    # Add variety labels in bold black with repel
    geom_text_repel(data = ind_plot_data,
                    aes(x = Dim.1, y = Dim.2, label = Variety),
                    color = "black", size = 3, fontface = "bold",
                    box.padding = 0.3, point.padding = 0.2,
                    segment.color = 'grey50', segment.size = 0.2,
                    max.overlaps = Inf) +
    
    # Highlight Superb and Red Fife with circles
    geom_point(data = ind_plot_data %>% filter(Highlight == "Highlight"),
               aes(x = Dim.1, y = Dim.2),
               color = "black", size = 6, shape = 1, stroke = 1.5) +
    
    # Add labels for highlighted varieties
    geom_text_repel(data = ind_plot_data %>% filter(Highlight == "Highlight"),
                    aes(x = Dim.1, y = Dim.2, label = Variety),
                    color = "black", size = 4.5, fontface = "bold",
                    box.padding = 0.5, point.padding = 0.3,
                    nudge_x = 0.3, nudge_y = 0.3,
                    segment.color = 'black', segment.size = 0.5) +
    
    # Add variable vectors with solid lines
    geom_segment(data = var_coord_scaled,
                 aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                 color = "gray30", size = 0.7, linetype = "solid") +
    
    # Add variable labels in pink
    geom_text_repel(data = var_coord_scaled,
                    aes(x = Dim.1, y = Dim.2, label = rownames(var_coord_scaled)),
                    color = "#FF69B4", size = 3.5, fontface = "bold",
                    box.padding = 0.5, point.padding = 0.3,
                    segment.color = '#FF69B4', segment.size = 0.2,
                    max.overlaps = Inf) +
    
    # Add group means as larger points (diamonds)
    geom_point(data = group_means, 
               aes(x = Dim.1, y = Dim.2, color = Group),
               size = 6, shape = 18, show.legend = FALSE) +
    
    # Add group mean labels
    geom_text_repel(data = group_means,
                    aes(x = Dim.1, y = Dim.2, label = Group, color = Group),
                    size = 4, fontface = "bold",
                    box.padding = 0.5, point.padding = 0.3,
                    nudge_y = 0.3, show.legend = FALSE) +
    
    # Scale colors
    scale_color_manual(values = group_colors, name = "STI Group") +
    
    # Theme and labels
    pca_theme() +
    labs(title = paste("PCA of Wheat Traits -", year_val),
         subtitle = paste("n =", nrow(ind_plot_data), "varieties"),
         x = paste0("Dim1 (", round(get_eigenvalue(pca)[1,2], 1), "%)"),
         y = paste0("Dim2 (", round(get_eigenvalue(pca)[2,2], 1), "%)")) +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      legend.box.background = element_rect(color = "black", linewidth = 0.5),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    ) +
    coord_fixed(ratio = 1)
  
  # Validate plot
  cat("  Validating plot...\n")
  p_valid <- validate_plot(p)
  
  if(is.null(p_valid)) {
    cat("  Warning: Plot for year", year_val, "failed validation\n")
    return(NULL)
  }
  
  cat("  Plot validated successfully\n")
  
  return(list(
    plot = p_valid,
    pca = pca,
    data = ind_plot_data,
    group_means = group_means
  ))
}

# ============================================================
# PART 3: GENERATE PCA PLOTS FOR EACH YEAR
# ============================================================

years <- unique(data_clean$year)
yearly_plots <- list()

for(yr in years) {
  result <- perform_yearly_pca(data_clean, yr, trait_cols, highlight_varieties)
  if(!is.null(result)) {
    yearly_plots[[as.character(yr)]] <- result
  }
}

# ============================================================
# PART 4: CREATE PANEL FIGURE WITH ALL YEARS - USING wrap_plots INSTEAD
# ============================================================

# Extract plots and arrange in panel - with NULL checks
plot_list <- list()
year_labels <- c()

for(yr in years) {
  yr_char <- as.character(yr)
  if(!is.null(yearly_plots[[yr_char]]) && !is.null(yearly_plots[[yr_char]]$plot)) {
    # Add a tag to each plot
    tag <- ifelse(yr_char == "2021", "A", 
                  ifelse(yr_char == "2022", "B", "C"))
    plot_list[[yr_char]] <- yearly_plots[[yr_char]]$plot + 
      labs(tag = tag) +
      theme(plot.tag = element_text(size = 16, face = "bold"))
    year_labels <- c(year_labels, yr_char)
  } else {
    cat("Warning: Plot for year", yr, "is NULL - skipping\n")
  }
}

# Create panel figure using wrap_plots from patchwork
if(length(plot_list) > 0) {
  cat("\nCreating panel figure with", length(plot_list), "plots using wrap_plots...\n")
  
  # Use wrap_plots which is more robust than plot_grid
  if(length(plot_list) == 3) {
    panel_plot <- wrap_plots(plot_list, ncol = 3, guides = 'collect')
  } else if(length(plot_list) == 2) {
    panel_plot <- wrap_plots(plot_list, ncol = 2, guides = 'collect')
  } else {
    panel_plot <- wrap_plots(plot_list, ncol = 1, guides = 'collect')
  }
  
  # Add overall title
  panel_plot_with_title <- panel_plot +
    plot_annotation(
      title = "PCA of Wheat Traits Across Years (Grouped by STI Classification)",
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    ) &
    theme(legend.position = 'bottom')
  
  # Print the panel
  print(panel_plot_with_title)
  
  # Save the panel
  save_publication_formats(panel_plot_with_title, "Figure4_PCA_Across_Years_Panel", 
                           width = 18, height = 7)
  
  # Also save individual year plots
  for(yr in names(plot_list)) {
    # Remove the tag for individual saves
    individual_plot <- plot_list[[yr]] + 
      labs(tag = NULL) +
      theme(plot.tag = element_blank())
    save_publication_formats(individual_plot, paste0("Figure4_PCA_Year_", yr), 
                             width = 12, height = 9)
  }
} else {
  cat("\nNo valid plots available to create panel figure.\n")
}

# ============================================================
# PART 5: PCA USING ALL YEARS COMBINED
# ============================================================

cat("\n=== GENERATING COMBINED PCA (All Years) ===\n")

# Prepare combined data
combined_data <- data_clean[, trait_cols]
rownames(combined_data) <- paste(data_clean$variety, data_clean$year, sep = "_")

# Remove rows with all NA
combined_data <- combined_data[!apply(is.na(combined_data), 1, all), ]

# Perform PCA
pca_combined <- PCA(combined_data, ncp = 7, graph = FALSE, scale.unit = TRUE)

# Extract coordinates
ind_coord_comb <- as.data.frame(pca_combined$ind$coord)
var_coord_comb <- as.data.frame(pca_combined$var$coord)
var_coord_scaled_comb <- var_coord_comb * 3.8

# Prepare data with group info
ind_plot_comb <- cbind(ind_coord_comb,
                       Variety = data_clean$variety[match(rownames(ind_coord_comb), 
                                                          paste(data_clean$variety, data_clean$year, sep = "_"))],
                       Year = data_clean$year[match(rownames(ind_coord_comb), 
                                                    paste(data_clean$variety, data_clean$year, sep = "_"))],
                       Group = data_clean$group[match(rownames(ind_coord_comb), 
                                                      paste(data_clean$variety, data_clean$year, sep = "_"))],
                       Highlight = ifelse(data_clean$variety[match(rownames(ind_coord_comb), 
                                                                   paste(data_clean$variety, data_clean$year, sep = "_"))] 
                                          %in% highlight_varieties, "Highlight", "Other"))

# Get group means
group_means_comb <- aggregate(ind_coord_comb[,1:2], 
                              by = list(Group = ind_plot_comb$Group), 
                              FUN = mean)

# Create combined biplot
combined_plot <- ggplot() +
  # Add individual points colored by group and shaped by year
  geom_point(data = ind_plot_comb,
             aes(x = Dim.1, y = Dim.2, color = Group, shape = as.factor(Year)),
             size = 2.5, alpha = 0.7) +
  
  # Add variety labels in bold black with repel
  geom_text_repel(data = ind_plot_comb,
                  aes(x = Dim.1, y = Dim.2, label = Variety),
                  color = "black", size = 2.5, fontface = "bold",
                  box.padding = 0.2, point.padding = 0.1,
                  segment.color = 'grey50', segment.size = 0.15,
                  max.overlaps = Inf) +
  
  # Highlight Superb and Red Fife with circles
  geom_point(data = ind_plot_comb %>% filter(Highlight == "Highlight"),
             aes(x = Dim.1, y = Dim.2),
             color = "black", size = 5, shape = 1, stroke = 1.5) +
  
  # Add labels for highlighted varieties
  geom_text_repel(data = ind_plot_comb %>% filter(Highlight == "Highlight"),
                  aes(x = Dim.1, y = Dim.2, label = Variety),
                  color = "black", size = 4, fontface = "bold",
                  box.padding = 0.5, point.padding = 0.3,
                  nudge_x = 0.3, nudge_y = 0.3,
                  segment.color = 'black', segment.size = 0.5) +
  
  # Add variable vectors
  geom_segment(data = var_coord_scaled_comb,
               aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               color = "gray30", size = 0.7, linetype = "solid") +
  
  # Add variable labels in pink
  geom_text_repel(data = var_coord_scaled_comb,
                  aes(x = Dim.1, y = Dim.2, label = rownames(var_coord_scaled_comb)),
                  color = "#FF69B4", size = 3.5, fontface = "bold",
                  box.padding = 0.5, point.padding = 0.3,
                  segment.color = '#FF69B4', segment.size = 0.2,
                  max.overlaps = Inf) +
  
  # Add group means
  geom_point(data = group_means_comb, 
             aes(x = Dim.1, y = Dim.2, color = Group),
             size = 7, shape = 18, show.legend = FALSE) +
  
  # Add group mean labels
  geom_text_repel(data = group_means_comb,
                  aes(x = Dim.1, y = Dim.2, label = Group, color = Group),
                  size = 4.5, fontface = "bold",
                  box.padding = 0.5, point.padding = 0.3,
                  nudge_y = 0.3, show.legend = FALSE) +
  
  # Scale colors and shapes
  scale_color_manual(values = group_colors, name = "STI Group") +
  scale_shape_manual(values = c(16, 17, 15), name = "Year") +
  
  # Theme and labels
  pca_theme() +
  labs(title = "PCA of Wheat Traits - All Years Combined",
       subtitle = "Colored by STI Group; Shaped by Year; Superb & Red Fife highlighted",
       x = paste0("Dim1 (", round(get_eigenvalue(pca_combined)[1,2], 1), "%)"),
       y = paste0("Dim2 (", round(get_eigenvalue(pca_combined)[2,2], 1), "%)")) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.box.background = element_rect(color = "black", linewidth = 0.5),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5)
  ) +
  coord_fixed(ratio = 1)

# Validate and save combined plot
combined_plot_valid <- validate_plot(combined_plot)
if(!is.null(combined_plot_valid)) {
  print(combined_plot_valid)
  save_publication_formats(combined_plot_valid, "Figure4_PCA_Combined_All_Years", width = 14, height = 10)
} else {
  cat("Warning: Combined plot failed validation\n")
}

# ============================================================
# PART 6: SUMMARY STATISTICS
# ============================================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("PCA ANALYSIS COMPLETE\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\nVariance explained (Combined PCA):\n")
print(get_eigenvalue(pca_combined)[1:2,])

cat("\nVarieties included by year:\n")
for(yr in years) {
  n_varieties <- nrow(data_clean %>% filter(year == yr))
  cat("  Year", yr, ":", n_varieties, "varieties\n")
}

cat("\nHighlighted varieties: Superb, Red Fife\n")
cat("\nColor scheme (same as Figure 3):\n")
cat("  High STI GY: #2E8B57 (Green)\n")
cat("  Intermediate STI GY: #6A0DAD (Purple)\n")
cat("  Low STI GY: #B22222 (Brick Red)\n")
cat("\nOutput files saved:\n")
cat("  Figure4_PCA_Across_Years_Panel (3-year panel)\n")
cat("  Figure4_PCA_Year_2021, 2022, 2023 (individual years)\n")
cat("  Figure4_PCA_Combined_All_Years (combined analysis)\n")

# ============================================================
# ADDITIONAL: PCA COMBINED YEARS - WITHOUT STI CLASSIFICATION
# Using wrap_plots for robustness
# ============================================================

cat("\n=== GENERATING PCA WITHOUT STI CLASSIFICATION ===\n")

perform_pca_no_sti <- function(data, trait_cols, highlight_varieties) {
  
  # Prepare data for PCA
  pca_data <- data[, trait_cols]
  rownames(pca_data) <- paste(data$variety, data$year, sep = "_")
  
  # Remove rows with all NA
  pca_data <- pca_data[!apply(is.na(pca_data), 1, all), ]
  
  # Perform PCA
  pca <- PCA(pca_data, ncp = 7, graph = FALSE, scale.unit = TRUE)
  
  # Extract coordinates
  ind_coord <- as.data.frame(pca$ind$coord)
  var_coord <- as.data.frame(pca$var$coord)
  
  # Scale variables for better visibility (3.8x as in Figure 3)
  var_coord_scaled <- var_coord * 3.8
  
  # Prepare individual plot data with variety and year info
  ind_plot_data <- cbind(ind_coord, 
                         Variety = data$variety[match(rownames(ind_coord), 
                                                      paste(data$variety, data$year, sep = "_"))],
                         Year = data$year[match(rownames(ind_coord), 
                                                paste(data$variety, data$year, sep = "_"))],
                         Highlight = ifelse(data$variety[match(rownames(ind_coord), 
                                                               paste(data$variety, data$year, sep = "_"))] 
                                            %in% highlight_varieties, "Highlight", "Other"))
  
  # Create biplot without STI grouping
  p <- ggplot() +
    # Add individual points colored by year (not STI)
    geom_point(data = ind_plot_data,
               aes(x = Dim.1, y = Dim.2, color = as.factor(Year)),
               size = 2.5, alpha = 0.7) +
    
    # Add variety labels in bold black with repel
    geom_text_repel(data = ind_plot_data,
                    aes(x = Dim.1, y = Dim.2, label = Variety),
                    color = "black", size = 2.5, fontface = "bold",
                    box.padding = 0.2, point.padding = 0.1,
                    segment.color = 'grey50', segment.size = 0.15,
                    max.overlaps = Inf) +
    
    # Highlight Superb and Red Fife with circles
    geom_point(data = ind_plot_data %>% filter(Highlight == "Highlight"),
               aes(x = Dim.1, y = Dim.2),
               color = "black", size = 5, shape = 1, stroke = 1.5) +
    
    # Add labels for highlighted varieties
    geom_text_repel(data = ind_plot_data %>% filter(Highlight == "Highlight"),
                    aes(x = Dim.1, y = Dim.2, label = Variety),
                    color = "black", size = 4.5, fontface = "bold",
                    box.padding = 0.5, point.padding = 0.3,
                    nudge_x = 0.3, nudge_y = 0.3,
                    segment.color = 'black', segment.size = 0.5) +
    
    # Add variable vectors with solid lines
    geom_segment(data = var_coord_scaled,
                 aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                 color = "gray30", size = 0.7, linetype = "solid") +
    
    # Add variable labels in pink
    geom_text_repel(data = var_coord_scaled,
                    aes(x = Dim.1, y = Dim.2, label = rownames(var_coord_scaled)),
                    color = "#FF69B4", size = 3.5, fontface = "bold",
                    box.padding = 0.5, point.padding = 0.3,
                    segment.color = '#FF69B4', segment.size = 0.2,
                    max.overlaps = Inf) +
    
    # Scale colors for years
    scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c"), 
                       name = "Year") +
    
    # Theme and labels
    pca_theme() +
    labs(title = "PCA of Wheat Traits - All Years Combined",
         subtitle = "Colored by Year; Superb & Red Fife highlighted with circles",
         x = paste0("Dim1 (", round(get_eigenvalue(pca)[1,2], 1), "%)"),
         y = paste0("Dim2 (", round(get_eigenvalue(pca)[2,2], 1), "%)")) +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      legend.box.background = element_rect(color = "black", linewidth = 0.5),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    ) +
    coord_fixed(ratio = 1)
  
  return(list(
    plot = p,
    pca = pca,
    data = ind_plot_data,
    var_coord = var_coord_scaled
  ))
}

# Run PCA without STI grouping
pca_no_sti <- perform_pca_no_sti(data_clean, trait_cols, highlight_varieties)

# Validate and save No-STI plot
pca_no_sti_valid <- validate_plot(pca_no_sti$plot)
if(!is.null(pca_no_sti_valid)) {
  print(pca_no_sti_valid)
  save_publication_formats(pca_no_sti_valid, "Figure4_PCA_Combined_No_STI_Grouping", 
                           width = 14, height = 10)
} else {
  cat("Warning: No-STI combined plot failed validation\n")
}

# ============================================================
# PART 8: INDIVIDUAL YEAR PLOTS - WITHOUT STI CLASSIFICATION
# ============================================================

perform_yearly_pca_no_sti <- function(data, year_val, trait_cols, highlight_varieties) {
  
  cat("\nProcessing year (No-STI):", year_val, "\n")
  
  # Subset data for specific year
  year_data <- data %>% filter(year == year_val)
  
  # Remove rows with all NA
  year_data <- year_data[!apply(is.na(year_data[, trait_cols]), 1, all), ]
  
  if(nrow(year_data) < 5) {
    cat("  Skipping year", year_val, "- insufficient data (n =", nrow(year_data), ")\n")
    return(NULL)
  }
  
  cat("  Data rows:", nrow(year_data), "\n")
  
  # Prepare PCA data
  pca_data <- year_data[, trait_cols]
  rownames(pca_data) <- year_data$variety
  
  # Perform PCA
  pca <- PCA(pca_data, ncp = 7, graph = FALSE, scale.unit = TRUE)
  
  # Extract coordinates
  ind_coord <- as.data.frame(pca$ind$coord)
  var_coord <- as.data.frame(pca$var$coord)
  
  # Scale variables for better visibility (3.8x as in Figure 3)
  var_coord_scaled <- var_coord * 3.8
  
  # Prepare individual plot data
  ind_plot_data <- cbind(ind_coord, 
                         Variety = rownames(ind_coord),
                         Highlight = ifelse(rownames(ind_coord) %in% highlight_varieties, 
                                            "Highlight", "Other"))
  
  # Create biplot without STI grouping
  p <- ggplot() +
    # Add individual points (all same color for simplicity)
    geom_point(data = ind_plot_data,
               aes(x = Dim.1, y = Dim.2),
               color = "#2c3e50", size = 3, alpha = 0.7) +
    
    # Add variety labels in bold black with repel
    geom_text_repel(data = ind_plot_data,
                    aes(x = Dim.1, y = Dim.2, label = Variety),
                    color = "black", size = 3, fontface = "bold",
                    box.padding = 0.3, point.padding = 0.2,
                    segment.color = 'grey50', segment.size = 0.2,
                    max.overlaps = Inf) +
    
    # Highlight Superb and Red Fife with circles
    geom_point(data = ind_plot_data %>% filter(Highlight == "Highlight"),
               aes(x = Dim.1, y = Dim.2),
               color = "black", size = 6, shape = 1, stroke = 1.5) +
    
    # Add labels for highlighted varieties
    geom_text_repel(data = ind_plot_data %>% filter(Highlight == "Highlight"),
                    aes(x = Dim.1, y = Dim.2, label = Variety),
                    color = "black", size = 4.5, fontface = "bold",
                    box.padding = 0.5, point.padding = 0.3,
                    nudge_x = 0.3, nudge_y = 0.3,
                    segment.color = 'black', segment.size = 0.5) +
    
    # Add variable vectors with solid lines
    geom_segment(data = var_coord_scaled,
                 aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                 color = "gray30", size = 0.7, linetype = "solid") +
    
    # Add variable labels in pink
    geom_text_repel(data = var_coord_scaled,
                    aes(x = Dim.1, y = Dim.2, label = rownames(var_coord_scaled)),
                    color = "#FF69B4", size = 3.5, fontface = "bold",
                    box.padding = 0.5, point.padding = 0.3,
                    segment.color = '#FF69B4', segment.size = 0.2,
                    max.overlaps = Inf) +
    
    # Theme and labels
    pca_theme() +
    labs(title = paste("PCA of Wheat Traits -", year_val),
         subtitle = paste("n =", nrow(ind_plot_data), "varieties"),
         x = paste0("Dim1 (", round(get_eigenvalue(pca)[1,2], 1), "%)"),
         y = paste0("Dim2 (", round(get_eigenvalue(pca)[2,2], 1), "%)")) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    ) +
    coord_fixed(ratio = 1)
  
  # Validate plot
  p_valid <- validate_plot(p)
  
  if(is.null(p_valid)) {
    cat("  Warning: No-STI plot for year", year_val, "failed validation\n")
    return(NULL)
  }
  
  cat("  No-STI plot validated successfully\n")
  
  return(list(
    plot = p_valid,
    pca = pca,
    data = ind_plot_data
  ))
}

# Generate individual year plots without STI
cat("\n=== GENERATING INDIVIDUAL YEAR PLOTS WITHOUT STI ===\n")
yearly_plots_no_sti <- list()

for(yr in years) {
  result <- perform_yearly_pca_no_sti(data_clean, yr, trait_cols, highlight_varieties)
  if(!is.null(result)) {
    yearly_plots_no_sti[[as.character(yr)]] <- result
    save_publication_formats(result$plot, paste0("Figure4_PCA_Year_", yr, "_No_STI"), 
                             width = 12, height = 9)
  }
}

# ============================================================
# PART 9: PANEL FIGURE WITHOUT STI CLASSIFICATION - Using wrap_plots
# ============================================================

# Extract plots and arrange in panel - with NULL checks
plot_list_no_sti <- list()

for(yr in years) {
  yr_char <- as.character(yr)
  if(!is.null(yearly_plots_no_sti[[yr_char]]) && !is.null(yearly_plots_no_sti[[yr_char]]$plot)) {
    tag <- ifelse(yr_char == "2021", "A", 
                  ifelse(yr_char == "2022", "B", "C"))
    plot_list_no_sti[[yr_char]] <- yearly_plots_no_sti[[yr_char]]$plot + 
      labs(tag = tag) +
      theme(plot.tag = element_text(size = 16, face = "bold"))
  } else {
    cat("Warning: No-STI plot for year", yr, "is NULL - skipping\n")
  }
}

# Create panel figure using wrap_plots
if(length(plot_list_no_sti) > 0) {
  cat("\nCreating No-STI panel figure with", length(plot_list_no_sti), "plots using wrap_plots...\n")
  
  if(length(plot_list_no_sti) == 3) {
    panel_plot_no_sti <- wrap_plots(plot_list_no_sti, ncol = 3, guides = 'collect')
  } else if(length(plot_list_no_sti) == 2) {
    panel_plot_no_sti <- wrap_plots(plot_list_no_sti, ncol = 2, guides = 'collect')
  } else {
    panel_plot_no_sti <- wrap_plots(plot_list_no_sti, ncol = 1, guides = 'collect')
  }
  
  # Add overall title
  panel_plot_no_sti_with_title <- panel_plot_no_sti +
    plot_annotation(
      title = "PCA of Wheat Traits Across Years (Without STI Classification)",
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    ) &
    theme(legend.position = 'none')
  
  # Print the panel
  print(panel_plot_no_sti_with_title)
  
  # Save the panel
  save_publication_formats(panel_plot_no_sti_with_title, "Figure4_PCA_Across_Years_Panel_No_STI", 
                           width = 18, height = 7)
} else {
  cat("\nNo valid No-STI plots available to create panel figure.\n")
}

# ============================================================
# PART 10: FINAL SUMMARY
# ============================================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("PCA WITHOUT STI CLASSIFICATION - COMPLETE\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\nOutput files saved:\n")
cat("  Figure4_PCA_Combined_No_STI_Grouping (all years combined, colored by year)\n")
cat("  Figure4_PCA_Year_2021_No_STI, 2022_No_STI, 2023_No_STI (individual years)\n")
cat("  Figure4_PCA_Across_Years_Panel_No_STI (3-year panel without STI)\n")

cat("\nFeatures:\n")
cat("  - Points colored by year (combined plot)\n")
cat("  - Single legend showing years\n")
cat("  - No STI classification shown\n")
cat("  - Superb and Red Fife highlighted with circles\n")
cat("  - Variables in pink (scaled 3.8x)\n")
cat("  - Variety names in bold black\n")
cat("  - Publication quality theme (same as Figure 3)\n")

cat("\nAll outputs saved with 600 DPI resolution!\n")


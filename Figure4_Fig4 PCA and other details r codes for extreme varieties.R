
setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd()

library(tidyverse)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(factoextra)
library(ggpubr)
library(rstatix)
library(dendextend)
library(GGally)
library(gridExtra)
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
      
      # Axis labels (bold, black)
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


pca_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
      axis.text.y = element_text(face = "bold", hjust = 1, size = 11),
      legend.position = "right",
      panel.grid.major = element_line(color = "gray80", size = 0.3)
    )
}


save_publication_formats <- function(plot_obj, base_name, width = 12, height = 8) {
  
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

data <- read.csv("Figure4_dataset for modeling codes run corrected.csv")

traits <- c("Ad_SD", "Ad_GCL", "Ad_APL", "Ad_GCW", "Ad_SF", 
            "Ad_AoV", "Ad_BoV", "Ab_SD", "Ab_GCL", "Ab_APL", 
            "Ab_GCW", "Ab_SF", "Ab_AoV", "HI", "GY", "TGW")

unique(data$variety)

unique(data$group)

print("Available years:")
print(unique(data$year))

target_varieties <- c("Red Fife", "Superb")
filtered_data <- data %>% filter(variety %in% target_varieties)

if(nrow(filtered_data) == 0) {
  stop("No data found for Red Fife and Superb varieties")
}

print(paste("Number of observations for Red Fife and Superb:", nrow(filtered_data)))
print("Year distribution:")
print(table(filtered_data$year, filtered_data$variety))

perform_advanced_pca <- function(data, traits) {
  complete_data <- data[, traits, drop = FALSE]
  complete_data <- complete_data[complete.cases(complete_data), ]
  
  if(nrow(complete_data) == 0) {
    stop("No complete cases found for PCA analysis")
  }
  
  complete_cases_idx <- complete.cases(data[, traits])
  variety_info <- data$variety[complete_cases_idx]
  year_info <- data$year[complete_cases_idx]

  pca_result <- prcomp(complete_data, scale. = TRUE)

  var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100

  scree_plot <- fviz_screeplot(pca_result, 
                               addlabels = TRUE,
                               main = "Scree Plot of Principal Components") +
    unified_theme() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(face = "bold", size = 11)
    )
  
  print(scree_plot)
  save_publication_formats(scree_plot, "pca_scree", width = 10, height = 8)
  
  biplot_data <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    Variety = variety_info,
    Year = year_info,
    Label = paste(variety_info, "-", year_info)
  )

  loadings <- as.data.frame(pca_result$rotation[, 1:2] * 7) 
  loadings$Trait <- rownames(loadings)
  
  calculate_repelled_positions <- function(loadings, max_iter = 100) {
    positions <- loadings[, c("PC1", "PC2")]
    n <- nrow(positions)
    
    for(iter in 1:max_iter) {
      moved <- FALSE
      for(i in 1:(n-1)) {
        for(j in (i+1):n) {
          dx <- positions$PC1[i] - positions$PC1[j]
          dy <- positions$PC2[i] - positions$PC2[j]
          distance <- sqrt(dx^2 + dy^2)
          
          if(distance < 0.8) {
            force <- (0.8 - distance) * 0.1
            angle <- atan2(dy, dx)
            
            positions$PC1[i] <- positions$PC1[i] + cos(angle) * force
            positions$PC2[i] <- positions$PC2[i] + sin(angle) * force
            positions$PC1[j] <- positions$PC1[j] - cos(angle) * force
            positions$PC2[j] <- positions$PC2[j] - sin(angle) * force
            
            moved <- TRUE
          }
        }
      }
      if(!moved) break
    }
    
    return(positions)
  }
  
  repelled_positions <- calculate_repelled_positions(loadings)
  loadings$Label_X <- repelled_positions$PC1
  loadings$Label_Y <- repelled_positions$PC2
  
  custom_biplot <- ggplot() +
    geom_point(data = biplot_data, 
               aes(x = PC1, y = PC2, color = Variety, shape = as.factor(Year)), 
               size = 3, alpha = 0.8) +
    
    ggrepel::geom_text_repel(
      data = biplot_data,
      aes(x = PC1, y = PC2, label = Label, color = Variety),
      size = 3.5,
      fontface = "bold",
      box.padding = 0.5,
      point.padding = 0.3,
      max.overlaps = 20,
      segment.color = "gray50",
      segment.size = 0.3,
      min.segment.length = 0.2
    ) +
    
    geom_segment(data = loadings, 
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.25, "cm"), type = "closed"), 
                 color = "darkred", alpha = 0.8, size = 1.0, lineend = "round") +
    
    geom_segment(data = loadings,
                 aes(x = PC1, y = PC2, xend = Label_X, yend = Label_Y),
                 color = "darkred", alpha = 0.5, size = 0.5, linetype = "dashed") +
    
    ggrepel::geom_text_repel(
      data = loadings,
      aes(x = Label_X, y = Label_Y, label = Trait),
      color = "darkred",
      size = 4.5,  # Slightly larger for better visibility
      fontface = "bold",
      box.padding = 0.8,
      point.padding = 0.5,
      max.overlaps = Inf,  # Allow unlimited overlaps handling
      segment.color = "darkred",
      segment.size = 0.3,
      min.segment.length = 0,
      force = 2,  # Increased repelling force
      force_pull = 0.5,
      direction = "both",
      nudge_x = 0.1,
      nudge_y = 0.1
    ) +
    scale_color_manual(values = c("Red Fife" = "#E41A1C", "Superb" = "#377EB8")) +
    scale_shape_manual(values = c(16, 17, 15, 18)) +
    
    labs(
      title = "PCA Biplot - Red Fife vs Superb",
      x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
      y = paste0("PC2 (", round(var_explained[2], 1), "%)"),
      color = "Variety",
      shape = "Year"
    ) +

    pca_theme() +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(face = "bold", size = 10),
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.grid.minor = element_line(color = "gray95", size = 0.2)
    ) +
    
    coord_fixed(ratio = 1) +
    expand_limits(x = c(min(biplot_data$PC1, loadings$Label_X) * 1.2, 
                        max(biplot_data$PC1, loadings$Label_X) * 1.2),
                  y = c(min(biplot_data$PC2, loadings$Label_Y) * 1.2, 
                        max(biplot_data$PC2, loadings$Label_Y) * 1.2))
  
  print(custom_biplot)
  save_publication_formats(custom_biplot, "pca_biplot_redfife_superb", width = 14, height = 10) 
  
  simple_biplot <- ggplot() +
    geom_point(data = biplot_data, 
               aes(x = PC1, y = PC2, color = Variety, shape = as.factor(Year)), 
               size = 3, alpha = 0.8) +
    
    ggrepel::geom_text_repel(
      data = biplot_data,
      aes(x = PC1, y = PC2, label = Label, color = Variety),
      size = 3.5, fontface = "bold",
      box.padding = 0.5, point.padding = 0.3
    ) +
    
    geom_segment(data = loadings, 
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm")), 
                 color = "darkred", alpha = 0.8, size = 0.8) +
    
    ggrepel::geom_text_repel(
      data = loadings,
      aes(x = PC1 * 1.15, y = PC2 * 1.15, label = Trait),
      color = "darkred", size = 4, fontface = "bold",
      box.padding = 0.8, point.padding = 0.5,
      max.overlaps = 50, force = 3
    ) +
    
    scale_color_manual(values = c("Red Fife" = "#E41A1C", "Superb" = "#377EB8")) +
    scale_shape_manual(values = c(16, 17, 15)) +
    labs(
      title = "PCA Biplot - Red Fife vs Superb (Simplified)",
      x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
      y = paste0("PC2 (", round(var_explained[2], 1), "%)"),
      color = "Variety",
      shape = "Year"
    ) +
    pca_theme()
  
  print(simple_biplot)
  save_publication_formats(simple_biplot, "pca_biplot_simplified", width = 14, height = 10)
  
  loadings_table <- data.frame(pca_result$rotation)
  write.csv(loadings_table, "pca_loadings.csv")
  
  var_table <- data.frame(
    Component = paste0("PC", 1:length(var_explained)),
    Variance_Explained = var_explained,
    Cumulative_Variance = cumsum(var_explained)
  )
  write.csv(var_table, "pca_variance.csv", row.names = FALSE)
  
  trait_loadings_plot <- ggplot(loadings, aes(x = PC1, y = PC2)) +
    geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm")), 
                 color = "blue", size = 1) +
    ggrepel::geom_text_repel(aes(label = Trait), 
                             size = 5, 
                             fontface = "bold",
                             box.padding = 1,
                             max.overlaps = Inf) +
    geom_point(color = "red", size = 2) +
    labs(
      title = "PCA Trait Loadings",
      x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
      y = paste0("PC2 (", round(var_explained[2], 1), "%)")
    ) +
    unified_theme() +
    theme(panel.grid.major = element_line(color = "gray90"))
  
  print(trait_loadings_plot)
  save_publication_formats(trait_loadings_plot, "pca_trait_loadings", width = 12, height = 10)
  
  # Print trait visibility summary
  cat("\n=== TRAIT VISIBILITY SUMMARY ===\n")
  cat("Total traits:", nrow(loadings), "\n")
  cat("Traits included:", paste(loadings$Trait, collapse = ", "), "\n")
  cat("Plot dimensions increased to accommodate all trait labels\n")
  cat("Multiple biplot versions created:\n")
  cat("- pca_biplot_redfife_superb.* (with leader lines)\n")
  cat("- pca_biplot_simplified.* (simplified repelling)\n")
  cat("- pca_trait_loadings.* (trait-only focus)\n")
  
  return(list(
    pca = pca_result,
    loadings = loadings_table,
    variance = var_table,
    scree_plot = scree_plot,
    biplot = custom_biplot,
    simple_biplot = simple_biplot,
    trait_loadings_plot = trait_loadings_plot,
    biplot_data = biplot_data
  ))
}

run_complete_analysis <- function(data, traits, var1, var2) {
  results <- list()
  
  tryCatch({
    results$correlation <- create_advanced_correlation(data, traits)
    print("Correlation analysis completed successfully")
  }, error = function(e) {
    print(paste("Error in correlation analysis:", e$message))
  })
  
  tryCatch({
    results$pca <- perform_advanced_pca(data, traits)
    print("PCA analysis completed successfully")
  }, error = function(e) {
    print(paste("Error in PCA analysis:", e$message))
  })
  
  tryCatch({
    results$extreme <- analyze_extreme_varieties(data, traits, var1, var2)
    print("Extreme varieties analysis completed successfully")
  }, error = function(e) {
    print(paste("Error in extreme varieties analysis:", e$message))
  })
  
  return(results)
}

cat("\n=== STARTING ENHANCED ANALYSIS FOR RED FIFE AND SUPERB ===\n")
results <- run_complete_analysis(filtered_data, traits, "Red Fife", "Superb")


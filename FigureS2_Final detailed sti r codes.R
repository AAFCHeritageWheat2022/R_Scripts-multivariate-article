

setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd()


library(ggplot2)
library(factoextra)
library(ggrepel)
library(corrplot)
library(gridExtra)
library(dplyr)
library(patchwork)
library(scales)
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


pca_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      axis.text.y = element_text(face = "bold", hjust = 1),
      legend.position = "right"
    )
}

distribution_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      strip.text = element_text(size = 10, face = "bold"),
      legend.position = "none"
    )
}

heatmap_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(face = "bold", hjust = 1, size = 8),
      legend.position = "right",
      panel.grid = element_blank()
    )
}

barplot_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      axis.text.y = element_text(size = 9),
      panel.grid.major.y = element_line(color = "gray80", size = 0.3),
      panel.grid.major.x = element_blank()
    )
}


save_publication_formats <- function(plot_obj, base_name, width_cm = 17.4, height_cm = 15) {
  
  width_in <- width_cm / 2.54
  height_in <- height_cm / 2.54
  
  tiff_filename <- paste0(base_name, ".tiff")
  ggsave(tiff_filename, plot = plot_obj, 
         width = width_in, height = height_in, units = "in",
         dpi = 600, compression = "lzw")
  cat("Saved:", tiff_filename, "\n")
  
  pdf_filename <- paste0(base_name, ".pdf")
  ggsave(pdf_filename, plot = plot_obj, 
         width = width_in, height = height_in, units = "in",
         device = cairo_pdf)
  cat("Saved:", pdf_filename, "\n")

  png_filename <- paste0(base_name, ".png")
  ggsave(png_filename, plot = plot_obj, 
         width = width_in, height = height_in, units = "in",
         dpi = 600)
  cat("Saved:", png_filename, "\n")

  jpeg_filename <- paste0(base_name, ".jpeg")
  ggsave(jpeg_filename, plot = plot_obj, 
         width = width_in, height = height_in, units = "in",
         dpi = 600, quality = 1.0)
  cat("Saved:", jpeg_filename, "\n")
}

df <- read.csv("FigureS2_GY.csv")

calculateIndices <- function() {
  RC <- function(Ys, Yp, YsBar, YpBar) ((Yp - Ys) / Yp) * 100
  TOL <- function(Ys, Yp, YsBar, YpBar) Yp - Ys
  MP <- function(Ys, Yp, YsBar, YpBar) (Yp + Ys) / 2
  GMP <- function(Ys, Yp, YsBar, YpBar) sqrt(Ys * Yp)
  HM <- function(Ys, Yp, YsBar, YpBar) 2 * (Ys * Yp) / (Ys + Yp)
  SSI <- function(Ys, Yp, YsBar, YpBar) (1 - Ys / Yp) / (1 - YsBar / YpBar)
  STI <- function(Ys, Yp, YsBar, YpBar) (Ys * Yp) / YpBar ^ 2
  YI <- function(Ys, Yp, YsBar, YpBar) Ys / YsBar
  YSI <- function(Ys, Yp, YsBar, YpBar) Ys / Yp
  RSI <- function(Ys, Yp, YsBar, YpBar) (Ys / Yp) / (YsBar / YpBar)

  getranks_df <- function(df_orig) {
    descendings <- c(2, 3, 6, 7, 8, 10, 11, 12, 13)
    for (col in descendings) {
      df_orig[col] = df_orig[col] * -1
    }
    
    df <- t(df_orig[, c(2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13)])
    ranks <- apply(df, 1, rank, ties.method = "min")
    
    SR <- data.frame(apply(ranks, 1, sum))
    colnames(SR) <- "SR"
    AR <- SR / length(ranks[1, ])
    colnames(AR) <- "AR"
    STD <- data.frame(apply(ranks, 1, sd))
    colnames(STD) <- "Std."
    
    cbind(df_orig[1], ranks, SR, AR, STD)
  }

  Calculate <- function(table_original) {
    table <- table_original[, -1]
    Yp <- table[1]
    Ys <- table[2]
    YpBar <- apply(Yp, 2, mean)
    YsBar <- apply(Ys, 2, mean)
    
    runFunc <- function(func) func(Ys, Yp, YsBar, YpBar)
    
    stats_df <- data.frame(
      table_original[, 1], 
      table_original[, 2], 
      table_original[, 3], 
      runFunc(RC), 
      runFunc(TOL), 
      runFunc(MP), 
      runFunc(GMP), 
      runFunc(HM), 
      runFunc(SSI), 
      runFunc(STI), 
      runFunc(YI), 
      runFunc(YSI), 
      runFunc(RSI)
    )
    
    colnames(stats_df) <- c("Species", "Yp", "Ys", "RC", "TOL", "MP", "GMP", "HM", "SSI", "STI", "YI", "YSI", "RSI")
    
    ranks_df <- getranks_df(stats_df)
    rownames(stats_df) <- stats_df[, 1]
    stats_df <- stats_df[, -1]
    rownames(ranks_df) <- ranks_df[, 1]
    ranks_df <- ranks_df[, -1]
    
    list(
      indices = stats_df, 
      ranks = ranks_df,
      correlations = list(
        pearson = cor(data.matrix(stats_df)),
        spearman = cor(data.matrix(ranks_df[, 1:(ncol(ranks_df)-3)]))
      )
    )
  }
  
  return(Calculate)
}

CalculateFunction <- calculateIndices()
Results <- CalculateFunction(df)

create_pca_biplot <- function(indices_df, title) {
  pca_result <- prcomp(indices_df, scale = TRUE)
  
  fviz_pca_biplot(pca_result, 
                  repel = TRUE,
                  col.var = "#2E9FDF",
                  col.ind = "#696969",
                  alpha.ind = 0.7,
                  labelsize = 3,
                  pointsize = 2,
                  title = title) +
    pca_theme() +
    theme(legend.position = "right")
}

create_indices_distribution <- function(indices_df) {
  indices_long <- indices_df %>%
    tibble::rownames_to_column("genotype") %>%
    tidyr::pivot_longer(cols = -genotype, names_to = "index", values_to = "value")
  
  ggplot(indices_long, aes(x = value, fill = index)) +
    geom_histogram(alpha = 0.7, bins = 15, color = "black", linewidth = 0.2) +
    facet_wrap(~ index, scales = "free", ncol = 3) +
    distribution_theme() +
    labs(title = "Distribution of Drought Tolerance Indices",
         x = "Value", y = "Frequency") +
    scale_fill_viridis_d()
}

create_ranks_heatmap <- function(ranks_df) {
  ranks_matrix <- as.matrix(ranks_df[, 1:(ncol(ranks_df)-3)])

  ranks_long <- as.data.frame(ranks_matrix) %>%
    tibble::rownames_to_column("genotype") %>%
    tidyr::pivot_longer(cols = -genotype, names_to = "index", values_to = "rank")
  
  ggplot(ranks_long, aes(x = index, y = genotype, fill = rank)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_viridis_c(name = "Rank", direction = -1) +
    heatmap_theme() +
    labs(title = "Heatmap of Drought Tolerance Index Ranks",
         x = "Indices", y = "Genotypes") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

create_performance_plot <- function(results) {
  mean_ranks <- data.frame(
    genotype = rownames(results$ranks),
    mean_rank = rowMeans(results$ranks[, 1:(ncol(results$ranks)-3)])
  ) %>%
    arrange(mean_rank)
  
  ggplot(mean_ranks, aes(x = reorder(genotype, -mean_rank), y = mean_rank)) +
    geom_bar(stat = "identity", fill = "#1f77b4", alpha = 0.7) +
    coord_flip() +
    barplot_theme() +
    labs(title = "Overall Drought Tolerance Performance",
         x = "Genotype", 
         y = "Mean Rank (lower = better)")
}

create_correlation_plot <- function(cor_matrix, title) {
  cor_data <- as.data.frame(as.table(cor_matrix))
  names(cor_data) <- c("Var1", "Var2", "value")
  
  ggplot(cor_data, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = round(value, 2)), color = "black", size = 3, fontface = "bold") +
    scale_fill_viridis_c(limits = c(-1, 1), name = "Correlation") +
    heatmap_theme() +
    labs(title = title, x = "", y = "") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

generate_consistent_plots <- function(results) {
  
  pca_biplot <- create_pca_biplot(results$indices, "PCA Biplot of Drought Tolerance Indices")
  print(pca_biplot)
  save_publication_formats(pca_biplot, "pca_biplot", width_cm = 17.4, height_cm = 15)
  
  dist_plot <- create_indices_distribution(results$indices)
  print(dist_plot)
  save_publication_formats(dist_plot, "indices_distribution", width_cm = 17.4, height_cm = 20)
  
  ranks_heatmap <- create_ranks_heatmap(results$ranks)
  print(ranks_heatmap)
  save_publication_formats(ranks_heatmap, "ranks_heatmap", width_cm = 17.4, height_cm = 20)

  performance_plot <- create_performance_plot(results)
  print(performance_plot)
  save_publication_formats(performance_plot, "performance_comparison", width_cm = 17.4, height_cm = 15)

  corr_pearson <- create_correlation_plot(results$correlations$pearson, "Pearson Correlation Matrix")
  print(corr_pearson)
  save_publication_formats(corr_pearson, "correlation_pearson", width_cm = 17.4, height_cm = 15)
  
  corr_spearman <- create_correlation_plot(results$correlations$spearman, "Spearman Correlation Matrix")
  print(corr_spearman)
  save_publication_formats(corr_spearman, "correlation_spearman", width_cm = 17.4, height_cm = 15)

  combined_plot <- (pca_biplot + dist_plot) / (ranks_heatmap + performance_plot) +
    plot_annotation(tag_levels = 'A') &
    theme(plot.tag = element_text(face = "bold", size = 14))
  
  print(combined_plot)
  save_publication_formats(combined_plot, "combined_analysis", width_cm = 20, height_cm = 25)
  
  list(
    pca_biplot = pca_biplot,
    dist_plot = dist_plot,
    ranks_heatmap = ranks_heatmap,
    performance_plot = performance_plot,
    correlation_pearson = corr_pearson,
    correlation_spearman = corr_spearman,
    combined_plot = combined_plot
  )
}

all_plots <- generate_consistent_plots(Results)

write.csv(Results$indices, "drought_tolerance_indices.csv", row.names = TRUE)
write.csv(Results$ranks, "drought_tolerance_ranks.csv", row.names = TRUE)
write.csv(Results$correlations$pearson, "pearson_correlations.csv", row.names = TRUE)
write.csv(Results$correlations$spearman, "spearman_correlations.csv", row.names = TRUE)


cat("=== DROUGHT TOLERANCE ANALYSIS SUMMARY ===\n\n")

cat("Basic Statistics of Yield Data:\n")
cat("Mean Yp (Potential Yield):", round(mean(df$Yp), 2), "\n")
cat("Mean Ys (Stressed Yield):", round(mean(df$Ys), 2), "\n")
cat("Yield Reduction (%):", round((1 - mean(df$Ys)/mean(df$Yp)) * 100, 2), "%\n\n")

cat("Top 5 Genotypes by Mean Rank:\n")
mean_ranks <- rowMeans(Results$ranks[, 1:(ncol(Results$ranks)-3)])
top_genotypes <- head(sort(mean_ranks), 5)
print(data.frame(Genotype = names(top_genotypes), Mean_Rank = round(top_genotypes, 2)))

cat("\nCorrelation between Yp and Ys:\n")
cor_test <- cor.test(df$Yp, df$Ys)
cat("Pearson r =", round(cor_test$estimate, 3), 
    ", p-value =", format.pval(cor_test$p.value), "\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All figures saved in multiple formats (TIFF, JPEG, PNG, PDF)\n")
cat("Data files saved: drought_tolerance_indices.csv, drought_tolerance_ranks.csv\n")
cat("Correlation matrices saved: pearson_correlations.csv, spearman_correlations.csv\n")


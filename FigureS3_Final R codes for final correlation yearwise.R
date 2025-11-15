
setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd()

library(corrplot)
library(tidyverse)
library(patchwork)
library(ggplot2)
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

cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  list(p = p.mat)
}


create_publication_correlation_plot <- function(df, year) {
  

  cor_matrix <- cor(df, use = "complete.obs")
  res <- cor.mtest(df, conf.level = 0.95)
  
  clean_names <- gsub(paste0("_", year), "", colnames(df))
  colnames(cor_matrix) <- clean_names
  rownames(cor_matrix) <- clean_names

  par(mar = c(3, 3, 4, 2), family = "sans")
  
  corrplot(cor_matrix, 
           method = 'color', 
           diag = FALSE, 
           type = "lower", 
           tl.col = "black",
           tl.cex = 0.8,  # Consistent font size
           tl.srt = 45,   # Angled labels for readability
           mar = c(1, 1, 2, 1),
           cl.cex = 0.7,  # Consistent color legend
           cl.ratio = 0.2,
           addgrid.col = "gray90",
           title = "",
           number.cex = 0.7,
           number.font = 2)
  
  n_vars <- ncol(cor_matrix)
  
  for(i in 2:n_vars) {
    for(j in 1:(i-1)) {
      cor_val <- cor_matrix[i, j]
      p_val <- res$p[i, j]
      
      x_pos <- j
      y_pos <- n_vars - i + 1
      
      text(x_pos, y_pos - 0.15, 
           labels = sprintf("%.2f", cor_val), 
           cex = 0.6, col = "black", font = 2)

      if (!is.na(p_val)) {
        sig_symbol <- ""
        if (p_val < 0.001) {
          sig_symbol <- "***"
        } else if (p_val < 0.01) {
          sig_symbol <- "**"
        } else if (p_val < 0.05) {
          sig_symbol <- "*"
        }
        
        if (sig_symbol != "") {
          rect(x_pos - 0.3, y_pos + 0.05, 
               x_pos + 0.3, y_pos + 0.25, 
               col = "white", border = NA)
          
          text(x_pos, y_pos + 0.15, 
               labels = sig_symbol, 
               cex = 0.7, col = "black", font = 2)
        }
      }
    }
  }
  
  title(main = paste("Year", year), 
        cex.main = 1.2, font.main = 2, line = 1)
}


save_correlation_publication <- function(plot_func, filename, year, width = 8, height = 7) {

  tiff(paste0(filename, "_", year, ".tiff"), 
       width = width, height = height, units = "in", res = 600, compression = "lzw")
  plot_func
  dev.off()
  
  pdf(paste0(filename, "_", year, ".pdf"), 
      width = width, height = height)
  plot_func
  dev.off()
  
  png(paste0(filename, "_", year, ".png"), 
      width = width, height = height, units = "in", res = 600)
  plot_func
  dev.off()
  

  jpeg(paste0(filename, "_", year, ".jpeg"), 
       width = width, height = height, units = "in", res = 600, quality = 1.0)
  plot_func
  dev.off()
}


analyze_correlations_publication <- function() {
  

  df_2021 <- read.csv("FigureS3_for correlation 2021 selected traits.csv", header = TRUE)
  df_2022 <- read.csv("FigureS3_for correlation 2022 selected traits.csv", header = TRUE) 
  df_2023 <- read.csv("FigureS3_for correlation 2023 selected traits.csv", header = TRUE)
  
  cat("=== DATA DIMENSIONS ===\n")
  cat("2021 data:", dim(df_2021), "\n")
  cat("2022 data:", dim(df_2022), "\n") 
  cat("2023 data:", dim(df_2023), "\n")
  
  cat("\nCreating publication-quality correlation plots...\n")
  

  save_correlation_publication({
    create_publication_correlation_plot(df_2021, "2021")
  }, "correlation_publication", "2021", width = 8, height = 7)

  save_correlation_publication({
    create_publication_correlation_plot(df_2022, "2022")
  }, "correlation_publication", "2022", width = 8, height = 7)

  save_correlation_publication({
    create_publication_correlation_plot(df_2023, "2023")
  }, "correlation_publication", "2023", width = 8, height = 7)
  
  cat("\nCreating combined side-by-side publication figure...\n")
  
  pdf("combined_correlation_publication.pdf", width = 18, height = 6)
  par(mfrow = c(1, 3), mar = c(3, 3, 4, 2), family = "sans")
  
  create_publication_correlation_plot(df_2021, "2021")
  mtext("A", side = 3, line = -1, adj = 0.02, font = 2, cex = 1.2)
  
  create_publication_correlation_plot(df_2022, "2022")
  mtext("B", side = 3, line = -1, adj = 0.02, font = 2, cex = 1.2)
  
  create_publication_correlation_plot(df_2023, "2023")
  mtext("C", side = 3, line = -1, adj = 0.02, font = 2, cex = 1.2)
  
  dev.off()

  tiff("combined_correlation_publication.tiff", 
       width = 18, height = 6, units = "in", res = 600, compression = "lzw")
  par(mfrow = c(1, 3), mar = c(3, 3, 4, 2), family = "sans")
  
  create_publication_correlation_plot(df_2021, "2021")
  mtext("A", side = 3, line = -1, adj = 0.02, font = 2, cex = 1.2)
  
  create_publication_correlation_plot(df_2022, "2022")
  mtext("B", side = 3, line = -1, adj = 0.02, font = 2, cex = 1.2)
  
  create_publication_correlation_plot(df_2023, "2023")
  mtext("C", side = 3, line = -1, adj = 0.02, font = 2, cex = 1.2)
  
  dev.off()
  
  png("combined_correlation_publication.png", 
      width = 18, height = 6, units = "in", res = 600)
  par(mfrow = c(1, 3), mar = c(3, 3, 4, 2), family = "sans")
  
  create_publication_correlation_plot(df_2021, "2021")
  mtext("A", side = 3, line = -1, adj = 0.02, font = 2, cex = 1.2)
  
  create_publication_correlation_plot(df_2022, "2022")
  mtext("B", side = 3, line = -1, adj = 0.02, font = 2, cex = 1.2)
  
  create_publication_correlation_plot(df_2023, "2023")
  mtext("C", side = 3, line = -1, adj = 0.02, font = 2, cex = 1.2)
  
  dev.off()
  
  jpeg("combined_correlation_publication.jpeg", 
       width = 18, height = 6, units = "in", res = 600, quality = 1.0)
  par(mfrow = c(1, 3), mar = c(3, 3, 4, 2), family = "sans")
  
  create_publication_correlation_plot(df_2021, "2021")
  mtext("A", side = 3, line = -1, adj = 0.02, font = 2, cex = 1.2)
  
  create_publication_correlation_plot(df_2022, "2022")
  mtext("B", side = 3, line = -1, adj = 0.02, font = 2, cex = 1.2)
  
  create_publication_correlation_plot(df_2023, "2023")
  mtext("C", side = 3, line = -1, adj = 0.02, font = 2, cex = 1.2)
  
  dev.off()

  cat("\n=== CORRELATION SUMMARY ===\n")
  
  summary_stats <- data.frame(
    Year = c("2021", "2022", "2023"),
    Variables = c(ncol(df_2021), ncol(df_2022), ncol(df_2023)),
    Observations = c(nrow(df_2021), nrow(df_2022), nrow(df_2023)),
    Mean_Correlation = c(
      mean(abs(cor(df_2021)[lower.tri(cor(df_2021))]), na.rm = TRUE),
      mean(abs(cor(df_2022)[lower.tri(cor(df_2022))]), na.rm = TRUE),
      mean(abs(cor(df_2023)[lower.tri(cor(df_2023))]), na.rm = TRUE)
    ),
    Significant_Correlations = c(
      sum(cor.mtest(df_2021)$p[lower.tri(cor.mtest(df_2021)$p)] < 0.05, na.rm = TRUE),
      sum(cor.mtest(df_2022)$p[lower.tri(cor.mtest(df_2022)$p)] < 0.05, na.rm = TRUE),
      sum(cor.mtest(df_2023)$p[lower.tri(cor.mtest(df_2023)$p)] < 0.05, na.rm = TRUE)
    )
  )
  
  print(summary_stats)

  write.csv(cor(df_2021), "correlation_matrix_2021.csv", row.names = TRUE)
  write.csv(cor(df_2022), "correlation_matrix_2022.csv", row.names = TRUE) 
  write.csv(cor(df_2023), "correlation_matrix_2023.csv", row.names = TRUE)

  list(
    data_2021 = df_2021,
    data_2022 = df_2022, 
    data_2023 = df_2023,
    correlations_2021 = cor(df_2021),
    correlations_2022 = cor(df_2022),
    correlations_2023 = cor(df_2023),
    summary = summary_stats
  )
}

results <- analyze_correlations_publication()


cat("\nDisplaying preview of 2021 correlation matrix...\n")
create_publication_correlation_plot(results$data_2021, "2021")

cat("\n=== PUBLICATION-QUALITY ANALYSIS COMPLETE ===\n")
cat("Files generated with unified theming:\n")
cat("- Individual correlation plots (2021, 2022, 2023)\n")
cat("- Combined side-by-side figure (A, B, C panels)\n")
cat("- All formats: TIFF, PDF, PNG, JPEG (600 DPI)\n")
cat("- Consistent font family: sans (Arial/Helvetica)\n")
cat("- Bold fonts for all text elements\n")
cat("- Professional color scheme and spacing\n")
cat("- CSV files with correlation matrices\n")
cat("- Publication-ready quality matching Figure 1 style\n")


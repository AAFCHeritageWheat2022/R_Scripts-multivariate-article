# ============================================================
# FIGURE 2: Broad Sense Heritability of Traits
# ============================================================
setwd()
getwd()


library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(extrafont)
library(viridis)

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
        margin = ggplot2::margin(b = 10)  # FIXED: Explicitly use ggplot2::margin()
      ),
      
      axis.title = element_text(
        size = base_size,
        face = "bold"
      ),
      axis.title.x = element_text(margin = ggplot2::margin(t = 8)),  # FIXED
      axis.title.y = element_text(margin = ggplot2::margin(r = 8)),  # FIXED
      
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
      
      plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm")  # FIXED
    )
}

heritability_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
      axis.text.y = element_text(face = "bold", hjust = 1, size = 10),
      panel.grid.major.y = element_line(color = "gray80", size = 0.3),
      panel.grid.major.x = element_line(color = "gray80", size = 0.3),
      legend.position = "top"
    )
}

# ============================================================
# SAVE FUNCTION
# ============================================================

save_publication_formats <- function(plot_obj, base_name, width = 12, height = 6) {
  
  if(is.null(plot_obj)) {
    cat("Warning: plot_obj is NULL for", base_name, "- skipping save\n")
    return()
  }
  
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
# LOAD AND PREPARE DATA
# ============================================================

data <- read.csv("Figure2_heritabilityselectedtraits.csv", header = TRUE)

# Pivot to long format for year-wise heritability
data_long <- data %>%
  pivot_longer(cols = c(her2021, her2022, her2023), 
               names_to = "Year", 
               values_to = "Heritability") %>%
  mutate(Year = case_when(
    Year == "her2021" ~ "2021",
    Year == "her2022" ~ "2022", 
    Year == "her2023" ~ "2023"
  ))

# ============================================================
# CREATE HERITABILITY PLOT
# ============================================================

p4 <- ggplot(data, aes(x = avg_heritab, y = reorder(traits, avg_heritab))) +
  # Bars for average heritability
  geom_col(aes(fill = avg_heritab), alpha = 0.6, width = 0.7) +
  
  # Points for year-wise heritability
  geom_point(data = data_long, 
             aes(x = Heritability, color = Year), 
             size = 3, 
             position = position_dodge(width = 0.5)) +
  
  # Vertical line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  
  # Fill color gradient
  scale_fill_viridis_c(
    option = "D", 
    name = "Avg Heritability",
    guide = guide_colorbar(
      barwidth = 10, 
      barheight = 0.8,
      title.position = "top",
      title.theme = element_text(face = "bold", size = 10)
    )
  ) +
  
  # Year colors
  scale_color_manual(
    values = c("2021" = "#D55E00", "2022" = "#0072B2", "2023" = "#009E73"),
    name = "Field Years",
    guide = guide_legend(
      title.position = "top",
      title.theme = element_text(face = "bold", size = 10)
    )
  ) +
  
  # X-axis scale
  scale_x_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # Labels
  labs(
    title = "Broad Sense Heritability of Traits",
    subtitle = "Bars show average heritability, points show year-wise heritability",
    x = "Heritability",
    y = "Traits"
  ) +
  
  # Theme
  heritability_theme()

# ============================================================
# PRINT AND SAVE
# ============================================================

print(p4)
save_publication_formats(p4, "Figure2_Heritability_Selected_Traits", width = 12, height = 8)

cat("\nHeritability plot created and saved in all publication formats!\n")

# ============================================================
# SUMMARY STATISTICS
# ============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("HERITABILITY SUMMARY STATISTICS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\nTrait heritability summary:\n")
print(data)

cat("\nYear-wise heritability summary:\n")
summary_stats <- data_long %>%
  group_by(Year) %>%
  summarise(
    mean = mean(Heritability, na.rm = TRUE),
    sd = sd(Heritability, na.rm = TRUE),
    min = min(Heritability, na.rm = TRUE),
    max = max(Heritability, na.rm = TRUE)
  )
print(summary_stats)

# ============================================================
# SAVE SUMMARY STATISTICS
# ============================================================

write.csv(summary_stats, "Figure2_Heritability_Summary_Stats.csv", row.names = FALSE)
cat("\nSummary statistics saved: Figure2_Heritability_Summary_Stats.csv\n")

# ============================================================
# FINAL SUMMARY
# ============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("FIGURE 2 - ANALYSIS COMPLETED SUCCESSFULLY\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("\nFILES CREATED:\n")
cat("  Figure2_Heritability_Selected_Traits.{tiff,pdf,png,jpeg}\n")
cat("  Figure2_Heritability_Summary_Stats.csv\n")

cat("\nCOLOR SCHEME:\n")
cat("  Bars: Viridis color scale (avg heritability)\n")
cat("  Points: 2021 - #D55E00 (Orange), 2022 - #0072B2 (Blue), 2023 - #009E73 (Green)\n")

cat("\nAll outputs saved with 600 DPI resolution!\n")


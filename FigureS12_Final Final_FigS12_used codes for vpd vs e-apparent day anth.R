setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd()

library(ggplot2)
library(ggExtra)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
library(dplyr)
library(patchwork)
library(grid)

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
      legend.key = element_rect(fill = "white"),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
}

scatter_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
      axis.text.y = element_text(face = "bold", hjust = 1, size = 11),
      legend.position = "right",
      panel.grid.major.y = element_line(color = "gray80", size = 0.3)
    )
}

save_publication_formats <- function(plot_obj, base_name, width = 12, height = 6) {
  
  tiff_filename <- paste0(base_name, ".tiff")
  ggsave(tiff_filename, plot = plot_obj, 
         width = width, height = height, units = "in",
         dpi = 600, compression = "lzw")
  cat("Saved:", tiff_filename, "\n")
  
  pdf_filename <- paste0(base_name, ".pdf")
  tryCatch({
    ggsave(pdf_filename, plot = plot_obj, 
           width = width, height = height, units = "in",
           device = cairo_pdf)
    cat("Saved:", pdf_filename, "\n")
  }, error = function(e) {
    ggsave(pdf_filename, plot = plot_obj, 
           width = width, height = height, units = "in")
    cat("Saved:", pdf_filename, " (using default PDF device)\n")
  })
  
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

data2021 <- read.csv("FigureS12_FigS12_dataset9_scatterplot_2021.csv", header = TRUE)
data2022 <- read.csv("FigureS12_FigS12_dataset9_scatterplot_2022.csv", header = TRUE)
data2023 <- read.csv("FigureS12_FigS12_data9 scatter plot_2023.csv", header = TRUE)

data2021$year <- "2021"
data2022$year <- "2022" 
data2023$year <- "2023"

combined_data <- bind_rows(data2021, data2022, data2023)

combined_data$time <- factor(combined_data$time, levels = c("7", "10", "13", "16", "19"))
combined_data$year <- factor(combined_data$year, levels = c("2021", "2022", "2023"))

time_colors <- c("7" = "orange",      # 7:00 AM
                 "10" = "brown",       # 10:00 AM  
                 "13" = "#0072B2",     # 1:00 PM
                 "16" = "purple",      # 4:00 PM
                 "19" = "darkgreen")   # 7:00 PM

time_labels <- c("7" = "7:00 AM", 
                 "10" = "10:00 AM", 
                 "13" = "1:00 PM", 
                 "16" = "4:00 PM", 
                 "19" = "7:00 PM")

create_year_plot <- function(data, year_label, panel_label) {
  
  year_data <- data %>% filter(year == year_label)
  
  p_main <- ggplot(year_data, aes(x = VPDleaf, y = E_apparent, color = time)) +
    geom_point(alpha = 0.8, size = 2.5, shape = 16) +
    scale_color_manual(values = time_colors, 
                       labels = time_labels,
                       name = "Time of Day") +
    scale_x_continuous(limits = c(0, 6), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.5, 6.0), expand = c(0, 0)) +
    
    annotate("text", x = -Inf, y = Inf, label = panel_label, 
             hjust = -0.5, vjust = 1.5, size = 5, fontface = "bold", color = "black")
  
  if (year_label == "2021") {
    p_main <- p_main + 
      labs(
        x = "VPD Flag leaf (kPa)",
        y = expression(paste("E"["apparent day"], " (mmol m"^{-2}*"·s"^{-1}*")")),
        title = paste("E_apparent vs VPD -", year_label)
      )
  } else {
    p_main <- p_main + 
      labs(
        x = "VPD Flag leaf (kPa)",
        y = "",
        title = paste("E_apparent vs VPD -", year_label)
      )
  }
  
  p_main <- p_main + scatter_theme()
  
  return(p_main)
}

plot_2021 <- create_year_plot(combined_data, "2021", "A")
plot_2022 <- create_year_plot(combined_data, "2022", "B") 
plot_2023 <- create_year_plot(combined_data, "2023", "C")

final_plot <- (plot_2021 | plot_2022 | plot_2023) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

print(final_plot)

save_publication_formats(final_plot, "E_apparent_VPD_scatterplots", width = 18, height = 6)


facet_plot <- ggplot(combined_data, aes(x = VPDleaf, y = E_apparent, color = time)) +
  geom_point(alpha = 0.8, size = 2) +
  facet_wrap(~year, nrow = 1) +
  scale_color_manual(values = time_colors, 
                     labels = time_labels,
                     name = "Time of Day") +
  scale_x_continuous(limits = c(0, 6), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.5, 6.0), expand = c(0, 0)) +
  labs(x = "VPD Flag leaf (kPa)",
       y = "E_apparent day (mmol m⁻²·s⁻¹)",  # Unicode superscripts
       title = "E_apparent vs VPD Across Years") +
  scatter_theme()

print(facet_plot)

save_publication_formats(facet_plot, "E_apparent_VPD_facet", width = 16, height = 5)

create_bold_facet_expression <- function() {
  p <- ggplot(combined_data, aes(x = VPDleaf, y = E_apparent, color = time)) +
    geom_point(alpha = 0.8, size = 2) +
    facet_wrap(~year, nrow = 1) +
    scale_color_manual(values = time_colors, 
                       labels = time_labels,
                       name = "Time of Day") +
    scale_x_continuous(limits = c(0, 6), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.5, 6.0), expand = c(0, 0)) +
    labs(x = "VPD Flag leaf (kPa)",
         y = "",  
         title = "E_apparent vs VPD Across Years") +
    scatter_theme()

  facet_grob <- ggplotGrob(p)
  
  ylab_index <- which(facet_grob$layout$name == "ylab-l")
  if (length(ylab_index) > 0) {
    facet_grob$grobs[[ylab_index]] <- nullGrob()
  }

  y_label_grob <- textGrob(
    expression(paste("E"["apparent day"], " (mmol m"^{-2}*"·s"^{-1}*")")),
    x = unit(0, "npc"), 
    y = unit(0.5, "npc"),
    just = c("left", "center"),
    rot = 90,
    gp = gpar(fontsize = 12, fontface = "bold", fontfamily = "sans")
  )
  
  facet_grob <- gtable_add_grob(
    facet_grob,
    y_label_grob,
    t = 7, 
    l = 2,
    name = "bold_ylab"
  )
  
  return(facet_grob)
}


facet_grob_expression <- create_bold_facet_expression()
grid::grid.newpage()
grid::grid.draw(facet_grob_expression)


tiff("E_apparent_VPD_facet_expression.tiff", width = 16, height = 5, units = "in", res = 600, compression = "lzw")
grid::grid.draw(facet_grob_expression)
dev.off()
cat("Saved: E_apparent_VPD_facet_expression.tiff\n")

pdf("E_apparent_VPD_facet_expression.pdf", width = 16, height = 5)
grid::grid.draw(facet_grob_expression)
dev.off()
cat("Saved: E_apparent_VPD_facet_expression.pdf\n")

png("E_apparent_VPD_facet_expression.png", width = 16, height = 5, units = "in", res = 600)
grid::grid.draw(facet_grob_expression)
dev.off()
cat("Saved: E_apparent_VPD_facet_expression.png\n")

jpeg("E_apparent_VPD_facet_expression.jpeg", width = 16, height = 5, units = "in", res = 600, quality = 1.0)
grid::grid.draw(facet_grob_expression)
dev.off()
cat("Saved: E_apparent_VPD_facet_expression.jpeg\n")

cat("Summary Statistics by Year:\n")
for(yr in c("2021", "2022", "2023")) {
  year_data <- combined_data %>% filter(year == yr)
  cat(paste("\nYear", yr, ":\n"))
  cat(paste("  Number of observations:", nrow(year_data), "\n"))
  cat(paste("  E_apparent range:", round(min(year_data$E_apparent, na.rm = TRUE), 3), 
            "to", round(max(year_data$E_apparent, na.rm = TRUE), 3), "\n"))
  cat(paste("  VPDleaf range:", round(min(year_data$VPDleaf, na.rm = TRUE), 3), 
            "to", round(max(year_data$VPDleaf, na.rm = TRUE), 3), "\n"))
  
  cor_test <- cor.test(year_data$VPDleaf, year_data$E_apparent, method = "pearson")
  cat(paste("  Correlation (r):", round(cor_test$estimate, 3), "\n"))
  cat(paste("  p-value:", format.pval(cor_test$p.value, digits = 3), "\n"))
}

cat("\nAll plots created and saved in publication formats!\n")


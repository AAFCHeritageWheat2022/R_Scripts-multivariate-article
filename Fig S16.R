setwd()
getwd()


library(ggplot2)
library(ggExtra)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
library(dplyr)
library(patchwork)
library(grid)
library(gtable)

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
      axis.title.y = element_text(face = "bold", size = 12),  # Explicit bold
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

# Load data
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

# SOLUTION 1: Direct approach with guaranteed bold y-axis
facet_plot_bold <- ggplot(combined_data, aes(x = VPDleaf, y = E_apparent, color = time)) +
  geom_point(alpha = 0.8, size = 2) +
  facet_wrap(~year, nrow = 1) +
  scale_color_manual(values = time_colors, 
                     labels = time_labels,
                     name = "Time of Day") +
  scale_x_continuous(limits = c(0, 6), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.5, 6.0), expand = c(0, 0)) +
  labs(x = "VPD Flag leaf (kPa)",
       y = expression(bold(paste("E_day_anth", " (mmol m"^{-2}*"·s"^{-1}*")"))),
       title = "E_apparent vs VPD Across Years") +
  theme_bw() +
  theme(
    text = element_text(family = "sans", color = "black"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 8)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 8)),
    axis.text = element_text(size = 11, face = "bold", color = "black"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    panel.border = element_rect(color = "black", size = 0.8, fill = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", size = 0.3),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "black"),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 11),
    legend.position = "right",
    legend.key = element_rect(fill = "white"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )

print(facet_plot_bold)
save_publication_formats(facet_plot_bold, "E_apparent_VPD_facet_BOLD_FIXED", width = 16, height = 5)

# SOLUTION 2: Using grob manipulation for absolute control
create_absolute_bold_facet <- function() {
  # Create basic plot
  p <- ggplot(combined_data, aes(x = VPDleaf, y = E_apparent, color = time)) +
    geom_point(alpha = 0.8, size = 2) +
    facet_wrap(~year, nrow = 1) +
    scale_color_manual(values = time_colors, 
                       labels = time_labels,
                       name = "Time of Day") +
    scale_x_continuous(limits = c(0, 6), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.5, 6.0), expand = c(0, 0)) +
    labs(x = "VPD Flag leaf (kPa)",
         y = "",  # Remove y-axis label initially
         title = "E_apparent vs VPD Across Years") +
    scatter_theme()
  
  # Convert to grob
  g <- ggplotGrob(p)
  
  # Remove existing y-label if present
  ylab_index <- which(g$layout$name == "ylab-l")
  if (length(ylab_index) > 0) {
    g$grobs[[ylab_index]] <- nullGrob()
  }
  
  # Create a completely custom bold y-label
  bold_y_label <- textGrob(
    label = expression(bold(paste("E_day_anth", " (mmol m"^{-2}*"·s"^{-1}*")"))),
    x = unit(0, "npc"),
    y = unit(0.5, "npc"), 
    just = c("left", "center"),
    rot = 90,
    gp = gpar(fontsize = 12, fontface = "bold", fontfamily = "sans")
  )
  
  # Add the custom bold label to the plot
  g <- gtable_add_grob(
    g,
    bold_y_label,
    t = 7,  # Top position - adjust if needed
    l = 2,  # Left position
    name = "custom_bold_ylab"
  )
  
  return(g)
}

# Create and display the absolute bold version
absolute_bold_plot <- create_absolute_bold_facet()
grid.draw(absolute_bold_plot)
save_publication_formats(absolute_bold_plot, "E_apparent_VPD_facet_ABSOLUTE_BOLD", width = 16, height = 5)

# SOLUTION 3: Simple theme override (easiest)
facet_plot_simple_bold <- ggplot(combined_data, aes(x = VPDleaf, y = E_apparent, color = time)) +
  geom_point(alpha = 0.8, size = 2) +
  facet_wrap(~year, nrow = 1) +
  scale_color_manual(values = time_colors, 
                     labels = time_labels,
                     name = "Time of Day") +
  scale_x_continuous(limits = c(0, 6), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.5, 6.0), expand = c(0, 0)) +
  labs(x = "VPD Flag leaf (kPa)",
       y = expression(paste("E_day_anth", " (mmol m"^{-2}*"·s"^{-1}*")")),
       title = "E_apparent vs VPD Across Years") +
  scatter_theme() +
  theme(axis.title.y = element_text(face = "bold", size = 12))  # Force bold

print(facet_plot_simple_bold)
save_publication_formats(facet_plot_simple_bold, "E_apparent_VPD_facet_SIMPLE_BOLD", width = 16, height = 5)

cat("All versions created. Check the output files:\n")
cat("- E_apparent_VPD_facet_BOLD_FIXED.*\n")
cat("- E_apparent_VPD_facet_ABSOLUTE_BOLD.*\n") 
cat("- E_apparent_VPD_facet_SIMPLE_BOLD.*\n")
cat("One of these should have the bold y-axis label you want!\n")


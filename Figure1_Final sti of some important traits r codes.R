
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


heatmap_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
      axis.text.y = element_text(face = "bold", hjust = 1, size = 9),
      legend.position = "right",
      panel.grid = element_blank()
    )
}

bubble_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(face = "bold", hjust = 1, size = 9),
      legend.position = "right",
      panel.grid.major = element_line(color = "grey90", linewidth = 0.2)
    )
}

barplot_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.major.y = element_line(color = "gray80", size = 0.3),
      panel.grid.major.x = element_blank()
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

setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd()

library(ggplot2)
library(ggpubr)
library(viridis)
library(dplyr)
library(tidyr)

data <- read.csv("Figure1_sti ranking.csv", header = TRUE, row.names = 1)

rownames(data) <- trimws(rownames(data))

y_axis_labels <- c("Red Fife", "Marquis", "Thatcher", "Canuck", "Katepwa", "Laura",
                   "AC Barrie", "AC Cadillac", "Stettler", "Carberry", "AAC Brandon",
                   "AAC Viewfield", "AAC Concord", "AAC Alida", "AAC Starbuck",
                   "Neepawa", "Columbus", "Pasqua", "AC Majestic", "Superb", "Harvest",
                   "Somerset", "Kane", "Unity VB", "Fieldstar", "Vesper", "AAC Prevail",
                   "AAC Cameron", "AAC Tradition", "AAC Magnet")


x_axis_labels <- c("GY", "TGW", "HI", "GPC", "GW")


create_publication_heatmap <- function() {
  data_matrix <- as.matrix(data)
  plot_data <- as.data.frame(data_matrix) %>%
    tibble::rownames_to_column("Variety") %>%
    pivot_longer(cols = -Variety, names_to = "Trait", values_to = "Value")
  plot_data$Variety <- factor(plot_data$Variety, levels = y_axis_labels)
  plot_data$Trait <- factor(plot_data$Trait, levels = colnames(data))
  ggplot(plot_data, aes(x = Trait, y = Variety, fill = Value)) +
    geom_tile(color = "white", linewidth = 0.8, height = 1, width = 0.9) +
    geom_text(aes(label = round(Value, 1)), size = 3.2, color = "black", fontface = "bold") +
    scale_fill_viridis_c(
      option = "D", 
      name = "STI Value",
      guide = guide_colorbar(
        barwidth = 0.8, 
        barheight = 12,
        title.position = "right",
        title.theme = element_text(angle = 90, face = "bold", size = 10)
      )
    ) +
    scale_x_discrete(
      labels = x_axis_labels, 
      position = "top",
      expand = c(0, 0)
    ) +
    scale_y_discrete(
      labels = y_axis_labels,
      expand = c(0, 0)
    ) +
    labs(
      title = "Stress Tolerance Index Ranking of Wheat Varieties",
      x = "Traits",
      y = "Wheat Varieties"
    ) +
    heatmap_theme()  
}


create_bubble_plot <- function() {
  data_matrix <- as.matrix(data)
  
  plot_data <- as.data.frame(data_matrix) %>%
    tibble::rownames_to_column("Variety") %>%
    pivot_longer(cols = -Variety, names_to = "Trait", values_to = "Value")

  plot_data$Variety <- factor(plot_data$Variety, levels = y_axis_labels)
  plot_data$Trait <- factor(plot_data$Trait, levels = colnames(data))
  
  ggplot(plot_data, aes(x = Trait, y = Variety)) +
    geom_point(aes(size = Value, fill = Value), shape = 21, color = "white", stroke = 0.8) +
    geom_text(aes(label = round(Value, 1)), size = 3.2, color = "black", fontface = "bold") +
    scale_size_area(
      name = "STI Value",
      max_size = 8,
      limits = c(min(plot_data$Value), max(plot_data$Value)),
      breaks = seq(0, 30, by = 5),
      guide = guide_legend(override.aes = list(fill = "black"))
    ) +
    scale_fill_viridis_c(
      option = "D", 
      name = "STI Value",
      guide = guide_colorbar(
        barwidth = 0.8, 
        barheight = 12,
        title.position = "right",
        title.theme = element_text(angle = 90, face = "bold", size = 10)
      )
    ) +
    scale_x_discrete(
      labels = x_axis_labels,
      expand = expansion(mult = 0.15)
    ) +
    scale_y_discrete(
      labels = y_axis_labels,
      expand = expansion(mult = 0.08)
    ) +
    labs(
      title = "Stress Tolerance Index Ranking of Wheat Varieties",
      x = "Traits",
      y = "Wheat Varieties"
    ) +
    bubble_theme() +  # Apply unified bubble plot theme
    coord_fixed(ratio = 0.8)
}

heatmap_plot <- create_publication_heatmap()
bubble_plot <- create_bubble_plot()

print(heatmap_plot)
print(bubble_plot)

save_publication_formats(heatmap_plot, "STI_Ranking_Heatmap", width = 10, height = 12)
save_publication_formats(bubble_plot, "STI_Ranking_Bubble", width = 10, height = 12)

cat("All plots created and saved in publication formats!\n")

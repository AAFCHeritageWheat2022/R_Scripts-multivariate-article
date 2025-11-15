setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd()

library(openxlsx)
library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)
library(scales)

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
      legend.position = "none", 

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

soil_data <- read.xlsx("FigureS1_3 year soil data.xlsx", sheet = "Sheet1")

soil_data_clean <- soil_data %>%
  setNames(c("year", "date", "soil_vwc", "soil_temp")) %>%
  mutate(
    date = as.Date(date),
    year = as.factor(year),
    doy = yday(date)
  ) %>%
  filter(month(date) %in% 6:8) %>%
  select(year, date, doy, soil_vwc, soil_temp)


cat("=== FINAL DATA STRUCTURE ===\n")
str(soil_data_clean)
summary(soil_data_clean)


vwc_color <- "#1f77b4"    
temp_color <- "#FF7F0E"  
y_axis_limit <- 0.5      
temp_axis_limit <- 25     

start_date <- as.Date("2022-06-15")
end_date <- as.Date("2022-08-31")
date_breaks <- seq(start_date, end_date, by = "10 days")

create_soil_plot <- function(data, year_val, show_x_label = TRUE) {
  
  year_data <- data %>% 
    filter(year == year_val) %>%
    mutate(plot_date = as.Date(paste("2022", month(date), day(date), sep = "-")))

  scaling_factor <- y_axis_limit / temp_axis_limit
  
  ggplot(year_data, aes(x = plot_date)) +

    geom_bar(aes(y = soil_vwc, fill = "Soil VWC"), 
             stat = "identity", width = 0.8, alpha = 0.7) +
    
    geom_line(aes(y = soil_temp * scaling_factor, color = "Soil Temperature"), 
              linewidth = 0.8) +
    geom_point(aes(y = soil_temp * scaling_factor, color = "Soil Temperature"), 
               size = 1.5, alpha = 0.8) +

    scale_y_continuous(
      name = expression("Soil VWC (m"^3*"/m"^3*")"),
      sec.axis = sec_axis(~./scaling_factor, 
                          name = "Soil Temperature (°C)",
                          breaks = seq(10, 25, by = 5)),
      limits = c(0, y_axis_limit),
      breaks = seq(0, 0.5, by = 0.1),
      expand = expansion(mult = c(0, 0.05))
    ) +

    scale_x_date(
      name = if(show_x_label) "Date" else "",
      breaks = date_breaks,
      labels = function(x) format(x, "%d-%b"),
      limits = c(start_date, end_date),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    
    scale_fill_manual(values = c("Soil VWC" = vwc_color)) +
    scale_color_manual(values = c("Soil Temperature" = temp_color)) +
    
    labs(title = paste("Year:", year_val)) +
    
    unified_theme() +  # Apply the unified theme
    theme(
      axis.title.y.left = element_text(color = vwc_color, face = "bold"),
      axis.text.y.left = element_text(color = vwc_color, face = "bold"),
      axis.title.y.right = element_text(color = temp_color, face = "bold"),
      axis.text.y.right = element_text(color = temp_color, face = "bold"),
      plot.title = element_text(face = "bold", size = 12)
    )
}

plot_2021 <- create_soil_plot(soil_data_clean, "2021", show_x_label = FALSE)
plot_2022 <- create_soil_plot(soil_data_clean, "2022", show_x_label = FALSE) 
plot_2023 <- create_soil_plot(soil_data_clean, "2023", show_x_label = TRUE)

create_legend <- function() {
  legend_plot <- ggplot() +
    geom_bar(aes(x = 1, y = 1, fill = "Soil VWC"), stat = "identity", width = 0.2) +
    geom_line(aes(x = c(0.8, 1.2), y = c(1, 1), color = "Soil Temperature"), linewidth = 1) +
    geom_point(aes(x = 1, y = 1, color = "Soil Temperature"), size = 2.5) +
    scale_fill_manual(name = NULL, values = c("Soil VWC" = vwc_color)) +
    scale_color_manual(name = NULL, values = c("Soil Temperature" = temp_color)) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 11, face = "bold", family = "sans"),
      legend.key = element_rect(fill = "white", color = NA),
      legend.key.size = unit(0.8, "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.spacing.x = unit(0.5, "cm")
    )
  
  cowplot::get_legend(legend_plot)
}

plot_legend <- create_legend()

combined_plot <- (plot_2021 / plot_2022 / plot_2023) +
  plot_layout(heights = c(1, 1, 1.1)) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(face = "bold", size = 12, family = "sans"))

final_plot <- wrap_plots(
  plot_legend,
  combined_plot,
  ncol = 1,
  heights = c(0.1, 3)
) +
  plot_annotation(
    theme = theme(
      plot.margin = margin(10, 10, 10, 10)
    )
  )

print(final_plot)

save_publication_formats(final_plot, "Figure_Soil_Dynamics", width = 6.85, height = 7.87) 


cat("\n=== STATISTICAL SUMMARY ===\n")
summary_stats <- soil_data_clean %>%
  group_by(year) %>%
  summarise(
    n_records = n(),
    mean_vwc = round(mean(soil_vwc, na.rm = TRUE), 3),
    sd_vwc = round(sd(soil_vwc, na.rm = TRUE), 3),
    mean_temp = round(mean(soil_temp, na.rm = TRUE), 1),
    sd_temp = round(sd(soil_temp, na.rm = TRUE), 1),
    vwc_range = paste(round(min(soil_vwc, na.rm = TRUE), 3), "-", 
                      round(max(soil_vwc, na.rm = TRUE), 3)),
    temp_range = paste(round(min(soil_temp, na.rm = TRUE), 1), "-", 
                       round(max(soil_temp, na.rm = TRUE), 1))
  )
print(summary_stats)

# Additional data validation
cat("\n=== DATA VALIDATION ===\n")
cat("Date ranges per year:\n")
soil_data_clean %>%
  group_by(year) %>%
  summarise(
    start = min(date),
    end = max(date),
    days = as.numeric(difftime(max(date), min(date), units = "days"))
  ) %>%
  print()

# Check for missing values
cat("\nMissing values:\n")
cat("Soil VWC:", sum(is.na(soil_data_clean$soil_vwc)), "\n")
cat("Soil Temperature:", sum(is.na(soil_data_clean$soil_temp)), "\n")

cat("\n=== COLOR CONSISTENCY CHECK ===\n")
color_check <- data.frame(
  Element = c("Soil VWC", "Soil Temperature"),
  Color_Hex = c("#1f77b4", "#FF7F0E"),
  Color_Name = c("Blue", "Orange")
)
print(color_check)

cat("All plots created and saved in publication formats!\n")


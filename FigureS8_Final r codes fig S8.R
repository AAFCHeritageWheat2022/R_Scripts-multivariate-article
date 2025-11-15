setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd() 

library(RVAideMemoire)
library(agricolae)
library(knitr)
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)

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

barplot_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
      panel.grid.major.y = element_line(color = "gray80", size = 0.3),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(size = 11, face = "bold"),
      strip.background = element_rect(fill = "lightgray", color = "black")
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

data <- read.csv("FigureS8_reformatted dataset fd abaxial.csv", header = TRUE)
names(data)
str(data)

data$specgroup <- factor(data$specgroup, levels = c("1", "2", "3", "4", "5"))
data$sample <- factor(data$sample, levels = c("Abaxial"))

summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE,
                      conf.interval = .95, .drop = TRUE) {
  library(plyr)
  
  length2 <- function(x, na.rm = FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }
  
  datac <- ddply(data, groupvars, .drop = .drop,
                 .fun = function(xx, col) {
                   c(N = length2(xx[[col]], na.rm = na.rm),
                     mean = mean(xx[[col]], na.rm = na.rm),
                     sd = sd(xx[[col]], na.rm = na.rm))
                 },
                 measurevar
  )
  
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)
  ciMult <- qt(conf.interval/2 + .5, datac$N - 1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

data_long <- data %>%
  pivot_longer(cols = c(X2021, X2022, X2023), 
               names_to = "year", 
               values_to = "file_distance") %>%
  mutate(year = gsub("X", "", year))

create_file_plot <- function(sample_data, sample_label) {

  TM <- summarySE(sample_data, measurevar = "file_distance", groupvars = c("year", "specgroup"), na.rm = TRUE)

  group_comparisons <- list()
  years <- c("2021", "2022", "2023")
  
  for (yr in years) {
    yr_data <- sample_data %>% filter(year == yr)
    model <- lm(file_distance ~ specgroup, data = yr_data)
    tukey <- HSD.test(model, "specgroup")
    group_comparisons[[yr]] <- tukey$groups
  }

  TM$group_letters <- NA
  for (yr in years) {
    yr_groups <- group_comparisons[[yr]]
    yr_groups <- yr_groups[order(rownames(yr_groups)), ]
    TM$group_letters[TM$year == yr] <- yr_groups$groups
  }

  model_year <- lm(file_distance ~ year, data = sample_data)
  tukey_year <- HSD.test(model_year, "year")

  year_significance <- tukey_year$groups

  max_y_value <- max(TM$file_distance + TM$sd, na.rm = TRUE)
  letter_offset <- max_y_value * 0.08  # Dynamic offset based on data range

  p <- ggplot(data = TM, aes(x = specgroup, y = file_distance, fill = specgroup)) +
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, color = "black", size = 0.3) +
    geom_errorbar(aes(ymin = file_distance - sd, ymax = file_distance + sd), 
                  width = 0.25, position = position_dodge(0.8), size = 0.6, color = "black") +
    facet_wrap(~year, nrow = 1) +

    geom_text(aes(label = group_letters, y = file_distance + sd + letter_offset), 
              position = position_dodge(0.8), size = 4.5, vjust = 0, fontface = "bold",
              color = "black") +

    geom_text(aes(label = sprintf("%.1f", file_distance), y = file_distance/2), 
              position = position_dodge(0.8), size = 3.5, color = "white", fontface = "bold") +
    
    geom_text(data = data.frame(year = years, 
                                letter = year_significance[years, "groups"],
                                x_pos = 3, 
                                y_pos = max_y_value + (letter_offset * 2.5)),
              aes(x = x_pos, y = y_pos, label = letter), 
              inherit.aes = FALSE, size = 5, fontface = "bold", color = "red") +
    
    labs(
      title = "Abaxial File Distance Analysis",
      x = "Groups",
      y = "File Distance (μm)"
    ) +
    scale_fill_manual(
      values = c("1" = "orange", 
                 "2" = "brown", 
                 "3" = "#0072B2", 
                 "4" = "purple", 
                 "5" = "darkgreen"),
      labels = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5")
    ) +
    scale_x_discrete(labels = c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + # More space for letters
    
    barplot_theme()
  
  return(list(plot = p, 
              group_comparisons = group_comparisons, 
              year_comparison = tukey_year,
              summary = TM))
}


file_data <- data_long


plot_file <- create_file_plot(file_data, "Abaxial File Distance")


print(plot_file$plot)


save_publication_formats(plot_file$plot, "abaxial_file_distance_analysis", width = 14, height = 8)


cat("ABAXIAL FILE DISTANCE ANALYSIS:\n")
cat("===============================\n")


year_means <- file_data %>%
  group_by(year) %>%
  summarise(mean_distance = mean(file_distance, na.rm = TRUE)) %>%
  arrange(desc(mean_distance))

cat("Year Means (descending order):\n")
print(year_means)


cat("\nBETWEEN YEARS COMPARISON (Red Letters in Figure):\n")
cat("-------------------------------------------------\n")
print(plot_file$year_comparison$groups)


cat("\nBETWEEN GROUPS Within Each Year (Small Letters in Figure):\n")
cat("----------------------------------------------------------\n")
for (yr in c("2021", "2022", "2023")) {
  cat(paste("\n", yr, ":\n"))
  print(plot_file$group_comparisons[[yr]])
}

cat("\nAll publication-quality figures saved in multiple formats!\n")


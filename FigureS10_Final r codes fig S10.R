
setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd()

library(RVAideMemoire)
library(agricolae)
library(knitr)
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
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
      axis.text.y = element_text(face = "bold"),
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
      axis.text.y = element_text(face = "bold", size = 11), # Ensure y-axis labels are bold
      panel.grid.major.y = element_line(color = "gray80", size = 0.3),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(size = 11, face = "bold")
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

data <- read.csv("FigureS10_reformatted dataset Flag leaf area length.csv", header = TRUE)
names(data)
str(data)

data$specgroup <- factor(data$specgroup, levels = c("1", "2", "3", "4", "5"))
data$sample <- factor(data$sample, levels = c("FLA", "FLL"))

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
               values_to = "value") %>%
  mutate(year = gsub("X", "", year))

create_publication_measurement_plot <- function(sample_data, sample_label, sample_letter, y_label) {

  TM <- summarySE(sample_data, measurevar = "value", groupvars = c("year", "specgroup"), na.rm = TRUE)
 
  group_comparisons <- list()
  years <- c("2021", "2022", "2023")
  
  for (yr in years) {
    yr_data <- sample_data %>% filter(year == yr)
    model <- lm(value ~ specgroup, data = yr_data)
    tukey <- HSD.test(model, "specgroup")
    group_comparisons[[yr]] <- tukey$groups
  }

  TM$group_letters <- NA
  for (yr in years) {
    yr_groups <- group_comparisons[[yr]]
    yr_groups <- yr_groups[order(rownames(yr_groups)), ]
    TM$group_letters[TM$year == yr] <- yr_groups$groups
  }

  model_year <- lm(value ~ year, data = sample_data)
  tukey_year <- HSD.test(model_year, "year")
  
  year_significance <- tukey_year$groups
  
  max_y_value <- max(TM$value + TM$sd, na.rm = TRUE)
  y_limit_buffer <- max_y_value * 0.15  # 15% buffer for significance letters
  
  p <- ggplot(data = TM, aes(x = specgroup, y = value, fill = specgroup)) +
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, color = "black", size = 0.3) +
    geom_errorbar(aes(ymin = value - sd, ymax = value + sd), 
                  width = 0.25, position = position_dodge(0.8), size = 0.6, color = "black") +
    facet_wrap(~year, nrow = 1) +
    
    geom_text(aes(label = group_letters, y = value + sd + (max_y_value * 0.05)), 
              position = position_dodge(0.8), size = 3.5, vjust = 0, fontface = "bold", color = "black") +

    geom_text(aes(label = sprintf("%.1f", value), y = value/2), 
              position = position_dodge(0.8), size = 3.2, color = "white", fontface = "bold") +
 
    geom_text(data = data.frame(year = years, 
                                letter = year_significance[years, "groups"],
                                x_pos = 3, 
                                y_pos = max_y_value + y_limit_buffer),
              aes(x = x_pos, y = y_pos, label = letter), 
              inherit.aes = FALSE, size = 4, fontface = "bold", color = "red") +
    
    labs(
      title = paste(sample_label, "(", sample_letter, ")"),
      x = "Groups",
      y = y_label,
      fill = "Groups"
    ) +
    scale_fill_manual(
      values = c("1" = "orange", 
                 "2" = "brown", 
                 "3" = "#0072B2", 
                 "4" = "purple", 
                 "5" = "darkgreen"),
      labels = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5")
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.2)),
      limits = c(0, max_y_value + y_limit_buffer * 1.2)
    ) +
    barplot_theme()  # Apply unified publication theme
  
  return(list(plot = p, 
              group_comparisons = group_comparisons, 
              year_comparison = tukey_year,
              summary = TM))
}

FLA_data <- data_long %>% filter(sample == "FLA")
FLL_data <- data_long %>% filter(sample == "FLL")

plot_FLA <- create_publication_measurement_plot(FLA_data, "Flag Leaf Area", "A", expression(paste(bold("Flag Leaf Area (cm"^2*")"))))
plot_FLL <- create_publication_measurement_plot(FLL_data, "Flag Leaf Length", "B", expression(bold("Flag Leaf Length (cm)")))

print(plot_FLA$plot)
print(plot_FLL$plot)

combined_plot <- plot_FLA$plot / plot_FLL$plot +
  plot_annotation(
    title = "Flag Leaf Area and Length Analysis: Between Groups and Between Years",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, family = "sans")
    )
  )

print(combined_plot)

save_publication_formats(combined_plot, "flag_leaf_complete_analysis_publication", width = 16, height = 12)

save_publication_formats(plot_FLA$plot, "flag_leaf_area_publication", width = 14, height = 6)
save_publication_formats(plot_FLL$plot, "flag_leaf_length_publication", width = 14, height = 6)


cat("MEASUREMENT TYPE COMPARISONS:\n")
cat("=============================\n")

measurement_means <- data_long %>%
  group_by(sample) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  arrange(desc(mean_value))

cat("Measurement Type means (descending order):\n")
print(measurement_means)

model_between_measurements <- lm(value ~ sample, data = data_long)
anova_between_measurements <- anova(model_between_measurements)
tukey_between_measurements <- HSD.test(model_between_measurements, "sample")

cat("\nANOVA Results:\n")
print(anova_between_measurements)
cat("\nTukey HSD Results:\n")
print(tukey_between_measurements$groups)

measurements <- c("FLA", "FLL")
plot_objs <- list(plot_FLA, plot_FLL)
labels <- c("Flag Leaf Area", "Flag Leaf Length")
letters <- c("A", "B")

for (i in 1:2) {
  measurement_type <- measurements[i]
  plot_obj <- plot_objs[[i]]
  measurement_label <- labels[i]
  measurement_letter <- letters[i]
  
  cat(paste("\n\n", measurement_letter, ": ", measurement_label, " - DETAILED ANALYSIS:\n"))
  cat(rep("=", 60), "\n")
  
  measurement_data <- get(paste0(measurement_type, "_data"))
  year_means <- measurement_data %>%
    group_by(year) %>%
    summarise(mean_value = mean(value, na.rm = TRUE)) %>%
    arrange(desc(mean_value))
  
  cat("\nYear Means (descending order):\n")
  print(year_means)
  
  cat("\nBETWEEN YEARS COMPARISON (Red Letters in Figure):\n")
  cat("-------------------------------------------------\n")
  print(plot_obj$year_comparison$groups)
  
  cat("\nBETWEEN GROUPS Within Each Year (Small Letters in Figure):\n")
  cat("----------------------------------------------------------\n")
  for (yr in c("2021", "2022", "2023")) {
    cat(paste("\n", yr, ":\n"))
    print(plot_obj$group_comparisons[[yr]])
  }
}

cat("\nAll publication-quality plots have been saved in TIFF, PDF, PNG, and JPEG formats!\n")
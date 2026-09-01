setwd()
getwd()

#============================================================
# FIGURE S10: Flag Leaf Area and Length Analysis
# Single legend on right side of top panel
# Group-wise comparisons at 1.08*Y_max; Year-wise at 1.28*Y_max
# ============================================================



# Fix margin conflict between plyr and ggplot2
if("package:plyr" %in% search()) {
  margin <- ggplot2::margin
}

# Load required libraries
library(RVAideMemoire)
library(agricolae)
library(knitr)
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(viridis)
library(cowplot)

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
        margin = ggplot2::margin(b = 10)
      ),
      
      axis.title = element_text(
        size = base_size,
        face = "bold"
      ),
      axis.title.x = element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y = element_text(margin = ggplot2::margin(r = 8)),
      
      axis.text = element_text(
        size = base_size - 1,
        face = "bold",
        color = "black"
      ),
      axis.text.x = element_blank(),  # Remove x-axis labels
      axis.ticks.x = element_blank(), # Remove x-axis ticks
      
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
      legend.position = "none",  # Remove individual legends by default
      legend.key = element_rect(fill = "white", color = NA),
      
      plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
}

barplot_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_blank(),  # Remove x-axis labels
      axis.ticks.x = element_blank(), # Remove x-axis ticks
      axis.title.x = element_blank(), # Remove x-axis title
      panel.grid.major.y = element_line(color = "gray80", size = 0.3),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(size = 12, face = "bold"),
      strip.background = element_rect(fill = "lightgray", color = "black")
    )
}

# ============================================================
# SAVE FUNCTION
# ============================================================

save_publication_formats <- function(plot_obj, base_name, width = 12, height = 6) {
  
  tiff_filename <- paste0(base_name, ".tiff")
  ggsave(tiff_filename, plot = plot_obj, 
         width = width, height = height, units = "in",
         dpi = 600, compression = "lzw")
  cat("Saved:", tiff_filename, "\n")
  
  pdf_filename <- paste0(base_name, ".pdf")
  ggsave(pdf_filename, plot = plot_obj, 
         width = width, height = height, units = "in")
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

# ============================================================
# SUMMARY SE FUNCTION
# ============================================================

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
  
  datac <- plyr::rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)
  ciMult <- qt(conf.interval/2 + .5, datac$N - 1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# ============================================================
# PART 1: LOAD AND PREPARE DATA
# ============================================================

data <- read.csv("FigureS10_reformatted dataset Flag leaf area length.csv", header = TRUE)

# Clean column names
names(data) <- gsub("X", "", names(data))

# Define consistent three-color scheme
group_colors <- c(
  "High STI GY" = "#2E8B57",        # Green
  "Intermediate STI GY" = "#6A0DAD", # Purple
  "Low STI GY" = "#B22222"          # Brick Red
)

# Convert specgroup to factor with proper levels
data$specgroup <- factor(data$specgroup, levels = c("High STI GY", "Intermediate STI GY", "Low STI GY"))
data$sample <- factor(data$sample, levels = c("FLA", "FLL"))

# ============================================================
# PART 2: RESHAPE DATA
# ============================================================

data_long <- data %>%
  dplyr::select(variety, rep, ecozone, release.year, specgroup, sample, `2021`, `2022`, `2023`) %>%
  tidyr::pivot_longer(cols = c(`2021`, `2022`, `2023`), 
                      names_to = "year", 
                      values_to = "value") %>%
  dplyr::mutate(year = gsub("X", "", year))

# Remove NA values
data_long <- data_long[!is.na(data_long$value), ]

# ============================================================
# PART 3: CREATE INDIVIDUAL MEASUREMENT PLOTS
# ============================================================

create_measurement_plot <- function(sample_data, sample_label, sample_letter, y_label, show_legend = FALSE) {
  
  # Calculate summary statistics by year and specgroup
  TM <- summarySE(sample_data, measurevar = "value", groupvars = c("year", "specgroup"), na.rm = TRUE)
  
  # Statistical analysis 1: Between groups within each year
  group_comparisons <- list()
  years <- c("2021", "2022", "2023")
  
  for (yr in years) {
    yr_data <- sample_data %>% dplyr::filter(year == yr)
    model <- lm(value ~ specgroup, data = yr_data)
    tukey <- HSD.test(model, "specgroup")
    group_comparisons[[yr]] <- tukey$groups
  }
  
  # Add between-group significance letters to TM
  TM$group_letters <- NA
  for (yr in years) {
    yr_groups <- group_comparisons[[yr]]
    yr_groups <- yr_groups[order(rownames(yr_groups)), ]
    TM$group_letters[TM$year == yr] <- yr_groups$groups
  }
  
  # Statistical analysis 2: Between years (overall)
  model_year <- lm(value ~ year, data = sample_data)
  tukey_year <- HSD.test(model_year, "year")
  
  # Get year significance
  year_significance <- tukey_year$groups
  
  # Calculate Y_max for positioning
  y_max <- max(TM$value + TM$sd, na.rm = TRUE)
  
  # Calculate dynamic positioning for significance letters
  TM <- TM %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(
      # Group-wise letters at 1.08*Y_max
      letter_y_pos = y_max * 1.08,
      # Value inside bars
      value_y_pos = value * 0.5
    ) %>%
    dplyr::ungroup()
  
  # Year-wise comparison letters at 1.28*Y_max
  y_pos_letters <- y_max * 1.28
  
  # Create year annotation data
  year_annotation <- data.frame(
    year = years,
    letter = toupper(year_significance[years, "groups"]),  # CAPITAL letters
    x_pos = 2,  # Middle of x-axis
    y_pos = y_pos_letters  # Position at 1.28 * Y_max
  )
  
  # Create the plot
  p <- ggplot(data = TM, aes(x = specgroup, y = value, fill = specgroup)) +
    
    # Add bars
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
    
    # Add error bars (SD)
    geom_errorbar(aes(ymin = value - sd, ymax = value + sd), 
                  width = 0.25, position = position_dodge(0.8), size = 0.8) +
    
    # Facet by year
    facet_wrap(~year, nrow = 1) +
    
    # Add group-wise significance letters (at 1.08*Y_max)
    geom_text(aes(label = group_letters, y = letter_y_pos), 
              position = position_dodge(0.8), 
              size = 5, vjust = 0, fontface = "bold", color = "black") +
    
    # Add values inside bars
    geom_text(aes(label = sprintf("%.1f", value), y = value_y_pos), 
              position = position_dodge(0.8), 
              size = 3.5, color = "white", fontface = "bold") +
    
    # Add year-wise comparison letters (at 1.28*Y_max)
    geom_text(data = year_annotation,
              aes(x = x_pos, y = y_pos, label = letter), 
              inherit.aes = FALSE, 
              size = 9, fontface = "bold", color = "red") +
    
    # Labels
    labs(
      title = paste(sample_label, "(", sample_letter, ")"),
      x = "STI Group",
      y = y_label,
      fill = "STI Group"
    ) +
    
    # Scale colors (consistent three-color scheme)
    scale_fill_manual(
      values = group_colors,
      labels = c("High STI GY", "Intermediate STI GY", "Low STI GY")
    ) +
    
    # Scale y-axis with extra space for letters at 1.28*Y_max
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.35)),
      limits = c(0, NA)
    ) +
    
    # Apply theme (x-axis labels removed)
    barplot_theme() +
    
    # Legend on right side only if show_legend is TRUE
    theme(
      legend.position = ifelse(show_legend, "right", "none"),
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      legend.box.background = element_rect(color = "black", linewidth = 0.5),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      strip.text = element_text(size = 12, face = "bold")
    )
  
  return(list(plot = p, 
              group_comparisons = group_comparisons, 
              year_comparison = tukey_year,
              summary = TM))
}

# ============================================================
# PART 4: GENERATE PLOTS FOR EACH MEASUREMENT
# ============================================================

# Create datasets for each measurement
FLA_data <- data_long %>% dplyr::filter(sample == "FLA")
FLL_data <- data_long %>% dplyr::filter(sample == "FLL")

# Generate individual measurement plots
# Only the top panel (FLA) shows the legend
plot_FLA <- create_measurement_plot(FLA_data, "Flag Leaf Area", "A", expression(bold("Flag Leaf Area (cm"^2*")")), show_legend = TRUE)
plot_FLL <- create_measurement_plot(FLL_data, "Flag Leaf Length", "B", expression(bold("Flag Leaf Length (cm)")), show_legend = FALSE)

# Display individual plots
print(plot_FLA$plot)
print(plot_FLL$plot)

# Save individual plots
save_publication_formats(plot_FLA$plot, "FigureS10_FLA", width = 14, height = 7)
save_publication_formats(plot_FLL$plot, "FigureS10_FLL", width = 14, height = 7)

# ============================================================
# PART 5: CREATE COMBINED PLOT WITH SINGLE LEGEND FROM TOP PANEL
# ============================================================

# Create combined plot - legend from top panel only
combined_plot <- plot_FLA$plot / plot_FLL$plot +
  plot_annotation(
    title = "Flag Leaf Area and Length Analysis",
    subtitle = "Capital letters (red) indicate between-year differences; small letters indicate within-year group differences",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 5)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40", margin = ggplot2::margin(b = 10))
    )
  )

# Print combined plot
print(combined_plot)

# Save combined plot
save_publication_formats(combined_plot, "FigureS10_Flag_Leaf_Combined", width = 16, height = 14)

# ============================================================
# PART 6: STATISTICAL ANALYSIS OUTPUTS
# ============================================================

cat("\n=== COMPLETE STATISTICAL ANALYSIS ===\n")

# Between measurement comparisons
cat("\nMEASUREMENT TYPE COMPARISONS (FLA vs FLL):\n")
cat("=========================================\n")

measurement_means <- data_long %>%
  group_by(sample) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  arrange(desc(mean_value))
print(measurement_means)

model_between_measurements <- lm(value ~ sample, data = data_long)
anova_between_measurements <- anova(model_between_measurements)
cat("\nANOVA Results:\n")
print(anova_between_measurements)

tukey_between_measurements <- HSD.test(model_between_measurements, "sample")
cat("\nTukey HSD Results:\n")
print(tukey_between_measurements$groups)

# Within measurement analysis
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
  
  # Year means
  year_means <- measurement_data %>%
    group_by(year) %>%
    summarise(mean_value = mean(value, na.rm = TRUE)) %>%
    arrange(desc(mean_value))
  
  cat("\nYear Means (descending order):\n")
  print(year_means)
  
  # Between years comparison
  cat("\nBETWEEN YEARS COMPARISON (Red Capital Letters at 1.28*Y_max):\n")
  print(plot_obj$year_comparison$groups)
  
  # Between groups within each year
  cat("\nBETWEEN GROUPS Within Each Year (Small Letters at 1.08*Y_max):\n")
  for (yr in c("2021", "2022", "2023")) {
    cat(paste("\n", yr, ":\n"))
    print(plot_obj$group_comparisons[[yr]])
  }
}

cat("\nAll publication-quality figures created successfully!\n")


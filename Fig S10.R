setwd()
getwd()


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

data <- read.csv("FigureS6_reformatted dataset_ndvi.csv", header = TRUE)

# Clean column names
names(data) <- gsub("X", "", names(data))

# Define consistent three-color scheme
group_colors <- c(
  "High STI GY" = "#2E8B57",        # Green
  "Intermediate STI GY" = "#6A0DAD", # Purple
  "Low STI GY" = "#B22222"          # Brick Red
)

# Factor formatting
data$specgroup <- factor(data$specgroup, levels = c("High STI GY", "Intermediate STI GY", "Low STI GY"))
data$sample <- factor(data$sample, levels = c("NDVI_2WBH", "NDVI_1WBH", "NDVI_WH"))

# ============================================================
# PART 2: RESHAPE DATA
# ============================================================

data_long <- data %>%
  dplyr::select(variety, rep, ecozone, release.year, specgroup, sample, `2021`, `2022`, `2023`) %>%
  tidyr::pivot_longer(cols = c(`2021`, `2022`, `2023`), 
                      names_to = "year", 
                      values_to = "ndvi") %>%
  dplyr::mutate(year = gsub("X", "", year))

# Remove NA values
data_long <- data_long[!is.na(data_long$ndvi), ]

# ============================================================
# PART 3: CREATE INDIVIDUAL SAMPLING TIME PLOTS
# ============================================================

create_sampling_plot <- function(sample_data, sample_label, sample_letter, show_legend = FALSE) {
  
  # Calculate summary statistics by year and specgroup
  TM <- summarySE(sample_data, measurevar = "ndvi", groupvars = c("year", "specgroup"), na.rm = TRUE)
  
  # Statistical analysis 1: Between groups within each year
  group_comparisons <- list()
  years <- c("2021", "2022", "2023")
  
  for (yr in years) {
    yr_data <- sample_data %>% dplyr::filter(year == yr)
    model <- lm(ndvi ~ specgroup, data = yr_data)
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
  
  # Statistical analysis 2: Between years (overall for this sampling time)
  model_year <- lm(ndvi ~ year, data = sample_data)
  tukey_year <- HSD.test(model_year, "year")
  
  # Get year significance
  year_significance <- tukey_year$groups
  
  # Calculate Y_max for positioning
  y_max <- max(TM$ndvi + TM$sd, na.rm = TRUE)
  
  # Calculate dynamic positioning for significance letters
  TM <- TM %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(
      # Group-wise letters at 1.08*Y_max
      letter_y_pos = y_max * 1.08,
      # NDVI values inside bars
      value_y_pos = ndvi * 0.5
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
  p <- ggplot(data = TM, aes(x = specgroup, y = ndvi, fill = specgroup)) +
    
    # Add bars
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
    
    # Add error bars (SD)
    geom_errorbar(aes(ymin = ndvi - sd, ymax = ndvi + sd), 
                  width = 0.25, position = position_dodge(0.8), size = 0.8) +
    
    # Facet by year
    facet_wrap(~year, nrow = 1) +
    
    # Add group-wise significance letters (at 1.08*Y_max)
    geom_text(aes(label = group_letters, y = letter_y_pos), 
              position = position_dodge(0.8), 
              size = 5, vjust = 0, fontface = "bold", color = "black") +
    
    # Add NDVI values inside bars
    geom_text(aes(label = sprintf("%.3f", ndvi), y = value_y_pos), 
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
      y = "NDVI Value",
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
# PART 4: GENERATE PLOTS FOR EACH SAMPLING TIME
# ============================================================

# Create datasets for each sampling time
sample_2WBH_data <- data_long %>% dplyr::filter(sample == "NDVI_2WBH")
sample_1WBH_data <- data_long %>% dplyr::filter(sample == "NDVI_1WBH")
sample_WH_data <- data_long %>% dplyr::filter(sample == "NDVI_WH")

# Generate individual sampling time plots
# Only the top panel (2WBH) shows the legend
plot_2WBH <- create_sampling_plot(sample_2WBH_data, "NDVI 2 Weeks Before Heading", "A", show_legend = TRUE)
plot_1WBH <- create_sampling_plot(sample_1WBH_data, "NDVI 1 Week Before Heading", "B", show_legend = FALSE)
plot_WH <- create_sampling_plot(sample_WH_data, "NDVI At Heading Week", "C", show_legend = FALSE)

# Display individual plots
print(plot_2WBH$plot)
print(plot_1WBH$plot)
print(plot_WH$plot)

# Save individual plots
save_publication_formats(plot_2WBH$plot, "FigureS6_NDVI_2WBH", width = 14, height = 7)
save_publication_formats(plot_1WBH$plot, "FigureS6_NDVI_1WBH", width = 14, height = 7)
save_publication_formats(plot_WH$plot, "FigureS6_NDVI_WH", width = 14, height = 7)

# ============================================================
# PART 5: CREATE COMBINED PLOT WITH SINGLE LEGEND FROM TOP PANEL
# ============================================================

# Create combined plot - legend from top panel only
combined_plot <- (plot_2WBH$plot / plot_1WBH$plot / plot_WH$plot) +
  plot_annotation(
    title = "NDVI Analysis: Across Sampling Times and STI Groups",
    subtitle = "Capital letters (red) indicate between-year differences; small letters indicate within-year group differences",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 5)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40", margin = ggplot2::margin(b = 10))
    )
  )

# Print combined plot
print(combined_plot)

# Save combined plot
save_publication_formats(combined_plot, "FigureS6_NDVI_Combined", width = 16, height = 18)

# ============================================================
# PART 6: STATISTICAL ANALYSIS OUTPUTS
# ============================================================

cat("\n=== COMPLETE STATISTICAL ANALYSIS ===\n")

# Between sampling time comparisons
cat("\nSAMPLING TIME COMPARISONS (Between Sampling Times):\n")
cat("==================================================\n")

sampling_means <- data_long %>%
  group_by(sample) %>%
  summarise(mean_ndvi = mean(ndvi, na.rm = TRUE)) %>%
  arrange(desc(mean_ndvi))
print(sampling_means)

model_between_samples <- lm(ndvi ~ sample, data = data_long)
anova_between_samples <- anova(model_between_samples)
cat("\nANOVA Results:\n")
print(anova_between_samples)

tukey_between_samples <- HSD.test(model_between_samples, "sample")
cat("\nTukey HSD Results:\n")
print(tukey_between_samples$groups)

# Within sampling time analysis
sampling_times <- c("NDVI_2WBH", "NDVI_1WBH", "NDVI_WH")
plot_objs <- list(plot_2WBH, plot_1WBH, plot_WH)
labels <- c("2 Weeks Before Heading", "1 Week Before Heading", "At Heading Week")
letters <- c("A", "B", "C")

for (i in 1:3) {
  sample_type <- sampling_times[i]
  plot_obj <- plot_objs[[i]]
  sample_label <- labels[i]
  sample_letter <- letters[i]
  
  cat(paste("\n\n", sample_letter, ": ", sample_label, " - DETAILED ANALYSIS:\n"))
  cat(rep("=", 60), "\n")
  
  sample_data <- get(paste0("sample_", c("2WBH", "1WBH", "WH")[i], "_data"))
  
  # Year means
  year_means <- sample_data %>%
    group_by(year) %>%
    summarise(mean_ndvi = mean(ndvi, na.rm = TRUE)) %>%
    arrange(desc(mean_ndvi))
  
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


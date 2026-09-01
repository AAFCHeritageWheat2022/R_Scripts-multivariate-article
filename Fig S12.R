setwd()
getwd()


# ============================================================
# FIGURE: Abaxial File Distance Analysis
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
      legend.position = "right",
      legend.key = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      legend.box.background = element_rect(color = "black", linewidth = 0.5),
      
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

data <- read.csv("FigureS8_reformatted dataset fd abaxial.csv", header = TRUE)

# Clean column names
names(data) <- gsub("X", "", names(data))

# Define consistent three-color scheme
group_colors <- c(
  "High STI GY" = "#2E8B57",        # Green
  "Intermediate STI GY" = "#6A0DAD", # Purple
  "Low STI GY" = "#B22222"          # Brick Red
)

# Convert specgroup to factor with proper levels (NOT numeric codes)
data$specgroup <- factor(data$specgroup, levels = c("High STI GY", "Intermediate STI GY", "Low STI GY"))
data$sample <- factor(data$sample, levels = c("Abaxial"))

# ============================================================
# PART 2: RESHAPE DATA
# ============================================================

data_long <- data %>%
  dplyr::select(variety, rep, ecozone, release.year, specgroup, sample, `2021`, `2022`, `2023`) %>%
  tidyr::pivot_longer(cols = c(`2021`, `2022`, `2023`), 
                      names_to = "year", 
                      values_to = "file_distance") %>%
  dplyr::mutate(year = gsub("X", "", year))

# Remove NA values
data_long <- data_long[!is.na(data_long$file_distance), ]

# ============================================================
# PART 3: CREATE FILE DISTANCE PLOT
# ============================================================

create_file_plot <- function(sample_data, sample_label, sample_letter) {
  
  # Calculate summary statistics by year and specgroup
  TM <- summarySE(sample_data, measurevar = "file_distance", groupvars = c("year", "specgroup"), na.rm = TRUE)
  
  # Statistical analysis 1: Between groups within each year
  group_comparisons <- list()
  years <- c("2021", "2022", "2023")
  
  for (yr in years) {
    yr_data <- sample_data %>% dplyr::filter(year == yr)
    model <- lm(file_distance ~ specgroup, data = yr_data)
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
  model_year <- lm(file_distance ~ year, data = sample_data)
  tukey_year <- HSD.test(model_year, "year")
  
  # Get year significance
  year_significance <- tukey_year$groups
  
  # Calculate Y_max for positioning
  y_max <- max(TM$file_distance + TM$sd, na.rm = TRUE)
  
  # Calculate dynamic positioning for significance letters
  TM <- TM %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(
      # Group-wise letters at 1.08*Y_max
      letter_y_pos = y_max * 1.08,
      # File distance values inside bars
      value_y_pos = file_distance * 0.5
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
  p <- ggplot(data = TM, aes(x = specgroup, y = file_distance, fill = specgroup)) +
    
    # Add bars
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
    
    # Add error bars (SD)
    geom_errorbar(aes(ymin = file_distance - sd, ymax = file_distance + sd), 
                  width = 0.25, position = position_dodge(0.8), size = 0.8) +
    
    # Facet by year
    facet_wrap(~year, nrow = 1) +
    
    # Add group-wise significance letters (at 1.08*Y_max)
    geom_text(aes(label = group_letters, y = letter_y_pos), 
              position = position_dodge(0.8), 
              size = 5, vjust = 0, fontface = "bold", color = "black") +
    
    # Add file distance values inside bars
    geom_text(aes(label = sprintf("%.1f", file_distance), y = value_y_pos), 
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
      y = "File Distance (µm)",
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
    
    # Legend on right side
    theme(
      legend.position = "right",
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
# PART 4: GENERATE PLOT
# ============================================================

# Generate file distance plot
plot_file <- create_file_plot(data_long, "Abaxial File Distance", "A")

# Display plot
print(plot_file$plot)

# Save plot
save_publication_formats(plot_file$plot, "FigureS8_Abaxial_File_Distance", width = 14, height = 7)

# ============================================================
# PART 5: STATISTICAL ANALYSIS OUTPUTS
# ============================================================

cat("\n=== ABAXIAL FILE DISTANCE ANALYSIS ===\n")
cat("====================================\n")

# Calculate overall means for each year
year_means <- data_long %>%
  group_by(year) %>%
  summarise(mean_distance = mean(file_distance, na.rm = TRUE)) %>%
  arrange(desc(mean_distance))

cat("Year Means (descending order):\n")
print(year_means)

# Between years analysis
cat("\nBETWEEN YEARS COMPARISON (Red Capital Letters at 1.28*Y_max):\n")
cat("------------------------------------------------------------\n")
print(plot_file$year_comparison$groups)

# Between groups within each year
cat("\nBETWEEN GROUPS Within Each Year (Small Letters at 1.08*Y_max):\n")
cat("-------------------------------------------------------------\n")
for (yr in c("2021", "2022", "2023")) {
  cat(paste("\n", yr, ":\n"))
  print(plot_file$group_comparisons[[yr]])
}

cat("\nAll publication-quality figures created successfully!\n")


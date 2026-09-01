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

data <- read.csv("FigureS7_reformatted dataset APL.csv", header = TRUE)

# Clean column names
names(data) <- gsub("X", "", names(data))

# Check the data structure
str(data)
print(head(data))
print(unique(data$specgroup))

# Define consistent three-color scheme (matching Figure S6)
group_colors <- c(
  "High STI GY" = "#2E8B57",        # Green
  "Intermediate STI GY" = "#6A0DAD", # Purple
  "Low STI GY" = "#B22222"          # Brick Red
)

# IMPORTANT: Check if specgroup is already in character format
# If it's numeric (1-5), map to groups
data$specgroup <- as.character(data$specgroup)

# Map numeric groups to STI categories
group_mapping <- c(
  "1" = "High STI GY",
  "2" = "High STI GY",
  "3" = "Intermediate STI GY",
  "4" = "Low STI GY",
  "5" = "Low STI GY"
)

# Apply mapping only if specgroup contains numeric values
if (all(data$specgroup %in% names(group_mapping))) {
  data$specgroup <- group_mapping[data$specgroup]
}

# Remove any NA or empty specgroup
data <- data[!is.na(data$specgroup) & data$specgroup != "", ]

# Factor formatting
data$specgroup <- factor(data$specgroup, levels = c("High STI GY", "Intermediate STI GY", "Low STI GY"))
data$sample <- factor(data$sample, levels = c("Adaxial", "Abaxial"))

# Check factor levels after formatting
print(unique(data$specgroup))
print(table(data$specgroup))

# ============================================================
# PART 2: RESHAPE DATA
# ============================================================

data_long <- data %>%
  dplyr::select(variety, rep, ecozone, release.year, specgroup, sample, `2021`, `2022`, `2023`) %>%
  tidyr::pivot_longer(cols = c(`2021`, `2022`, `2023`), 
                      names_to = "year", 
                      values_to = "pore_length") %>%
  dplyr::mutate(year = gsub("X", "", year))

# Remove NA values
data_long <- data_long[!is.na(data_long$pore_length), ]

# Check data after reshaping
print(head(data_long))
print(unique(data_long$specgroup))
print(unique(data_long$sample))

# ============================================================
# PART 3: CREATE INDIVIDUAL LEAF SURFACE PLOTS
# ============================================================

create_surface_plot <- function(surface_data, surface_label, surface_letter, show_legend = FALSE) {
  
  # Check if there is data
  if (nrow(surface_data) == 0) {
    cat("Warning: No data for", surface_label, "\n")
    return(NULL)
  }
  
  # Check if we have at least 2 groups for ANOVA
  unique_groups <- unique(surface_data$specgroup)
  if (length(unique_groups) < 2) {
    cat("Warning: Only", length(unique_groups), "group(s) found for", surface_label, 
        "- cannot perform group comparisons\n")
    # Create plot without statistical letters for groups
    return(create_basic_plot(surface_data, surface_label, surface_letter, show_legend))
  }
  
  # Calculate summary statistics by year and specgroup
  TM <- summarySE(surface_data, measurevar = "pore_length", groupvars = c("year", "specgroup"), na.rm = TRUE)
  
  # Statistical analysis 1: Between groups within each year
  group_comparisons <- list()
  years <- c("2021", "2022", "2023")
  
  for (yr in years) {
    yr_data <- surface_data %>% dplyr::filter(year == yr)
    if (nrow(yr_data) > 0) {
      # Check if we have at least 2 groups for this year
      yr_groups <- unique(yr_data$specgroup)
      if (length(yr_groups) >= 2) {
        model <- lm(pore_length ~ specgroup, data = yr_data)
        tukey <- HSD.test(model, "specgroup")
        group_comparisons[[yr]] <- tukey$groups
      } else {
        cat("Warning: Only", length(yr_groups), "group(s) for year", yr, "in", surface_label, "\n")
      }
    }
  }
  
  # Add between-group significance letters to TM
  TM$group_letters <- NA
  for (yr in years) {
    if (yr %in% names(group_comparisons)) {
      yr_groups <- group_comparisons[[yr]]
      yr_groups <- yr_groups[order(rownames(yr_groups)), ]
      # Match letters to TM
      for (i in 1:nrow(TM)) {
        if (TM$year[i] == yr) {
          group_name <- as.character(TM$specgroup[i])
          if (group_name %in% rownames(yr_groups)) {
            TM$group_letters[i] <- yr_groups[group_name, "groups"]
          }
        }
      }
    }
  }
  
  # Statistical analysis 2: Between years (overall for this leaf surface)
  # Check if we have enough years for ANOVA
  unique_years <- unique(surface_data$year)
  if (length(unique_years) >= 2) {
    model_year <- lm(pore_length ~ year, data = surface_data)
    tukey_year <- HSD.test(model_year, "year")
    year_significance <- tukey_year$groups
  } else {
    # If only one year, assign all as 'a'
    year_significance <- data.frame(
      groups = rep("a", length(unique_years)),
      stringsAsFactors = FALSE
    )
    rownames(year_significance) <- unique_years
  }
  
  # Calculate Y_max for positioning
  y_max <- max(TM$pore_length + TM$sd, na.rm = TRUE)
  if (is.na(y_max) || y_max == 0) y_max <- 1  # Fallback
  
  # Calculate dynamic positioning for significance letters
  TM <- TM %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(
      # Group-wise letters at 1.08*Y_max
      letter_y_pos = y_max * 1.08,
      # Pore length values inside bars
      value_y_pos = pore_length * 0.5
    ) %>%
    dplyr::ungroup()
  
  # Year-wise comparison letters at 1.28*Y_max
  y_pos_letters <- y_max * 1.28
  
  # Create year annotation data
  year_annotation <- data.frame(
    year = years,
    letter = NA,
    x_pos = 2,  # Middle of x-axis
    y_pos = y_pos_letters
  )
  
  # Fill in letters if available
  for (i in 1:length(years)) {
    yr <- years[i]
    if (yr %in% rownames(year_significance)) {
      year_annotation$letter[i] <- toupper(year_significance[yr, "groups"])
    } else {
      year_annotation$letter[i] <- ""
    }
  }
  
  # Create the plot
  p <- ggplot(data = TM, aes(x = specgroup, y = pore_length, fill = specgroup)) +
    
    # Add bars
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
    
    # Add error bars (SD)
    geom_errorbar(aes(ymin = pore_length - sd, ymax = pore_length + sd), 
                  width = 0.25, position = position_dodge(0.8), size = 0.8) +
    
    # Facet by year
    facet_wrap(~year, nrow = 1) +
    
    # Add pore length values inside bars
    geom_text(aes(label = sprintf("%.1f", pore_length), y = value_y_pos), 
              position = position_dodge(0.8), 
              size = 3.5, color = "white", fontface = "bold") +
    
    # Labels
    labs(
      title = paste(surface_label, "(", surface_letter, ")"),
      x = "STI Group",
      y = "Aperture Length (µm)",
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
  
  # Add group-wise significance letters if available
  if (any(!is.na(TM$group_letters))) {
    p <- p + geom_text(aes(label = group_letters, y = letter_y_pos), 
                       position = position_dodge(0.8), 
                       size = 5, vjust = 0, fontface = "bold", color = "black")
  }
  
  # Add year-wise comparison letters if available
  if (any(year_annotation$letter != "")) {
    p <- p + geom_text(data = year_annotation,
                       aes(x = x_pos, y = y_pos, label = letter), 
                       inherit.aes = FALSE, 
                       size = 9, fontface = "bold", color = "red")
  }
  
  return(list(plot = p, 
              group_comparisons = group_comparisons, 
              year_comparison = year_significance,
              summary = TM))
}

# Alternative: Basic plot without statistical letters
create_basic_plot <- function(surface_data, surface_label, surface_letter, show_legend = FALSE) {
  
  TM <- summarySE(surface_data, measurevar = "pore_length", groupvars = c("year", "specgroup"), na.rm = TRUE)
  y_max <- max(TM$pore_length + TM$sd, na.rm = TRUE)
  if (is.na(y_max) || y_max == 0) y_max <- 1
  
  TM <- TM %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(
      value_y_pos = pore_length * 0.5
    ) %>%
    dplyr::ungroup()
  
  p <- ggplot(data = TM, aes(x = specgroup, y = pore_length, fill = specgroup)) +
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
    geom_errorbar(aes(ymin = pore_length - sd, ymax = pore_length + sd), 
                  width = 0.25, position = position_dodge(0.8), size = 0.8) +
    facet_wrap(~year, nrow = 1) +
    geom_text(aes(label = sprintf("%.1f", pore_length), y = value_y_pos), 
              position = position_dodge(0.8), 
              size = 3.5, color = "white", fontface = "bold") +
    labs(
      title = paste(surface_label, "(", surface_letter, ")"),
      x = "STI Group",
      y = "Aperture Length (µm)",
      fill = "STI Group"
    ) +
    scale_fill_manual(
      values = group_colors,
      labels = c("High STI GY", "Intermediate STI GY", "Low STI GY")
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.15)),
      limits = c(0, NA)
    ) +
    barplot_theme() +
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
              group_comparisons = list(), 
              year_comparison = data.frame(),
              summary = TM))
}

# ============================================================
# PART 4: GENERATE PLOTS FOR EACH LEAF SURFACE
# ============================================================

# Create datasets for each leaf surface
adaxial_data <- data_long %>% dplyr::filter(sample == "Adaxial")
abaxial_data <- data_long %>% dplyr::filter(sample == "Abaxial")

cat("Adaxial data rows:", nrow(adaxial_data), "\n")
cat("Abaxial data rows:", nrow(abaxial_data), "\n")
cat("Adaxial specgroup levels:", paste(unique(adaxial_data$specgroup), collapse=", "), "\n")
cat("Abaxial specgroup levels:", paste(unique(abaxial_data$specgroup), collapse=", "), "\n")

# Generate individual leaf surface plots
# Only the top panel (Adaxial) shows the legend
plot_adaxial <- create_surface_plot(adaxial_data, "Adaxial Surface", "A", show_legend = TRUE)
plot_abaxial <- create_surface_plot(abaxial_data, "Abaxial Surface", "B", show_legend = FALSE)

# Check if plots were created successfully
if (!is.null(plot_adaxial)) {
  print(plot_adaxial$plot)
} else {
  cat("Error: Adaxial plot creation failed\n")
}

if (!is.null(plot_abaxial)) {
  print(plot_abaxial$plot)
} else {
  cat("Error: Abaxial plot creation failed\n")
}

# ============================================================
# PART 5: CREATE COMBINED PLOT
# ============================================================

# Check if both plots exist before combining
if (!is.null(plot_adaxial) && !is.null(plot_abaxial)) {
  
  # Save individual plots
  save_publication_formats(plot_adaxial$plot, "FigureS7_APL_Adaxial", width = 14, height = 7)
  save_publication_formats(plot_abaxial$plot, "FigureS7_APL_Abaxial", width = 14, height = 7)
  
  # Create combined plot - legend from top panel only
  combined_plot <- (plot_adaxial$plot / plot_abaxial$plot) +
    plot_annotation(
      title = "Stomatal Aperture Length Analysis: Across Leaf Surfaces and STI Groups",
      subtitle = "Capital letters (red) indicate between-year differences; small letters indicate within-year group differences",
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 5)),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40", margin = ggplot2::margin(b = 10))
      )
    )
  
  # Print combined plot
  print(combined_plot)
  
  # Save combined plot
  save_publication_formats(combined_plot, "FigureS7_APL_Combined", width = 16, height = 12)
  
} else {
  cat("Cannot create combined plot - one or both plots failed\n")
}

# ============================================================
# PART 6: STATISTICAL ANALYSIS OUTPUTS
# ============================================================

cat("\n=== COMPLETE STATISTICAL ANALYSIS ===\n")

# Between leaf surface comparisons
cat("\nLEAF SURFACE COMPARISONS (Between Surfaces):\n")
cat("============================================\n")

surface_means <- data_long %>%
  group_by(sample) %>%
  summarise(mean_pore_length = mean(pore_length, na.rm = TRUE)) %>%
  arrange(desc(mean_pore_length))
print(surface_means)

# Check if we have at least 2 surfaces for comparison
if (length(unique(data_long$sample)) >= 2) {
  model_between_surfaces <- lm(pore_length ~ sample, data = data_long)
  anova_between_surfaces <- anova(model_between_surfaces)
  cat("\nANOVA Results:\n")
  print(anova_between_surfaces)
  
  tukey_between_surfaces <- HSD.test(model_between_surfaces, "sample")
  cat("\nTukey HSD Results:\n")
  print(tukey_between_surfaces$groups)
}

# Within leaf surface analysis
surfaces <- c("Adaxial", "Abaxial")
plot_objs <- list(plot_adaxial, plot_abaxial)
labels <- c("Adaxial Surface", "Abaxial Surface")
letters <- c("A", "B")

for (i in 1:2) {
  surface_type <- surfaces[i]
  plot_obj <- plot_objs[[i]]
  surface_label <- labels[i]
  surface_letter <- letters[i]
  
  if (!is.null(plot_obj)) {
    cat(paste("\n\n", surface_letter, ": ", surface_label, " - DETAILED ANALYSIS:\n"))
    cat(rep("=", 60), "\n")
    
    surface_data <- get(paste0(tolower(surface_type), "_data"))
    
    # Year means
    year_means <- surface_data %>%
      group_by(year) %>%
      summarise(mean_pore_length = mean(pore_length, na.rm = TRUE)) %>%
      arrange(desc(mean_pore_length))
    
    cat("\nYear Means (descending order):\n")
    print(year_means)
    
    # Between groups within each year
    cat("\nBETWEEN GROUPS Within Each Year (Small Letters):\n")
    for (yr in c("2021", "2022", "2023")) {
      if (yr %in% names(plot_obj$group_comparisons)) {
        cat(paste("\n", yr, ":\n"))
        print(plot_obj$group_comparisons[[yr]])
      } else {
        cat(paste("\n", yr, ": No data or insufficient groups for comparison\n"))
      }
    }
  } else {
    cat(paste("\n\n", surface_letter, ": ", surface_label, " - No data available\n"))
  }
}

cat("\nAll publication-quality figures created successfully!\n")
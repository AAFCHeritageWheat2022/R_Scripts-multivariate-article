# Load required libraries
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
      axis.text.x = element_text(angle = 0, hjust = 0.5),
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

setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd() 

data <- read.csv("FigureS6_reformatted dataset_ndvi.csv", header = TRUE)
names(data)
str(data)

data$specgroup <- factor(data$specgroup, levels = c("1", "2", "3", "4", "5"))
data$sample <- factor(data$sample, levels = c("NDVI_2WBH", "NDVI_1WBH", "NDVI_WH"))

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
               values_to = "ndvi") %>%
  mutate(year = gsub("X", "", year))


create_sampling_plot <- function(sample_data, sample_label, sample_letter) {
  

  TM <- summarySE(sample_data, measurevar = "ndvi", groupvars = c("year", "specgroup"), na.rm = TRUE)

  group_comparisons <- list()
  years <- c("2021", "2022", "2023")
  
  for (yr in years) {
    yr_data <- sample_data %>% filter(year == yr)
    model <- lm(ndvi ~ specgroup, data = yr_data)
    tukey <- HSD.test(model, "specgroup")
    group_comparisons[[yr]] <- tukey$groups
  }
  
  TM$group_letters <- NA
  for (yr in years) {
    yr_groups <- group_comparisons[[yr]]
    yr_groups <- yr_groups[order(rownames(yr_groups)), ]
    TM$group_letters[TM$year == yr] <- yr_groups$groups
  }
  

  model_year <- lm(ndvi ~ year, data = sample_data)
  tukey_year <- HSD.test(model_year, "year")

  year_significance <- tukey_year$groups
  
  TM <- TM %>%
    group_by(year) %>%
    mutate(
      letter_y_pos = ndvi + sd + max(sd) * 0.15,
      value_y_pos = ndvi * 0.5
    ) %>%
    ungroup()
  

  year_annotation <- data.frame(
    year = years,
    letter = year_significance[years, "groups"],
    x_pos = 3,  # Middle of x-axis
    y_pos = max(TM$ndvi + TM$sd, na.rm = TRUE) * 1.12  
  )
  

  p <- ggplot(data = TM, aes(x = specgroup, y = ndvi, fill = specgroup)) +
    geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, color = "black", size = 0.3) +
    geom_errorbar(aes(ymin = ndvi - sd, ymax = ndvi + sd), 
                  width = 0.25, position = position_dodge(0.8), size = 0.6, color = "black") +
    facet_wrap(~year, nrow = 1) +
    
    geom_text(aes(label = group_letters, y = letter_y_pos), 
              position = position_dodge(0.8), size = 3.5, vjust = 0, fontface = "bold", color = "black") +
    
    geom_text(aes(label = sprintf("%.3f", ndvi), y = value_y_pos), 
              position = position_dodge(0.8), size = 3.2, color = "white", fontface = "bold") +
    
    geom_text(data = year_annotation,
              aes(x = x_pos, y = y_pos, label = letter), 
              inherit.aes = FALSE, size = 5, fontface = "bold", color = "red") +
    
    labs(
      title = paste(sample_label, "(", sample_letter, ")"),
      x = "Groups",
      y = "NDVI Value",
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
      expand = expansion(mult = c(0, 0.15)),  
      limits = c(0, NA)
    ) +
    barplot_theme()  
  
  return(list(plot = p, 
              group_comparisons = group_comparisons, 
              year_comparison = tukey_year,
              summary = TM))
}


sample_2WBH_data <- data_long %>% filter(sample == "NDVI_2WBH")
sample_1WBH_data <- data_long %>% filter(sample == "NDVI_1WBH")
sample_WH_data <- data_long %>% filter(sample == "NDVI_WH")


plot_2WBH <- create_sampling_plot(sample_2WBH_data, "NDVI 2 Weeks Before Heading", "A")
plot_1WBH <- create_sampling_plot(sample_1WBH_data, "NDVI 1 Week Before Heading", "B")
plot_WH <- create_sampling_plot(sample_WH_data, "NDVI at Heading Week", "C")


print(plot_2WBH$plot)
print(plot_1WBH$plot)
print(plot_WH$plot)


combined_plot <- plot_2WBH$plot / plot_1WBH$plot / plot_WH$plot +
  plot_annotation(
    title = "NDVI Analysis: Between Groups and Between Years Across Sampling Times",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, 
                                margin = margin(b = 15), family = "sans")
    )
  )

print(combined_plot)

cat("Saving publication-quality images in all formats...\n")

save_publication_formats(combined_plot, "ndvi_complete_analysis_publication", width = 16, height = 20)


save_publication_formats(plot_2WBH$plot, "ndvi_2WBH_publication", width = 14, height = 8)
save_publication_formats(plot_1WBH$plot, "ndvi_1WBH_publication", width = 14, height = 8)
save_publication_formats(plot_WH$plot, "ndvi_WH_publication", width = 14, height = 8)


cat("\nPUBLICATION-QUALITY FILES SAVED:\n")
cat("================================\n")
cat("1. ndvi_complete_analysis_publication.tiff - 600 DPI TIFF (Publication quality)\n")
cat("2. ndvi_complete_analysis_publication.pdf - Vector PDF (Scalable)\n")
cat("3. ndvi_complete_analysis_publication.png - 600 DPI PNG\n")
cat("4. ndvi_complete_analysis_publication.jpeg - 600 DPI JPEG\n")
cat("5. Individual plots for each sampling time in all formats\n")
cat("\nFile dimensions: 16x20 inches (combined), 14x8 inches (individual)\n")
cat("Resolution: 600 DPI for raster formats\n")
cat("Font family: sans (consistent with Figure 1)\n")

# Statistical analysis for between-sampling time comparisons
cat("\nCOMPLETE STATISTICAL ANALYSIS\n")
cat("=============================\n\n")


sampling_means <- data_long %>%
  group_by(sample) %>%
  summarise(mean_ndvi = mean(ndvi, na.rm = TRUE)) %>%
  arrange(desc(mean_ndvi))

cat("SAMPLING TIME COMPARISONS (Between Sampling Times):\n")
cat("==================================================\n")
cat("Sampling Time means (descending order):\n")
print(sampling_means)


model_between_samples <- lm(ndvi ~ sample, data = data_long)
anova_between_samples <- anova(model_between_samples)
tukey_between_samples <- HSD.test(model_between_samples, "sample")

cat("\nANOVA Results:\n")
print(anova_between_samples)
cat("\nTukey HSD Results (A = highest NDVI):\n")
print(tukey_between_samples$groups)


cat("\nVerification:\n")
cat("Sampling time with highest mean (", as.character(sampling_means$sample[1]), 
    ") has letter:", tukey_between_samples$groups[as.character(sampling_means$sample[1]), "groups"], "\n")


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
  year_means <- sample_data %>%
    group_by(year) %>%
    summarise(mean_ndvi = mean(ndvi, na.rm = TRUE)) %>%
    arrange(desc(mean_ndvi))
  
  cat("\nYear Means (descending order):\n")
  print(year_means)
  
  cat("\nBETWEEN YEARS COMPARISON (Red Letters in Figure):\n")
  cat("-------------------------------------------------\n")
  print(plot_obj$year_comparison$groups)
  
  highest_year <- as.character(year_means$year[1])
  cat("Verification - Highest year (", highest_year, ") has letter:", 
      plot_obj$year_comparison$groups[highest_year, "groups"], "\n")

  cat("\nBETWEEN GROUPS Within Each Year (Small Letters in Figure):\n")
  cat("----------------------------------------------------------\n")
  for (yr in c("2021", "2022", "2023")) {
    # Calculate group means for this year to verify a assignment
    yr_means <- sample_data %>%
      filter(year == yr) %>%
      group_by(specgroup) %>%
      summarise(mean_ndvi = mean(ndvi, na.rm = TRUE)) %>%
      arrange(desc(mean_ndvi))
    
    cat(paste("\n", yr, " - Group Means (descending order):\n"))
    print(yr_means)
    cat("Tukey HSD Results:\n")
    print(plot_obj$group_comparisons[[yr]])

    highest_group <- as.character(yr_means$specgroup[1])
    cat("Verification - Highest group (", highest_group, ") has letter:", 
        plot_obj$group_comparisons[[yr]][highest_group, "groups"], "\n")
  }
}

cat("\nAll publication-quality figures created successfully!\n")

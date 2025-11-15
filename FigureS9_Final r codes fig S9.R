setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd()

library(ggplot2)
library(agricolae)
library(dplyr)
library(patchwork)
library(tidyr)
library(ggpubr)

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


data <- read.csv("FigureS9_reformatted dataset_EPdL_CPdL.csv", header = TRUE)

data$specgroup <- factor(data$specgroup, levels = c("1", "2", "3", "4", "5"))
data$sample <- factor(rep(c("2021", "2022", "2023"), each = nrow(data)/3))

summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE,
                      conf.interval = .95, .drop = TRUE) {
  
  length2 <- function(x, na.rm = FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }
  
  datac <- plyr::ddply(data, groupvars, .drop = .drop,
                       .fun = function(xx, col) {
                         c(N = length2(xx[[col]], na.rm = na.rm),
                           mean = mean(xx[[col]], na.rm = na.rm),
                           sd = sd(xx[[col]], na.rm = na.rm))
                       },
                       measurevar
  )
  
  datac <- plyr::rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

data_long <- data %>%
  dplyr::select(variety, rep, ecozone, release.year, specgroup, sample, X2021, X2022, X2023) %>%
  tidyr::pivot_longer(cols = c(X2021, X2022, X2023), 
                      names_to = "year", 
                      values_to = "ratio") %>%
  mutate(year = gsub("X", "", year))

data_long <- data_long %>% filter(!is.na(ratio))

TM <- summarySE(data_long, measurevar = "ratio", groupvars = c("year", "specgroup"), na.rm = TRUE)

years <- c("2021", "2022", "2023")
tukey_results <- list()

for (yr in years) {
  year_data <- data_long %>% filter(year == yr)
  model <- lm(ratio ~ specgroup, data = year_data)
  tukey_results[[yr]] <- HSD.test(model, "specgroup")
}

TM$group <- NA
for (i in 1:nrow(TM)) {
  yr <- TM$year[i]
  sg <- as.character(TM$specgroup[i])
  if (sg %in% rownames(tukey_results[[yr]]$groups)) {
    TM$group[i] <- tukey_results[[yr]]$groups[sg, "groups"]
  }
}

model_all <- lm(ratio ~ year, data = data_long)
tukey_all <- HSD.test(model_all, "year")

pd <- position_dodge(0.8)

y_max <- max(TM$ratio + TM$sd, na.rm = TRUE)

final_plot <- ggplot(data = TM, aes(x = specgroup, y = ratio, fill = specgroup)) +
  geom_bar(stat = "identity", position = pd, width = 0.7) +
  geom_errorbar(aes(ymin = ratio - sd, ymax = ratio + sd), 
                width = 0.25, position = pd, size = 0.6) +
  facet_wrap(~year, nrow = 1) +
  
  geom_text(aes(label = group, y = ratio + sd + 0.08), 
            position = pd, size = 3.5, vjust = 0, fontface = "bold") +
  
  geom_text(aes(label = sprintf("%.2f", ratio), y = ratio/2), 
            position = pd, size = 3.2, color = "white", fontface = "bold") +
  
  geom_text(data = data.frame(year = years, 
                              label = tukey_all$groups[years, "groups"],
                              x_pos = 3, 
                              y_pos = y_max + 0.25),
            aes(x = x_pos, y = y_pos, label = label), 
            inherit.aes = FALSE, size = 4, fontface = "bold", color = "red") +
  
  labs(
    title = "Peduncle (Pupper/Plower) Ratio Across Years by Groups",
    x = "Groups",
    y = "Pupper/Plower Ratio",
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
  scale_x_discrete(labels = c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5")) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    limits = c(0, y_max + 0.35) 
  ) +
  barplot_theme() + 
  theme(
    strip.text = element_text(size = 11, face = "bold", margin = margin(b = 5)),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(margin = margin(r = 5))
  )

print(final_plot)

save_publication_formats(final_plot, "pupper_plower_ratio_analysis", width = 12, height = 6)

cat("ANOVA Results by Year:\n")
for (yr in years) {
  year_data <- data_long %>% filter(year == yr)
  model <- lm(ratio ~ specgroup, data = year_data)
  cat(paste("\nYear", yr, ":\n"))
  print(anova(model))
}

cat("\nTukey HSD Results Between Years:\n")
print(tukey_all)

cat("\nTukey HSD Results Within Each Year:\n")
for (yr in years) {
  cat(paste("\nYear", yr, ":\n"))
  print(tukey_results[[yr]]$groups)
}

cat("\nAll publication-quality plots have been saved in multiple formats!\n")


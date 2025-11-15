setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/v5 article ready for submission/codes redraw figures")
getwd() 

library(ggplot2)
library(agricolae)
library(dplyr)
library(patchwork)
library(tidyr)
library(plyr)

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

data <- read.csv("Figure6_reformatted dataset_grainyield.csv", header = TRUE)

data$specgroup <- factor(data$specgroup, levels = c("1", "2", "3", "4", "5"))
data$sample <- factor(rep(c("2021", "2022", "2023"), each = nrow(data)/3))

summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE,
                      conf.interval = .95, .drop = TRUE) {
  
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
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

data_long <- data %>%
  select(variety, rep, ecozone, release.year, specgroup, sample, X2021, X2022, X2023) %>%
  pivot_longer(cols = c(X2021, X2022, X2023), 
               names_to = "year", 
               values_to = "yield") %>%
  mutate(year = gsub("X", "", year))

TM <- summarySE(data_long, measurevar = "yield", groupvars = c("year", "specgroup"), na.rm = TRUE)


years <- c("2021", "2022", "2023")
tukey_results <- list()

for (yr in years) {
  year_data <- data_long %>% filter(year == yr)
  model <- lm(yield ~ specgroup, data = year_data)
  tukey_results[[yr]] <- HSD.test(model, "specgroup")
}

# Add significance groups (adjust based on your Tukey results)
TM$group <- c("a", "a", "ab", "ab", "b",
              "a", "a", "bc", "ab", "c", 
              "a", "ab", "b", "b", "b")

model_all <- lm(yield ~ year, data = data_long)
tukey_all <- HSD.test(model_all, "year")

final_plot <- ggplot(data = TM, aes(x = specgroup, y = yield, fill = specgroup)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = yield - sd, ymax = yield + sd), 
                width = 0.25, position = position_dodge(0.8), size = 0.6) +
  facet_wrap(~year, nrow = 1) +
  
  geom_text(aes(label = group, y = yield + sd + 150), 
            position = position_dodge(0.8), size = 4, vjust = 0, fontface = "bold") +
  
  geom_text(aes(label = sprintf("%.0f", yield), y = yield/2), 
            position = position_dodge(0.8), size = 3.2, color = "white", fontface = "bold") +
  
  geom_text(data = data.frame(year = years, 
                              label = tukey_all$groups[years, "groups"],
                              x_pos = 3, 
                              y_pos = max(TM$yield + TM$sd, na.rm = TRUE) + 400),
            aes(x = x_pos, y = y_pos, label = label), 
            inherit.aes = FALSE, size = 4, fontface = "bold", color = "red") +
  
  labs(
    title = "Grain Yield Across Years by Groups",
    x = "Groups",
    y = "Grain Yield (kg/ha)",
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  barplot_theme()  


print(final_plot)


save_publication_formats(final_plot, "grain_yield_analysis", width = 12, height = 6)


cat("ANOVA Results by Year:\n")
for (yr in years) {
  year_data <- data_long %>% filter(year == yr)
  model <- lm(yield ~ specgroup, data = year_data)
  cat(paste("\nYear", yr, ":\n"))
  print(anova(model))
}

cat("\nTukey HSD Results Between Years:\n")
print(tukey_all)

# Print detailed group comparisons
cat("\nTukey HSD Results Within Each Year:\n")
for (yr in years) {
  cat(paste("\nYear", yr, ":\n"))
  print(tukey_results[[yr]]$groups)
}

cat("All plots created and saved in publication formats!\n")


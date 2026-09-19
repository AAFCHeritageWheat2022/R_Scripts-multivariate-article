

rm(list = ls())

setwd("/Users/subarnasharma/Desktop/Chapter1 all related in one/September 2026 v20 revision")
getwd()


library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(extrafont)
library(viridis)

# ============================================================
# THEMES
# ============================================================

unified_theme <- function(base_size = 12, base_family = "sans") {
  theme_bw() +
    theme(
      text = element_text(family = base_family, size = base_size, color = "black"),
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5,
                                margin = ggplot2::margin(b = 10)),
      axis.title = element_text(size = base_size, face = "bold"),
      axis.title.x = element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y = element_text(margin = ggplot2::margin(r = 8)),
      axis.text = element_text(size = base_size - 1, face = "bold", color = "black"),
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
      legend.position = "right",
      plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
}

heritability_theme <- function() {
  unified_theme() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11, face = "bold"),
      axis.text.y = element_text(face = "bold", hjust = 1, size = 10),
      panel.grid.major.y = element_line(color = "gray80", size = 0.3),
      panel.grid.major.x = element_line(color = "gray80", size = 0.3),
      legend.position = "top"
    )
}

# ============================================================
# SAVE FUNCTION
# ============================================================

save_publication_formats <- function(plot_obj, base_name, width = 12, height = 6) {
  if (is.null(plot_obj)) {
    cat("Warning: plot_obj is NULL for", base_name, "- skipping save\n")
    return()
  }
  tryCatch({
    ggsave(paste0(base_name, ".tiff"), plot = plot_obj,
           width = width, height = height, units = "in",
           dpi = 600, compression = "lzw")
    cat("Saved:", paste0(base_name, ".tiff"), "\n")
    ggsave(paste0(base_name, ".pdf"), plot = plot_obj,
           width = width, height = height, units = "in", device = cairo_pdf)
    cat("Saved:", paste0(base_name, ".pdf"), "\n")
    ggsave(paste0(base_name, ".png"), plot = plot_obj,
           width = width, height = height, units = "in", dpi = 600)
    cat("Saved:", paste0(base_name, ".png"), "\n")
    ggsave(paste0(base_name, ".jpeg"), plot = plot_obj,
           width = width, height = height, units = "in",
           dpi = 600, quality = 1.0)
    cat("Saved:", paste0(base_name, ".jpeg"), "\n")
  }, error = function(e) {
    cat("Error saving", base_name, ":", e$message, "\n")
  })
}

# ============================================================
# TRAIT LABEL FORMATTER — FINAL v3
# ============================================================

format_trait_label <- function(trait_name) {
  
  keep_literal <- grepl("^(N_|Ab_|Ad_|FD_|CCI_|E_day)", trait_name)
  
  convert_mu <- function(x) {
    x <- gsub("\\(_m\\)",   "(\u00b5m)", x)
    x <- gsub("\\( _m\\)",  "(\u00b5m)", x)
    x <- gsub("\\(_\u00b5m\\)", "(\u00b5m)", x)
    x <- gsub("\\(_um\\)",  "(\u00b5m)", x)
    x <- gsub("\\(um\\)",   "(\u00b5m)", x)
    x <- gsub("_m",          "\u00b5m",  x)
    x <- gsub("um",          "\u00b5m",  x)
    return(x)
  }
  
  escape_literal <- function(x) {
    x <- gsub("\\\\", "\\\\\\\\", x)
    x <- gsub("\"",   "\\\\\"",    x)
    return(x)
  }
  
  # Special case: E_day_anth — literal base + superscripted units
  if (grepl("^E_day", trait_name)) {
    base_name <- gsub("\\s*\\(.*$", "", trait_name)
    base_name <- trimws(base_name)
    return(paste0("bold(\"", base_name, "\")~bold((mol~m^-2~s^-1))"))
  }
  
  if (keep_literal) {
    cleaned <- convert_mu(trait_name)
    cleaned <- escape_literal(cleaned)
    return(paste0("bold(\"", cleaned, "\")"))
  }
  
  label_map <- c(
    "DTH (days)"   = "bold(DTH~(days))",
    "DTA (days)"   = "bold(DTA~(days))",
    "DTM (days)"   = "bold(DTM~(days))",
    "GY (kg/ha)"   = "bold(GY~(kg~ha^-1))",
    "GY"           = "bold(GY)",
    "TGW (g)"      = "bold(TGW~(g))",
    "TGW"          = "bold(TGW)",
    "GW (cm)"      = "bold(GW~(cm))",
    "GW"           = "bold(GW)",
    "Pupper (cm)"  = "bold(P[upper]~(cm))",
    "Pupper"       = "bold(P[upper])",
    "Plower (cm)"  = "bold(P[lower]~(cm))",
    "Plower"       = "bold(P[lower])",
    "PHT (cm)"     = "bold(PHT~(cm))",
    "PHT"          = "bold(PHT)",
    "FLL (cm)"     = "bold(FLL~(cm))",
    "FLL"          = "bold(FLL)",
    "FLA (cm2)"    = "bold(FLA~(cm^2))",
    "FLA"          = "bold(FLA)",
    "HI"           = "bold(HI)",
    "STI_GY"       = "bold(STI[GY])",
    "STI GY"       = "bold(STI[GY])"
  )
  
  if (trait_name %in% names(label_map)) {
    return(label_map[[trait_name]])
  }
  
  cleaned <- convert_mu(trait_name)
  cleaned <- escape_literal(cleaned)
  return(paste0("bold(\"", cleaned, "\")"))
}

# ============================================================
# LOAD AND PREPARE
# ============================================================

data <- read.csv("Figure2_heritabilityselectedtraits.csv", header = TRUE)

cat("Raw trait names:\n"); print(data$traits)

data$traits_parsed <- sapply(data$traits, format_trait_label)

cat("\nFormatted labels:\n"); print(data$traits_parsed)

# Verify
cat("\nVerifying plotmath...\n")
parse_check <- sapply(data$traits_parsed, function(x) {
  tryCatch({ parse(text = x); TRUE },
           error = function(e) { cat("FAILED:", x, "|", e$message, "\n"); FALSE })
})
if (all(parse_check)) cat("All labels parse successfully!\n")

data_long <- data %>%
  pivot_longer(cols = c(her2021, her2022, her2023),
               names_to = "Year", values_to = "Heritability") %>%
  mutate(Year = case_when(
    Year == "her2021" ~ "2021",
    Year == "her2022" ~ "2022",
    Year == "her2023" ~ "2023"
  ))

# ============================================================
# PLOT
# ============================================================

p4 <- ggplot(data, aes(x = avg_heritab, y = reorder(traits_parsed, avg_heritab))) +
  geom_col(aes(fill = avg_heritab), alpha = 0.6, width = 0.7) +
  geom_point(data = data_long,
             aes(x = Heritability, color = Year),
             size = 3, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  scale_fill_viridis_c(
    option = "D", name = "Avg Heritability",
    guide = guide_colorbar(barwidth = 10, barheight = 0.8,
                           title.position = "top",
                           title.theme = element_text(face = "bold", size = 10))
  ) +
  scale_color_manual(
    values = c("2021" = "#D55E00", "2022" = "#0072B2", "2023" = "#009E73"),
    name = "Field Years",
    guide = guide_legend(title.position = "top",
                         title.theme = element_text(face = "bold", size = 10))
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_y_discrete(labels = function(x) parse(text = x)) +
  labs(
    title = "Broad Sense Heritability of Traits",
    subtitle = "Bars show average heritability, points show year-wise heritability",
    x = "Heritability", y = "Traits"
  ) +
  heritability_theme()

print(p4)
save_publication_formats(p4, "Figure2_Heritability_Selected_Traits", width = 12, height = 8)

# ============================================================
# SUMMARY
# ============================================================

summary_stats <- data_long %>%
  group_by(Year) %>%
  summarise(mean = mean(Heritability, na.rm = TRUE),
            sd   = sd(Heritability, na.rm = TRUE),
            min  = min(Heritability, na.rm = TRUE),
            max  = max(Heritability, na.rm = TRUE))
print(summary_stats)
write.csv(summary_stats, "Figure2_Heritability_Summary_Stats.csv", row.names = FALSE)

cat("\nDone!\n")


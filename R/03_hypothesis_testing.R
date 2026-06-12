## ==========================================
## 03_hypothesis_testing.R
## Hypothesis Testing (High-quality figures)
## ==========================================

if (!exists("ames_data")) stop("Run 00_setup.R first to load ames_data.")

library(dplyr)
library(tidyr)
library(ggplot2)

# -------------------------
# Helper: Save high-quality figures
# -------------------------

# Vector PDF (best for LaTeX)
save_plot_pdf <- function(filename, plot, w = 8, h = 5) {
  ggsave(
    filename = file.path("figures", filename),
    plot = plot,
    device = cairo_pdf,      # crisp vector output
    width = w, height = h, units = "in"
  )
}



# -------------------------
# Hypothesis 1:
# Sale Price vs Central Air
# -------------------------

df_air <- ames_data %>%
  dplyr::select(Sale_Price, Central_Air) %>%
  tidyr::drop_na()

# Descriptive statistics
air_summary <- df_air %>%
  group_by(Central_Air) %>%
  summarise(
    n = n(),
    mean_price = mean(Sale_Price),
    median_price = median(Sale_Price),
    sd_price = sd(Sale_Price),
    .groups = "drop"
  )

cat("\n=== Descriptives: Sale_Price by Central_Air ===\n")
print(air_summary)

# Welch two-sample t-test (robust)
t_air <- t.test(Sale_Price ~ Central_Air,
                data = df_air,
                var.equal = FALSE)

cat("\n=== Welch Two-Sample t-test: Sale_Price ~ Central_Air ===\n")
print(t_air)

# Optional supporting plot (high quality): boxplot
p_air <- ggplot(df_air, aes(x = Central_Air, y = Sale_Price)) +
  geom_boxplot() +
  labs(
    title = "Sale Price by Central Air",
    x = "Central Air",
    y = "Sale Price"
  ) +
  theme_minimal(base_size = 12)

save_plot_pdf("hypothesis_central_air_boxplot.pdf", p_air, w = 7, h = 5)


# -------------------------
# Hypothesis 2:
# Sale Price across Neighborhoods
# -------------------------

# Choose top 10 neighborhoods by count (keeps interpretation manageable)
top_neigh <- ames_data %>%
  count(Neighborhood, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(Neighborhood)

df_neigh <- ames_data %>%
  filter(Neighborhood %in% top_neigh) %>%
  dplyr::select(Sale_Price, Neighborhood) %>%
  tidyr::drop_na()

# Descriptive stats (median + IQR are appropriate for skew)
neigh_summary <- df_neigh %>%
  group_by(Neighborhood) %>%
  summarise(
    n = n(),
    median_price = median(Sale_Price),
    IQR_price = IQR(Sale_Price),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

cat("\n=== Descriptives: Sale_Price by Neighborhood (Top 10 by n) ===\n")
print(neigh_summary)

# Kruskal–Wallis (non-parametric)
kw_test <- kruskal.test(Sale_Price ~ Neighborhood, data = df_neigh)

cat("\n=== Kruskal–Wallis test: Sale_Price ~ Neighborhood (Top 10) ===\n")
print(kw_test)

# Post-hoc if significant
if (kw_test$p.value < 0.05) {
  pairwise_results <- pairwise.wilcox.test(
    x = df_neigh$Sale_Price,
    g = df_neigh$Neighborhood,
    p.adjust.method = "BH"
  )
  cat("\n=== Pairwise Wilcoxon (BH-adjusted) ===\n")
  print(pairwise_results)
} else {
  cat("\nKruskal–Wallis not significant; post-hoc tests not run.\n")
}

# High-quality boxplot: Neighborhood
p_neigh <- ggplot(df_neigh, aes(x = Neighborhood, y = Sale_Price)) +
  geom_boxplot() +
  labs(
    title = "Sale Price by Neighborhood (Top 10 by sample size)",
    x = "Neighborhood",
    y = "Sale Price"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot_pdf("hypothesis_neighborhood_boxplot.pdf", p_neigh, w = 9, h = 5)


# -------------------------
# Console summary
# -------------------------
cat("\nHypothesis testing completed.\n")
cat("Saved figures PDF in /figures:\n")
cat("- hypothesis_central_air_boxplot.pdf\n")
cat("- hypothesis_neighborhood_boxplot.pdf\n")


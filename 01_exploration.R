## ==========================================
## 01_exploration.R
## Exploratory Data Analysis (EDA) - factor-safe
## ==========================================

if (!exists("ames_data")) stop("Run 00_setup.R first to load ames_data.")

library(ggplot2)
library(dplyr)
library(tidyr)

# -------------------------
# Helper: Save high-quality PDF figures 
# -------------------------
save_plot_pdf <- function(filename, plot, w = 7, h = 5) {
  ggsave(
    filename = file.path("figures", filename),
    plot = plot,
    device = cairo_pdf,
    width = w, height = h, units = "in"
  )
}


# -------------------------
# 1) Variables (based on your dataset)
# -------------------------
# Core variables
v_sale  <- "Sale_Price"
v_area  <- "Gr_Liv_Area"
v_year  <- "Year_Built"
v_air   <- "Central_Air"
v_neigh <- "Neighborhood"
v_cond  <- "Overall_Cond"   # may be factor/ordinal

# Safety check
needed <- c(v_sale, v_area, v_year, v_air, v_neigh, v_cond)
missing_cols <- setdiff(needed, names(ames_data))
if (length(missing_cols) > 0) {
  stop("These columns are missing in ames_data: ", paste(missing_cols, collapse = ", "))
}

# -------------------------
# 2) Basic structure
# -------------------------
cat("\nRows, Cols:", nrow(ames_data), ncol(ames_data), "\n\n")
print(str(ames_data))

# -------------------------
# 3) Missingness (core variables)
# -------------------------
missing_summary <- ames_data %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(needed),
      ~ sum(is.na(.)),
      .names = "missing_{col}"
    )
  )
print(missing_summary)

# -------------------------
# 4) Summary statistics (numeric only)
# -------------------------
# Identify numeric columns among the core set
core_numeric <- needed[sapply(ames_data[needed], is.numeric)]
cat("\nNumeric core variables used for numeric summaries:\n")
print(core_numeric)

summary_stats <- ames_data %>%
  dplyr::select(dplyr::all_of(core_numeric)) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::everything(),
      list(
        mean   = ~ mean(., na.rm = TRUE),
        sd     = ~ sd(., na.rm = TRUE),
        median = ~ median(., na.rm = TRUE),
        IQR    = ~ IQR(., na.rm = TRUE),
        min    = ~ min(., na.rm = TRUE),
        max    = ~ max(., na.rm = TRUE)
      ),
      .names = "{col}_{fn}"
    )
  )
print(summary_stats)

# -------------------------
# 5) Univariate plots
# -------------------------
p_saleprice <- ggplot(ames_data, aes(x = .data[[v_sale]])) +
  geom_histogram(bins = 40, color = "white") +
  labs(title = "Distribution of Sale Price", x = "Sale Price", y = "Count") +
  theme_minimal()


p_area <- ggplot(ames_data, aes(x = .data[[v_area]])) +
  geom_histogram(bins = 40, color = "white") +
  labs(title = "Distribution of Above-Ground Living Area",
       x = "Living Area (sq ft)", y = "Count") +
  theme_minimal()


# Treat Overall_Cond as categorical (factor-safe)
p_cond <- ggplot(ames_data, aes(x = factor(.data[[v_cond]]))) +
  geom_bar() +
  labs(title = "Overall Condition (Counts)", x = "Overall Condition", y = "Count") +
  theme_minimal()


# -------------------------
# 6) Bivariate plots
# -------------------------
p_scatter <- ggplot(ames_data, aes(x = .data[[v_area]], y = .data[[v_sale]])) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sale Price vs Living Area",
       x = "Living Area (sq ft)", y = "Sale Price") +
  theme_minimal()


p_air <- ggplot(ames_data, aes(x = .data[[v_air]], y = .data[[v_sale]])) +
  geom_boxplot() +
  labs(title = "Sale Price by Central Air", x = "Central Air", y = "Sale Price") +
  theme_minimal()


p_cond_price <- ggplot(ames_data, aes(x = factor(.data[[v_cond]]), y = .data[[v_sale]])) +
  geom_boxplot() +
  labs(title = "Sale Price by Overall Condition",
       x = "Overall Condition", y = "Sale Price") +
  theme_minimal()


# -------------------------
# 7) Neighborhood overview (top 10)
# -------------------------
top_neigh <- ames_data %>%
  dplyr::count(.data[[v_neigh]], sort = TRUE) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::pull(1)

p_neigh <- ames_data %>%
  dplyr::filter(.data[[v_neigh]] %in% top_neigh) %>%
  ggplot(aes(x = .data[[v_neigh]], y = .data[[v_sale]])) +
  geom_boxplot() +
  labs(title = "Sale Price by Neighborhood (Top 10 by sample size)",
       x = "Neighborhood", y = "Sale Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# -------------------------
# 8) Correlation snapshot (numeric candidates only)
# -------------------------
candidate_vars <- c(v_sale, v_area, v_year, "Year_Remod_Add", "Total_Bsmt_SF",
                    "Garage_Area", "Garage_Cars", "TotRms_AbvGrd")

# Keep only those that exist and are numeric
candidate_vars <- candidate_vars[candidate_vars %in% names(ames_data)]
candidate_vars <- candidate_vars[sapply(ames_data[candidate_vars], is.numeric)]

num_candidates <- ames_data %>%
  dplyr::select(dplyr::all_of(candidate_vars)) %>%
  tidyr::drop_na()

cor_mat <- cor(num_candidates)
cat("\nCorrelation matrix (numeric candidates):\n")
print(round(cor_mat, 3))

# -------------------------
# 9) Console notes
# -------------------------
cat("\nEDA completed. Figures saved in /figures:\n")


# -------------------------
# 10) High-quality PDF versions 
# -------------------------

save_plot_pdf("sale_price_hist.pdf", p_saleprice, w = 7, h = 5)
save_plot_pdf("gr_liv_area_hist.pdf", p_area, w = 7, h = 5)
save_plot_pdf("overall_condition_bar.pdf", p_cond, w = 7, h = 5)
save_plot_pdf("saleprice_vs_area.pdf", p_scatter, w = 7, h = 5)
save_plot_pdf("saleprice_central_air.pdf", p_air, w = 7, h = 5)
save_plot_pdf("saleprice_overall_condition.pdf", p_cond_price, w = 7, h = 5)
save_plot_pdf("saleprice_neighborhood.pdf", p_neigh, w = 8, h = 5)

cat("\nHigh-quality PDF figures saved.\n")


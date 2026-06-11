## ==========================================
## 04_regression.R
## Multiple Linear Regression & Prediction
## ==========================================

if (!exists("ames_data")) stop("Run 00_setup.R first to load ames_data.")

library(dplyr)
library(ggplot2)
library(car)
library(caret)

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
# 1) Prepare modelling dataset
# -------------------------

model_data <- ames_data %>%
  dplyr::select(
    Sale_Price,
    Gr_Liv_Area,
    Year_Built,
    Year_Remod_Add,
    Total_Bsmt_SF,
    Garage_Cars,
    Central_Air
  ) %>%
  tidyr::drop_na() %>%
  mutate(
    log_Sale_Price = log(Sale_Price)
  )

cat("\nRows used for regression:", nrow(model_data), "\n")

# -------------------------
# 2) Fit multiple linear regression model
# -------------------------

lm_fit <- lm(
  log_Sale_Price ~ Gr_Liv_Area +
    Year_Built +
    Year_Remod_Add +
    Total_Bsmt_SF +
    Garage_Cars +
    Central_Air,
  data = model_data
)

cat("\n=== Regression Summary ===\n")
print(summary(lm_fit))

# -------------------------
# 3) Multicollinearity check
# -------------------------

cat("\n=== Variance Inflation Factors (VIF) ===\n")
vif_vals <- car::vif(lm_fit)
print(vif_vals)

# Interpretation rule (for your report):
# VIF < 5 → acceptable
# VIF > 10 → problematic

# -------------------------
# 4) Model diagnostics
# -------------------------

# Residuals vs Fitted
p_resid_fitted <- ggplot(lm_fit, aes(.fitted, .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_minimal()

save_plot_pdf("regression_residuals_vs_fitted.pdf", p_resid_fitted)

# Normal Q–Q plot
p_qq <- ggplot(lm_fit, aes(sample = .stdresid)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line() +
  labs(
    title = "Normal Q–Q Plot of Standardised Residuals",
    x = "Theoretical quantiles",
    y = "Standardised residuals"
  ) +
  theme_minimal()

save_plot_pdf("regression_qqplot.pdf", p_qq)

# Cook's Distance
cook_vals <- cooks.distance(lm_fit)
p_cook <- ggplot(
  data.frame(obs = seq_along(cook_vals), cook = cook_vals),
  aes(x = obs, y = cook)
) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 4 / nrow(model_data), linetype = "dashed") +
  labs(
    title = "Cook's Distance",
    x = "Observation",
    y = "Cook's distance"
  ) +
  theme_minimal()

save_plot_pdf("regression_cooks_distance.pdf", p_cook)

# -------------------------
# 5) Train / Test validation
# -------------------------

set.seed(123)

train_index <- createDataPartition(
  model_data$log_Sale_Price,
  p = 0.8,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

lm_train <- lm(
  log_Sale_Price ~ Gr_Liv_Area +
    Year_Built +
    Year_Remod_Add +
    Total_Bsmt_SF +
    Garage_Cars +
    Central_Air,
  data = train_data
)

# Predictions on test set
pred_log <- predict(lm_train, newdata = test_data)

# RMSE on log scale
rmse_log <- RMSE(pred_log, test_data$log_Sale_Price)

cat("\n=== Out-of-sample performance ===\n")
cat("Test RMSE (log scale):", round(rmse_log, 4), "\n")

# -------------------------
# 6) Back-transform for interpretability (optional)
# -------------------------

pred_price <- exp(pred_log)
rmse_price <- RMSE(pred_price, test_data$Sale_Price)

cat("Approximate RMSE on price scale:", round(rmse_price, 0), "\n")

# -------------------------
# 7) Console notes
# -------------------------
cat("\nRegression analysis completed.\nSaved diagnostic figures (PDF only):\n")
cat("- regression_residuals_vs_fitted.pdf\n")
cat("- regression_qqplot.pdf\n")
cat("- regression_cooks_distance.pdf\n")

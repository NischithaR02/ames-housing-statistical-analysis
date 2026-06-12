## ==========================================
## 02_distribution_analysis.R
## Distribution fitting for Sale_Price (robust) + PDF figures
## ==========================================

if (!exists("ames_data")) stop("Run 00_setup.R first to load ames_data.")

library(ggplot2)
library(dplyr)
library(fitdistrplus)

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
# 1) Extract and validate Sale Price
# -------------------------
sale_price <- ames_data$Sale_Price
sale_price <- sale_price[!is.na(sale_price)]

# Gamma requires strictly positive values
if (any(sale_price <= 0)) {
  stop("Sale_Price contains non-positive values; Gamma fit requires > 0.")
}

cat("Sale_Price summary:\n")
print(summary(sale_price))

# -------------------------
# 2) Base histogram (density scale)
# -------------------------
p_hist <- ggplot(data.frame(Sale_Price = sale_price),
                 aes(x = Sale_Price)) +
  geom_histogram(aes(y = ..density..), bins = 40, color = "white") +
  labs(title = "Sale Price with Fitted Distributions",
       x = "Sale Price",
       y = "Density") +
  theme_minimal()

# -------------------------
# 3) Fit distributions
# -------------------------

# Normal (MLE)
fit_norm <- fitdist(sale_price, "norm")

# Log-normal (MLE)
fit_lognorm <- fitdist(sale_price, "lnorm")

# Gamma: try MLE; if fails, fall back to method-of-moments (MME)
fit_gamma <- NULL
gamma_method <- NA_character_

fit_gamma_try <- try(fitdist(sale_price, "gamma"), silent = TRUE)

if (inherits(fit_gamma_try, "try-error")) {
  cat("\nGamma MLE failed. Falling back to method-of-moments (MME).\n")
  fit_gamma <- fitdist(sale_price, "gamma", method = "mme")
  gamma_method <- "MME"
} else {
  fit_gamma <- fit_gamma_try
  gamma_method <- "MLE"
}

cat("\nGamma fit method used:", gamma_method, "\n")
print(fit_gamma)

# -------------------------
# 4) Overlay fitted densities
# -------------------------
# Normal curve parameters
mu  <- unname(fit_norm$estimate["mean"])
sig <- unname(fit_norm$estimate["sd"])

# Log-normal curve parameters
ml <- unname(fit_lognorm$estimate["meanlog"])
sl <- unname(fit_lognorm$estimate["sdlog"])

# Gamma curve parameters (shape + rate)
sh <- unname(fit_gamma$estimate["shape"])
rt <- unname(fit_gamma$estimate["rate"])

p_hist_fit <- p_hist +
  stat_function(fun = dnorm,  args = list(mean = mu, sd = sig), linewidth = 1) +
  stat_function(fun = dlnorm, args = list(meanlog = ml, sdlog = sl), linewidth = 1) +
  stat_function(fun = dgamma, args = list(shape = sh, rate = rt), linewidth = 1)



# ADD high-quality PDF (vector)
save_plot_pdf("saleprice_distribution_fits.pdf", p_hist_fit, w = 7, h = 5)

# -------------------------
# 5) Q–Q plots (Normal vs Log-normal vs Gamma)
# -------------------------
qqnorm(sale_price, main = "Normal Q–Q Plot")
qqline(sale_price)

qqplot(qlnorm(ppoints(length(sale_price)), meanlog = ml, sdlog = sl),
       sale_price,
       main = "Log-normal Q–Q",
       xlab = "Theoretical quantiles",
       ylab = "Sample quantiles")

qqplot(qgamma(ppoints(length(sale_price)), shape = sh, rate = rt),
       sale_price,
       main = paste0("Gamma Q–Q (", gamma_method, ")"),
       xlab = "Theoretical quantiles",
       ylab = "Sample quantiles")

par(mfrow = c(1, 1))
dev.off()

# ADD a PDF version (vector, higher quality in LaTeX)
pdf("figures/qqplots_saleprice.pdf", width = 11, height = 3.5)
par(mfrow = c(1, 3))

qqnorm(sale_price, main = "Normal Q–Q Plot")
qqline(sale_price)

qqplot(qlnorm(ppoints(length(sale_price)), meanlog = ml, sdlog = sl),
       sale_price,
       main = "Log-normal Q–Q",
       xlab = "Theoretical quantiles",
       ylab = "Sample quantiles")

qqplot(qgamma(ppoints(length(sale_price)), shape = sh, rate = rt),
       sale_price,
       main = paste0("Gamma Q–Q (", gamma_method, ")"),
       xlab = "Theoretical quantiles",
       ylab = "Sample quantiles")

par(mfrow = c(1, 1))
dev.off()

# -------------------------
# 6) Model comparison
# -------------------------
aic_table <- data.frame(
  Distribution = c("Normal", "Log-normal", "Gamma"),
  AIC = c(fit_norm$aic,
          fit_lognorm$aic,
          if (gamma_method == "MLE") fit_gamma$aic else NA_real_)
)

cat("\nAIC table (Gamma AIC only shown if MLE succeeded):\n")
print(aic_table)

# -------------------------
# 7) Log-transform check (diagnostic)
# -------------------------
log_sale <- log(sale_price)

p_log_hist <- ggplot(data.frame(log_Sale_Price = log_sale),
                     aes(x = log_Sale_Price)) +
  geom_histogram(bins = 40, color = "white") +
  labs(title = "Log-transformed Sale Price",
       x = "log(Sale Price)",
       y = "Count") +
  theme_minimal()



# ADD high-quality PDF (vector)
save_plot_pdf("log_saleprice_hist.pdf", p_log_hist, w = 7, h = 5)

# -------------------------
# 8) Console notes
# -------------------------
cat("\nDistribution fitting completed.\nSaved figures:\n")

cat("Added PDF versions:\n")
cat("- figures/saleprice_distribution_fits.pdf\n")
cat("- figures/qqplots_saleprice.pdf\n")
cat("- figures/log_saleprice_hist.pdf\n")

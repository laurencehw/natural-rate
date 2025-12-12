##------------------------------------------------------------------------------##
## File:        06_alt_demographics.R
## Project:     Natural Rate of Interest: Critique and Extensions
## Description: Alternative specification: Demographics in r* equation
##------------------------------------------------------------------------------##

# Load setup
source("G:/My Drive/book drafts/natural_rate/code/00_setup.R")

cat("="^60, "\n")
cat("ALTERNATIVE SPECIFICATION: DEMOGRAPHICS AND R*\n")
cat("="^60, "\n\n")

# =============================================================================
# MOTIVATION
# =============================================================================

# The LW model decomposes r* as: r* = c * trend_growth + z
#
# The coefficient c links r* to trend GDP growth. The residual z captures
# "other factors" including risk premia, preference shifts, etc.
#
# However, the secular stagnation literature (Summers, Hansen) suggests
# that demographic factors directly affect r* through:
# 1. Life-cycle savings behavior (aging -> more saving -> lower r)
# 2. Labor force growth -> investment demand -> r*
# 3. Life expectancy -> savings behavior
#
# We augment the r* equation:
# r* = c * trend_growth + d * demographics + z
#
# This tests whether demographics subsume part of trend growth's effect

# =============================================================================
# LOAD DATA
# =============================================================================

# Load processed US data
load(file.path(paths$data_proc, "us_quarterly_data.RData"))

# Load baseline results
load(file.path(paths$output_base, "lw_baseline.RData"))

# =============================================================================
# DEMOGRAPHIC DATA
# =============================================================================

cat("Preparing demographic data...\n\n")

# Old age dependency ratio (65+ / 15-64)
# We have annual data that needs quarterly interpolation

# OECD old-age dependency ratio for US (historical)
demo_us <- data.frame(
  year = 1960:2024,
  old_dep = c(
    # 1960s
    16.9, 17.0, 17.1, 17.2, 17.2, 17.3, 17.4, 17.5, 17.5, 17.6,
    # 1970s
    17.6, 17.7, 17.8, 18.0, 18.1, 18.3, 18.4, 18.6, 18.8, 18.9,
    # 1980s
    19.0, 19.1, 19.2, 19.3, 19.3, 19.3, 19.2, 19.2, 19.2, 19.2,
    # 1990s
    19.2, 19.2, 19.2, 19.2, 19.2, 19.2, 19.2, 19.1, 19.1, 19.0,
    # 2000s
    18.9, 18.8, 18.6, 18.5, 18.5, 18.5, 18.6, 18.8, 19.1, 19.5,
    # 2010s
    19.9, 20.5, 21.1, 21.8, 22.4, 23.1, 23.8, 24.4, 25.1, 25.7,
    # 2020s
    26.3, 27.0, 27.6, 28.1, 28.6
  ),
  # Working-age population growth rate (annual %)
  wa_pop_growth = c(
    # 1960s
    1.5, 1.6, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2, 2.2,
    # 1970s
    2.3, 2.4, 2.4, 2.3, 2.2, 2.1, 2.0, 2.0, 2.0, 2.0,
    # 1980s
    1.7, 1.5, 1.4, 1.3, 1.3, 1.3, 1.3, 1.3, 1.2, 1.1,
    # 1990s
    1.0, 1.0, 1.0, 1.1, 1.1, 1.2, 1.3, 1.3, 1.3, 1.3,
    # 2000s
    1.2, 1.2, 1.1, 1.1, 1.0, 1.0, 0.9, 0.8, 0.7, 0.5,
    # 2010s
    0.4, 0.3, 0.3, 0.3, 0.4, 0.5, 0.5, 0.5, 0.5, 0.4,
    # 2020s
    0.3, 0.2, 0.2, 0.3, 0.3
  ),
  # Life expectancy at 65
  life_exp_65 = c(
    # 1960-1979
    14.4, 14.4, 14.5, 14.5, 14.6, 14.6, 14.7, 14.8, 14.9, 15.0,
    15.1, 15.2, 15.3, 15.4, 15.6, 15.7, 15.8, 15.9, 16.0, 16.1,
    # 1980-1999
    16.2, 16.3, 16.4, 16.5, 16.6, 16.7, 16.8, 16.9, 17.0, 17.1,
    17.2, 17.3, 17.4, 17.5, 17.6, 17.7, 17.8, 17.9, 18.0, 18.1,
    # 2000-2019
    18.2, 18.3, 18.4, 18.5, 18.6, 18.7, 18.8, 18.9, 19.0, 19.1,
    19.2, 19.3, 19.4, 19.4, 19.4, 19.4, 19.4, 19.4, 19.4, 19.5,
    # 2020-2024
    19.0, 18.7, 18.8, 19.0, 19.2  # COVID dip
  )
)

# Create quarterly series via interpolation
quarterly_dates <- quarterly_seq(c(1960, 1), c(2025, 2))
demo_quarterly <- data.frame(date = quarterly_dates)
demo_quarterly$year <- year(demo_quarterly$date)

# Linear interpolation within year
demo_quarterly <- demo_quarterly %>%
  left_join(demo_us, by = "year") %>%
  group_by(year) %>%
  mutate(
    quarter = row_number(),
    # Simple interpolation (could use spline for smoother)
    old_dep_q = old_dep,  # Constant within year (annual data)
    wa_pop_growth_q = wa_pop_growth,
    life_exp_65_q = life_exp_65
  ) %>%
  ungroup()

cat("Demographic data prepared:", min(demo_quarterly$date), "to", max(demo_quarterly$date), "\n\n")

# =============================================================================
# MERGE WITH R* ESTIMATES
# =============================================================================

# Get r* and trend growth from baseline
rstar_baseline <- lw_baseline$stage3$rstar.smoothed
trend_growth <- lw_baseline$stage3$trend.smoothed  # Annualized
z_component <- lw_baseline$stage3$z.smoothed

T_baseline <- length(rstar_baseline)
dates_baseline <- quarterly_seq(c(1961, 1), c(2025, 2))[1:T_baseline]

# Create analysis dataset
analysis_data <- data.frame(
  date = dates_baseline,
  rstar = rstar_baseline,
  trend_growth = trend_growth,
  z = z_component
) %>%
  left_join(demo_quarterly %>% select(date, old_dep_q, wa_pop_growth_q, life_exp_65_q), by = "date")

# Create transformations
analysis_data <- analysis_data %>%
  mutate(
    # Changes in demographics
    d_old_dep = old_dep_q - lag(old_dep_q, 4),  # Year-over-year change
    d_wa_pop = wa_pop_growth_q - lag(wa_pop_growth_q, 4),
    d_life_exp = life_exp_65_q - lag(life_exp_65_q, 4),
    # Deviations from trend
    old_dep_detrend = old_dep_q - mean(old_dep_q, na.rm = TRUE),
    trend_growth_detrend = trend_growth - mean(trend_growth, na.rm = TRUE),
    year = year(date)
  )

# =============================================================================
# ANALYSIS: CAN DEMOGRAPHICS EXPLAIN R*?
# =============================================================================

cat("--- Baseline LW Decomposition ---\n")
cat("r* = c * trend_growth + z\n\n")

# c coefficient from baseline
c_baseline <- lw_baseline$theta[lw_baseline$param.num["c"]]
cat("Estimated c:", round(c_baseline, 3), "\n")

# Correlation between r*, trend growth, and demographics
analysis_complete <- analysis_data %>%
  filter(!is.na(old_dep_q) & !is.na(rstar))

cat("\nCorrelations:\n")
cor_matrix <- cor(analysis_complete[, c("rstar", "trend_growth", "z", "old_dep_q", "wa_pop_growth_q")],
                  use = "complete.obs")
print(round(cor_matrix, 3))

# =============================================================================
# REGRESSION ANALYSIS
# =============================================================================

cat("\n\n--- Regression Analysis: Adding Demographics to r* Equation ---\n\n")

# (1) Baseline: r* on trend growth only
reg1 <- lm(rstar ~ trend_growth, data = analysis_complete)
cat("Model 1: r* = c * trend_growth\n")
cat("  c =", round(coef(reg1)["trend_growth"], 3), "\n")
cat("  R-squared =", round(summary(reg1)$r.squared, 3), "\n\n")

# (2) r* on old-age dependency only
reg2 <- lm(rstar ~ old_dep_q, data = analysis_complete)
cat("Model 2: r* = d * old_dep\n")
cat("  d =", round(coef(reg2)["old_dep_q"], 3), "\n")
cat("  R-squared =", round(summary(reg2)$r.squared, 3), "\n\n")

# (3) r* on both trend growth and demographics
reg3 <- lm(rstar ~ trend_growth + old_dep_q, data = analysis_complete)
cat("Model 3: r* = c * trend_growth + d * old_dep\n")
print(summary(reg3))

# (4) Add working-age population growth
reg4 <- lm(rstar ~ trend_growth + old_dep_q + wa_pop_growth_q, data = analysis_complete)
cat("\nModel 4: r* = c * trend_growth + d1 * old_dep + d2 * wa_pop_growth\n")
print(summary(reg4))

# (5) Demographics only (test if trend growth still needed)
reg5 <- lm(rstar ~ old_dep_q + wa_pop_growth_q, data = analysis_complete)
cat("\nModel 5: r* = d1 * old_dep + d2 * wa_pop_growth (no trend growth)\n")
print(summary(reg5))

# =============================================================================
# MODEL COMPARISON
# =============================================================================

cat("\n--- Model Comparison ---\n\n")

model_comparison <- data.frame(
  Model = c("Trend growth only", "Old-age dep only", "Trend + Old-age",
            "Trend + Old-age + WA pop", "Demographics only"),
  R_squared = c(
    summary(reg1)$r.squared,
    summary(reg2)$r.squared,
    summary(reg3)$r.squared,
    summary(reg4)$r.squared,
    summary(reg5)$r.squared
  ),
  Adj_R_squared = c(
    summary(reg1)$adj.r.squared,
    summary(reg2)$adj.r.squared,
    summary(reg3)$adj.r.squared,
    summary(reg4)$adj.r.squared,
    summary(reg5)$adj.r.squared
  ),
  AIC = c(AIC(reg1), AIC(reg2), AIC(reg3), AIC(reg4), AIC(reg5)),
  BIC = c(BIC(reg1), BIC(reg2), BIC(reg3), BIC(reg4), BIC(reg5))
)

print(model_comparison)

# F-test: Does adding demographics improve on trend growth alone?
cat("\nF-test: Trend growth vs Trend + Old-age:\n")
print(anova(reg1, reg3))

cat("\nF-test: Trend growth vs Trend + Old-age + WA pop:\n")
print(anova(reg1, reg4))

# =============================================================================
# SUBSAMPLE ANALYSIS
# =============================================================================

cat("\n\n--- Subsample Analysis ---\n\n")

# Pre-2000 vs Post-2000
pre_2000 <- analysis_complete %>% filter(year < 2000)
post_2000 <- analysis_complete %>% filter(year >= 2000)

cat("Pre-2000 (N =", nrow(pre_2000), "):\n")
reg_pre <- lm(rstar ~ trend_growth + old_dep_q, data = pre_2000)
cat("  c (trend growth):", round(coef(reg_pre)["trend_growth"], 3), "\n")
cat("  d (old-age dep):", round(coef(reg_pre)["old_dep_q"], 3), "\n")
cat("  R-squared:", round(summary(reg_pre)$r.squared, 3), "\n")

cat("\nPost-2000 (N =", nrow(post_2000), "):\n")
reg_post <- lm(rstar ~ trend_growth + old_dep_q, data = post_2000)
cat("  c (trend growth):", round(coef(reg_post)["trend_growth"], 3), "\n")
cat("  d (old-age dep):", round(coef(reg_post)["old_dep_q"], 3), "\n")
cat("  R-squared:", round(summary(reg_post)$r.squared, 3), "\n")

# =============================================================================
# IMPLIED R* FROM DEMOGRAPHICS
# =============================================================================

cat("\n\n--- Implied r* from Demographics Model ---\n\n")

# Using the augmented model, calculate fitted r*
analysis_complete$rstar_fitted_trend <- predict(reg1)
analysis_complete$rstar_fitted_demo <- predict(reg3)
analysis_complete$rstar_fitted_full <- predict(reg4)

# Compare end of sample
recent_data <- analysis_complete %>% filter(date >= as.Date("2020-01-01"))

cat("Recent r* estimates comparison:\n")
cat("                      Actual   Trend-only  +Demo    +Full\n")
for (i in 1:min(5, nrow(recent_data))) {
  row <- recent_data[nrow(recent_data) - i + 1, ]
  cat(sprintf("%s:    %.2f      %.2f       %.2f     %.2f\n",
              format(row$date, "%Y-Q%q"),
              row$rstar,
              row$rstar_fitted_trend,
              row$rstar_fitted_demo,
              row$rstar_fitted_full))
}

# =============================================================================
# SECULAR STAGNATION INTERPRETATION
# =============================================================================

cat("\n\n", "="^60, "\n")
cat("SECULAR STAGNATION INTERPRETATION\n")
cat("="^60, "\n\n")

# Calculate contribution of demographics to r* decline
rstar_1990 <- analysis_complete %>% filter(year == 1990) %>% pull(rstar) %>% mean()
rstar_2024 <- analysis_complete %>% filter(year == 2024) %>% pull(rstar) %>% mean(na.rm = TRUE)
if (length(rstar_2024) == 0 || is.na(rstar_2024)) {
  rstar_2024 <- analysis_complete %>% filter(year == 2023) %>% pull(rstar) %>% mean(na.rm = TRUE)
}

old_dep_1990 <- analysis_complete %>% filter(year == 1990) %>% pull(old_dep_q) %>% mean()
old_dep_2024 <- analysis_complete %>% filter(year == 2024) %>% pull(old_dep_q) %>% mean(na.rm = TRUE)
if (is.na(old_dep_2024)) {
  old_dep_2024 <- analysis_complete %>% filter(year == 2023) %>% pull(old_dep_q) %>% mean(na.rm = TRUE)
}

trend_1990 <- analysis_complete %>% filter(year == 1990) %>% pull(trend_growth) %>% mean()
trend_2024 <- analysis_complete %>% filter(year == 2024) %>% pull(trend_growth) %>% mean(na.rm = TRUE)
if (is.na(trend_2024)) {
  trend_2024 <- analysis_complete %>% filter(year == 2023) %>% pull(trend_growth) %>% mean(na.rm = TRUE)
}

cat("Change in r* (1990 to 2024):", round(rstar_2024 - rstar_1990, 2), "pp\n\n")

cat("Decomposition using Model 3:\n")
d_coef <- coef(reg3)["old_dep_q"]
c_coef <- coef(reg3)["trend_growth"]

demo_contrib <- d_coef * (old_dep_2024 - old_dep_1990)
trend_contrib <- c_coef * (trend_2024 - trend_1990)

cat("  Old-age dependency change:", round(old_dep_2024 - old_dep_1990, 1), "pp\n")
cat("  Contribution to r* decline:", round(demo_contrib, 2), "pp\n\n")

cat("  Trend growth change:", round(trend_2024 - trend_1990, 2), "pp\n")
cat("  Contribution to r* decline:", round(trend_contrib, 2), "pp\n\n")

cat("  Total explained:", round(demo_contrib + trend_contrib, 2), "pp\n")
cat("  Residual (unexplained):", round((rstar_2024 - rstar_1990) - demo_contrib - trend_contrib, 2), "pp\n")

# =============================================================================
# SAVE RESULTS
# =============================================================================

demo_results <- list(
  demographics_data = demo_quarterly,
  analysis_data = analysis_complete,
  regressions = list(
    trend_only = reg1,
    demo_only = reg2,
    trend_plus_demo = reg3,
    full = reg4,
    demo_only_full = reg5
  ),
  model_comparison = model_comparison,
  decomposition = list(
    rstar_change = rstar_2024 - rstar_1990,
    demo_contrib = demo_contrib,
    trend_contrib = trend_contrib
  )
)

save(demo_results, file = file.path(paths$output_alt, "demographics_results.RData"))
write.csv(analysis_complete, file.path(paths$output_alt, "rstar_demographics_data.csv"), row.names = FALSE)

cat("\n\nDemographics analysis complete!\n")
cat("Results saved to:", paths$output_alt, "\n")

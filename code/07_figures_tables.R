##------------------------------------------------------------------------------##
## File:        07_figures_tables.R
## Project:     Natural Rate of Interest: Critique and Extensions
## Description: Generate publication-quality figures and tables
##------------------------------------------------------------------------------##

# Load setup
source("G:/My Drive/book drafts/natural_rate/code/00_setup.R")

cat("="^60, "\n")
cat("GENERATING FIGURES AND TABLES\n")
cat("="^60, "\n\n")

# =============================================================================
# LOAD ALL RESULTS
# =============================================================================

# Baseline results
load(file.path(paths$output_base, "lw_baseline.RData"))
tryCatch({
  load(file.path(paths$output_base, "hlw_us_baseline.RData"))
  load(file.path(paths$output_base, "hlw_ea_baseline.RData"))
}, error = function(e) {
  cat("Note: Some baseline results not available\n")
})

# Sensitivity results
tryCatch({
  load(file.path(paths$output_sens, "sensitivity_results.RData"))
}, error = function(e) {
  cat("Note: Sensitivity results not available\n")
})

# Alternative specification results
tryCatch({
  load(file.path(paths$output_alt, "fci_alternative.RData"))
  load(file.path(paths$output_alt, "tvp_phillips_results.RData"))
  load(file.path(paths$output_alt, "demographics_results.RData"))
}, error = function(e) {
  cat("Note: Some alternative results not available\n")
})

# =============================================================================
# FIGURE 1: BASELINE R* ESTIMATES (US)
# =============================================================================

cat("Creating Figure 1: Baseline r* estimates...\n")

# Prepare data
rstar_us <- lw_baseline$stage3$rstar.smoothed
dates_us <- quarterly_seq(c(1961, 1), c(2025, 2))[1:length(rstar_us)]

fig1_data <- data.frame(
  date = dates_us,
  rstar_smoothed = rstar_us,
  rstar_filtered = lw_baseline$stage3$rstar.filtered,
  trend_growth = lw_baseline$stage3$trend.smoothed
)

p1 <- ggplot(fig1_data, aes(x = date)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(aes(y = rstar_smoothed, color = "r* (two-sided)"), linewidth = 1) +
  geom_line(aes(y = rstar_filtered, color = "r* (one-sided)"), linewidth = 0.7, linetype = "dashed") +
  geom_ribbon(aes(ymin = rstar_smoothed - 1.5, ymax = rstar_smoothed + 1.5),
              alpha = 0.2, fill = "steelblue") +
  scale_color_manual(values = c("r* (two-sided)" = "steelblue", "r* (one-sided)" = "darkblue")) +
  labs(
    title = "Natural Rate of Interest (r*) - United States",
    subtitle = "Laubach-Williams Model, Baseline Specification",
    x = NULL,
    y = "Percent",
    color = NULL,
    caption = "Source: Authors' calculations based on FRBNY methodology.\nShaded area represents approximately 95% confidence interval."
  ) +
  theme_publication() +
  theme(legend.position = c(0.8, 0.9))

ggsave(file.path(paths$figures, "fig1_baseline_rstar_us.pdf"), p1, width = 10, height = 6)
ggsave(file.path(paths$figures, "fig1_baseline_rstar_us.png"), p1, width = 10, height = 6, dpi = 300)

# =============================================================================
# FIGURE 2: R* AND TREND GROWTH
# =============================================================================

cat("Creating Figure 2: r* and trend growth...\n")

# Scale trend growth (annualized) to match r* units
c_coef <- lw_baseline$theta[lw_baseline$param.num["c"]]

fig2_data <- data.frame(
  date = dates_us,
  rstar = rstar_us,
  trend_growth = fig1_data$trend_growth,
  trend_component = c_coef * fig1_data$trend_growth,
  z_component = lw_baseline$stage3$z.smoothed
)

p2 <- ggplot(fig2_data, aes(x = date)) +
  geom_line(aes(y = rstar, color = "r*"), linewidth = 1) +
  geom_line(aes(y = trend_component, color = paste0("c*g (c=", round(c_coef, 2), ")")), linewidth = 0.8) +
  geom_line(aes(y = z_component, color = "z (other factors)"), linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("r*" = "black", "z (other factors)" = "coral",
                                 setNames("forestgreen", paste0("c*g (c=", round(c_coef, 2), ")")))) +
  labs(
    title = "Decomposition of r*",
    subtitle = "r* = c x trend growth + z",
    x = NULL,
    y = "Percent",
    color = NULL
  ) +
  theme_publication()

ggsave(file.path(paths$figures, "fig2_rstar_decomposition.pdf"), p2, width = 10, height = 6)
ggsave(file.path(paths$figures, "fig2_rstar_decomposition.png"), p2, width = 10, height = 6, dpi = 300)

# =============================================================================
# FIGURE 3: SENSITIVITY TO CONSTRAINTS
# =============================================================================

cat("Creating Figure 3: Sensitivity analysis...\n")

if (exists("sensitivity_results")) {
  # IS curve sensitivity
  fig3a_data <- data.frame(date = dates_us)
  for (lab in names(sensitivity_results$a_r)) {
    if (!is.null(sensitivity_results$a_r[[lab]]$rstar)) {
      rstar_vec <- sensitivity_results$a_r[[lab]]$rstar
      if (length(rstar_vec) == length(dates_us)) {
        fig3a_data[[lab]] <- rstar_vec
      }
    }
  }

  if (ncol(fig3a_data) > 1) {
    fig3a_long <- fig3a_data %>%
      pivot_longer(-date, names_to = "Specification", values_to = "rstar")

    p3a <- ggplot(fig3a_long, aes(x = date, y = rstar, color = Specification)) +
      geom_line(linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        title = "Sensitivity of r* to IS Curve Slope Constraint",
        x = NULL,
        y = "Percent"
      ) +
      theme_publication() +
      scale_color_brewer(palette = "Set1")

    ggsave(file.path(paths$figures, "fig3a_sensitivity_is.pdf"), p3a, width = 10, height = 6)
  }

  # Phillips curve sensitivity
  fig3b_data <- data.frame(date = dates_us)
  for (lab in names(sensitivity_results$b_y)) {
    if (!is.null(sensitivity_results$b_y[[lab]]$rstar)) {
      rstar_vec <- sensitivity_results$b_y[[lab]]$rstar
      if (length(rstar_vec) == length(dates_us)) {
        fig3b_data[[lab]] <- rstar_vec
      }
    }
  }

  if (ncol(fig3b_data) > 1) {
    fig3b_long <- fig3b_data %>%
      pivot_longer(-date, names_to = "Specification", values_to = "rstar")

    p3b <- ggplot(fig3b_long, aes(x = date, y = rstar, color = Specification)) +
      geom_line(linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        title = "Sensitivity of r* to Phillips Curve Slope Constraint",
        x = NULL,
        y = "Percent"
      ) +
      theme_publication() +
      scale_color_brewer(palette = "Set2")

    ggsave(file.path(paths$figures, "fig3b_sensitivity_phillips.pdf"), p3b, width = 10, height = 6)
  }
}

# =============================================================================
# FIGURE 4: ROLLING PHILLIPS CURVE SLOPE
# =============================================================================

cat("Creating Figure 4: Time-varying Phillips curve...\n")

if (exists("tvp_phillips_results")) {
  rolling_data <- tvp_phillips_results$rolling_results

  p4 <- ggplot(rolling_data, aes(x = end_date)) +
    geom_line(aes(y = b_y), color = "steelblue", linewidth = 1) +
    geom_ribbon(aes(ymin = b_y - 1.96 * b_y_se, ymax = b_y + 1.96 * b_y_se),
                alpha = 0.2, fill = "steelblue") +
    geom_hline(yintercept = 0.025, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
    annotate("text", x = as.Date("1990-01-01"), y = 0.03, label = "LW Constraint (0.025)",
             color = "red", size = 3, hjust = 0) +
    labs(
      title = "Rolling Estimates of Phillips Curve Slope",
      subtitle = "15-year rolling windows",
      x = NULL,
      y = "Coefficient on Output Gap",
      caption = "Shaded area represents 95% confidence interval."
    ) +
    theme_publication()

  ggsave(file.path(paths$figures, "fig4_phillips_slope.pdf"), p4, width = 10, height = 6)
  ggsave(file.path(paths$figures, "fig4_phillips_slope.png"), p4, width = 10, height = 6, dpi = 300)
}

# =============================================================================
# FIGURE 5: DEMOGRAPHICS AND R*
# =============================================================================

cat("Creating Figure 5: Demographics and r*...\n")

if (exists("demo_results")) {
  demo_data <- demo_results$analysis_data

  # Dual axis plot
  scale_factor <- 4  # To align old_dep with rstar scale

  p5 <- ggplot(demo_data, aes(x = date)) +
    geom_line(aes(y = rstar, color = "r*"), linewidth = 1) +
    geom_line(aes(y = (old_dep_q - 20) / scale_factor, color = "Old-Age Dependency Ratio"),
              linewidth = 1, linetype = "dashed") +
    scale_y_continuous(
      name = "r* (Percent)",
      sec.axis = sec_axis(~ . * scale_factor + 20, name = "Old-Age Dependency Ratio")
    ) +
    scale_color_manual(values = c("r*" = "steelblue", "Old-Age Dependency Ratio" = "coral")) +
    labs(
      title = "Natural Rate of Interest and Demographics",
      x = NULL,
      color = NULL
    ) +
    theme_publication() +
    theme(legend.position = "bottom")

  ggsave(file.path(paths$figures, "fig5_rstar_demographics.pdf"), p5, width = 10, height = 6)
  ggsave(file.path(paths$figures, "fig5_rstar_demographics.png"), p5, width = 10, height = 6, dpi = 300)
}

# =============================================================================
# FIGURE 6: US VS EURO AREA R*
# =============================================================================

cat("Creating Figure 6: US vs Euro Area...\n")

if (exists("hlw_us_baseline") && exists("hlw_ea_baseline")) {
  # Align the series
  us_rstar <- hlw_us_baseline$estimation$out.stage3$rstar.smoothed
  ea_rstar <- hlw_ea_baseline$estimation$out.stage3$rstar.smoothed

  # EA has shorter history, need to align
  # US starts 1961, EA starts 1972
  dates_us_hlw <- quarterly_seq(c(1961, 1), c(2025, 2))[1:length(us_rstar)]
  dates_ea_hlw <- quarterly_seq(c(1972, 1), c(2025, 2))[1:length(ea_rstar)]

  fig6_us <- data.frame(date = dates_us_hlw, rstar = us_rstar, country = "United States")
  fig6_ea <- data.frame(date = dates_ea_hlw, rstar = ea_rstar, country = "Euro Area")
  fig6_data <- rbind(fig6_us, fig6_ea)

  p6 <- ggplot(fig6_data, aes(x = date, y = rstar, color = country)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c("United States" = "steelblue", "Euro Area" = "coral")) +
    labs(
      title = "Natural Rate of Interest: US vs Euro Area",
      subtitle = "Holston-Laubach-Williams Model",
      x = NULL,
      y = "Percent",
      color = NULL
    ) +
    theme_publication()

  ggsave(file.path(paths$figures, "fig6_us_ea_comparison.pdf"), p6, width = 10, height = 6)
  ggsave(file.path(paths$figures, "fig6_us_ea_comparison.png"), p6, width = 10, height = 6, dpi = 300)
}

# =============================================================================
# TABLE 1: BASELINE PARAMETER ESTIMATES
# =============================================================================

cat("Creating Table 1: Baseline parameters...\n")

# Extract parameters from baseline
theta <- lw_baseline$theta
param_num <- lw_baseline$param.num

table1 <- data.frame(
  Parameter = c("a_1 (Output gap AR1)", "a_2 (Output gap AR2)", "a_3 (IS curve slope)",
                "b_1 (Inflation AR1)", "b_2 (Inflation AR2-4)", "b_3 (Phillips slope)",
                "b_4 (Oil price)", "b_5 (Import price)", "c (r* loading on g)",
                "sigma_1 (Output)", "sigma_2 (Inflation)", "sigma_4 (Potential)",
                "phi (COVID shift)", "lambda_g", "lambda_z"),
  Estimate = c(
    theta[param_num["a_1"]], theta[param_num["a_2"]], theta[param_num["a_3"]],
    theta[param_num["b_1"]], theta[param_num["b_2"]], theta[param_num["b_3"]],
    theta[param_num["b_4"]], theta[param_num["b_5"]], theta[param_num["c"]],
    theta[param_num["sigma_1"]], theta[param_num["sigma_2"]], theta[param_num["sigma_4"]],
    theta[param_num["phi"]],
    lw_baseline$lambda.g, lw_baseline$lambda.z
  )
)

table1$Estimate <- round(table1$Estimate, 4)

write.csv(table1, file.path(paths$tables, "table1_baseline_parameters.csv"), row.names = FALSE)

# LaTeX version
sink(file.path(paths$tables, "table1_baseline_parameters.tex"))
cat("\\begin{table}[htbp]\n")
cat("\\centering\n")
cat("\\caption{Baseline Parameter Estimates: Laubach-Williams Model}\n")
cat("\\label{tab:baseline_params}\n")
cat("\\begin{tabular}{lc}\n")
cat("\\hline\\hline\n")
cat("Parameter & Estimate \\\\\n")
cat("\\hline\n")
for (i in 1:nrow(table1)) {
  cat(sprintf("%s & %.4f \\\\\n", table1$Parameter[i], table1$Estimate[i]))
}
cat("\\hline\\hline\n")
cat("\\end{tabular}\n")
cat("\\end{table}\n")
sink()

# =============================================================================
# TABLE 2: SENSITIVITY ANALYSIS SUMMARY
# =============================================================================

cat("Creating Table 2: Sensitivity summary...\n")

if (exists("sensitivity_results")) {
  # Compile results
  sens_summary <- list()

  # IS curve sensitivity
  for (lab in names(sensitivity_results$a_r)) {
    res <- sensitivity_results$a_r[[lab]]
    if (!is.null(res$rstar)) {
      sens_summary[[paste0("IS: ", lab)]] <- data.frame(
        Category = "IS Slope",
        Specification = lab,
        Final_rstar = round(tail(res$rstar, 1), 2),
        Mean_rstar_2000s = round(mean(res$rstar[dates_us >= as.Date("2000-01-01")], na.rm = TRUE), 2),
        Lambda_g = round(res$lambda_g, 4),
        Lambda_z = round(res$lambda_z, 4)
      )
    }
  }

  # Phillips sensitivity
  for (lab in names(sensitivity_results$b_y)) {
    res <- sensitivity_results$b_y[[lab]]
    if (!is.null(res$rstar)) {
      sens_summary[[paste0("PC: ", lab)]] <- data.frame(
        Category = "Phillips Slope",
        Specification = lab,
        Final_rstar = round(tail(res$rstar, 1), 2),
        Mean_rstar_2000s = round(mean(res$rstar[dates_us >= as.Date("2000-01-01")], na.rm = TRUE), 2),
        Lambda_g = round(res$lambda_g, 4),
        Lambda_z = round(res$lambda_z, 4)
      )
    }
  }

  if (length(sens_summary) > 0) {
    table2 <- do.call(rbind, sens_summary)
    write.csv(table2, file.path(paths$tables, "table2_sensitivity_summary.csv"), row.names = FALSE)
  }
}

# =============================================================================
# TABLE 3: MODEL COMPARISON (DEMOGRAPHICS)
# =============================================================================

cat("Creating Table 3: Demographics model comparison...\n")

if (exists("demo_results")) {
  write.csv(demo_results$model_comparison,
            file.path(paths$tables, "table3_demographics_comparison.csv"), row.names = FALSE)
}

# =============================================================================
# COMPLETION
# =============================================================================

cat("\n", "="^60, "\n")
cat("FIGURES AND TABLES COMPLETE\n")
cat("="^60, "\n")
cat("\nFigures saved to:", paths$figures, "\n")
cat("Tables saved to:", paths$tables, "\n")

# List all created files
cat("\nCreated files:\n")
cat("  Figures:\n")
for (f in list.files(paths$figures)) {
  cat("    -", f, "\n")
}
cat("  Tables:\n")
for (f in list.files(paths$tables)) {
  cat("    -", f, "\n")
}

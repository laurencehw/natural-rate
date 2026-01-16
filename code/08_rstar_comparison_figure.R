##------------------------------------------------------------------------------##
## File:        08_rstar_comparison_figure.R
## Project:     Natural Rate of Interest: Critique and Extensions
## Description: Create figure comparing baseline vs unconstrained r* time series
##------------------------------------------------------------------------------##

# Load setup
source("G:/My Drive/book drafts/natural_rate/code/00_setup.R")

# Load sensitivity results
load(file.path(paths$output_sens, "sensitivity_results.RData"))

# =============================================================================
# EXTRACT BASELINE AND UNCONSTRAINED R* SERIES
# =============================================================================

# Get baseline r* (b_y > 0.025)
baseline_rstar <- sensitivity_results$b_y[["b_y > 0.025 (baseline)"]]$rstar

# Get unconstrained r*
unconstrained_rstar <- sensitivity_results$b_y[["Unconstrained"]]$rstar

# Check lengths
cat("Baseline length:", length(baseline_rstar), "\n")
cat("Unconstrained length:", length(unconstrained_rstar), "\n")

# Create date sequence (quarterly from 1961Q1)
n_obs <- length(baseline_rstar)
dates <- seq(as.Date("1961-01-01"), by = "quarter", length.out = n_obs)

# Create data frame for plotting
plot_data <- data.frame(
  date = dates,
  Baseline = baseline_rstar,
  Unconstrained = unconstrained_rstar
)

# Reshape for ggplot
library(tidyr)
plot_long <- pivot_longer(plot_data, cols = c(Baseline, Unconstrained),
                           names_to = "Specification", values_to = "rstar")

# =============================================================================
# CREATE COMPARISON FIGURE
# =============================================================================

library(ggplot2)

# Define colors
spec_colors <- c("Baseline" = "#1f77b4", "Unconstrained" = "#d62728")

# Create the plot
p <- ggplot(plot_long, aes(x = date, y = rstar, color = Specification, linetype = Specification)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  scale_color_manual(values = spec_colors,
                     labels = c("Baseline (b_y > 0.025)", "Unconstrained Phillips Curve")) +
  scale_linetype_manual(values = c("Baseline" = "solid", "Unconstrained" = "dashed"),
                        labels = c("Baseline (b_y > 0.025)", "Unconstrained Phillips Curve")) +
  labs(
    x = NULL,
    y = expression(r^"*" ~ "(percent)"),
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey70"),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"),
    plot.margin = margin(10, 15, 10, 10)
  ) +
  # Add recession shading (approximate)
  annotate("rect", xmin = as.Date("2007-12-01"), xmax = as.Date("2009-06-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "gray50") +
  annotate("rect", xmin = as.Date("2020-02-01"), xmax = as.Date("2020-04-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "gray50") +
  annotate("rect", xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "gray50") +
  annotate("rect", xmin = as.Date("1990-07-01"), xmax = as.Date("1991-03-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "gray50") +
  annotate("rect", xmin = as.Date("1981-07-01"), xmax = as.Date("1982-11-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "gray50") +
  annotate("rect", xmin = as.Date("1980-01-01"), xmax = as.Date("1980-07-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "gray50") +
  annotate("rect", xmin = as.Date("1973-11-01"), xmax = as.Date("1975-03-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "gray50") +
  annotate("rect", xmin = as.Date("1969-12-01"), xmax = as.Date("1970-11-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "gray50")

# Save figure
ggsave(file.path(paths$figures, "fig4_rstar_baseline_vs_unconstrained.pdf"),
       plot = p, width = 6, height = 4, device = cairo_pdf)

ggsave(file.path(paths$figures, "fig4_rstar_baseline_vs_unconstrained.png"),
       plot = p, width = 6, height = 4, dpi = 300)

cat("\nFigure saved to:", file.path(paths$figures, "fig4_rstar_baseline_vs_unconstrained.pdf"), "\n")

# =============================================================================
# COMPUTE SUMMARY STATISTICS
# =============================================================================

# Post-2000 comparison
post_2000 <- plot_data$date >= as.Date("2000-01-01")
cat("\n--- Post-2000 Summary ---\n")
cat("Baseline mean:", round(mean(plot_data$Baseline[post_2000]), 2), "\n")
cat("Unconstrained mean:", round(mean(plot_data$Unconstrained[post_2000]), 2), "\n")
cat("Difference (baseline - unconstrained):", round(mean(plot_data$Baseline[post_2000] - plot_data$Unconstrained[post_2000]), 2), "\n")

# Post-GFC comparison
post_gfc <- plot_data$date >= as.Date("2010-01-01")
cat("\n--- Post-GFC Summary ---\n")
cat("Baseline mean:", round(mean(plot_data$Baseline[post_gfc]), 2), "\n")
cat("Unconstrained mean:", round(mean(plot_data$Unconstrained[post_gfc]), 2), "\n")
cat("Difference (baseline - unconstrained):", round(mean(plot_data$Baseline[post_gfc] - plot_data$Unconstrained[post_gfc]), 2), "\n")

# Final values
cat("\n--- Final Values (2025Q2) ---\n")
cat("Baseline:", round(tail(plot_data$Baseline, 1), 2), "\n")
cat("Unconstrained:", round(tail(plot_data$Unconstrained, 1), 2), "\n")
cat("Difference:", round(tail(plot_data$Baseline, 1) - tail(plot_data$Unconstrained, 1), 2), "\n")

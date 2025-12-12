# Rethinking the Natural Rate of Interest: A Critique of the Laubach-Williams Model

This repository contains replication code and analysis for a critique of the Laubach-Williams (LW) and Holston-Laubach-Williams (HLW) natural rate of interest (r*) estimation frameworks.

## Paper

The complete paper is available at [`paper/r_star_critique_complete.pdf`](paper/r_star_critique_complete.pdf).

### Key Findings

1. **IS Curve Specification**: Adding financial conditions (NFCI) to the IS curve improves model fit (R² from 0.929 to 0.935) and reduces the estimated interest rate coefficient by 38%, suggesting the standard model may overstate how precisely we can identify r*.

2. **Phillips Curve Constraint Sensitivity**: The unconstrained r* estimate (0.72%) differs substantially from the baseline with the b_y > 0.025 constraint (1.37%)---a 65 basis point difference.

3. **Demographics**: Old-age dependency ratios and working-age population growth explain substantial r* variation beyond trend growth, suggesting the standard r* = c*g + z decomposition conflates demographic effects with growth effects.

## Repository Structure

```
natural-rate/
├── code/                    # R analysis scripts
│   ├── 00_setup.R          # Package installation and configuration
│   ├── 01_download_data.R  # Data acquisition from FRED API
│   ├── 02_run_baseline.R   # Baseline LW model estimation
│   ├── 03_sensitivity.R    # Constraint sensitivity analysis
│   ├── 04_alt_financial.R  # Alternative: Financial conditions in IS curve
│   ├── 05_alt_tvp_phillips.R # Alternative: Time-varying Phillips curve
│   ├── 06_alt_demographics.R # Alternative: Demographics in r* equation
│   └── 07_figures_tables.R # Generate publication figures/tables
├── paper/                   # LaTeX source and compiled PDF
├── figures/                 # Generated figures
└── tables/                  # Generated tables (CSV format)
```

## Requirements

### R Packages
- Core: `tis`, `nloptr`, `mFilter`, `openxlsx`
- Data: `fredr`, `httr`, `jsonlite`
- Analysis: `dplyr`, `tidyr`, `lubridate`, `zoo`, `vars`, `dlm`, `KFAS`
- Visualization: `ggplot2`, `ggthemes`, `gridExtra`, `scales`
- Tables: `xtable`, `stargazer`

### External Dependencies

**FRBNY Replication Code**: Download the official LW and HLW replication code from the Federal Reserve Bank of New York:
- [LW Model](https://www.newyorkfed.org/research/policy/rstar)
- [HLW Model](https://www.newyorkfed.org/research/policy/rstar)

Place the extracted folders in `LW_model/` and `HLW_model/` directories.

**API Keys**: You will need a FRED API key (free):
1. Get a key at https://fred.stlouisfed.org/docs/api/api_key.html
2. Set it as an environment variable:
   ```r
   Sys.setenv(FRED_API_KEY = "your_key_here")
   ```
   Or add to a `.Renviron` file in the project directory.

## Running the Analysis

```r
# 1. Setup and install packages
source("code/00_setup.R")

# 2. Download data from FRED
source("code/01_download_data.R")

# 3. Run baseline LW model
source("code/02_run_baseline.R")

# 4. Sensitivity analysis
source("code/03_sensitivity.R")

# 5-6. Alternative specifications
source("code/04_alt_financial.R")
source("code/05_alt_tvp_phillips.R")
source("code/06_alt_demographics.R")

# 7. Generate figures and tables
source("code/07_figures_tables.R")
```

## Data Sources

- **GDP**: Real GDP (FRED: GDPC1)
- **Inflation**: PCE price index (FRED: PCEPI)
- **Interest Rates**: Federal funds rate (FRED: FEDFUNDS), 10-year Treasury (FRED: GS10)
- **Financial Conditions**: Chicago Fed NFCI (FRED: NFCI)
- **Inflation Expectations**: University of Michigan survey (FRED: MICH)
- **Demographics**: OECD old-age dependency ratios

## Citation

If you use this code, please cite:

```bibtex
@article{yourname2025rstar,
  title={Rethinking the Natural Rate of Interest: A Critique of the Laubach-Williams Model},
  author={[Author]},
  year={2025}
}
```

## References

- Laubach, T., & Williams, J. C. (2003). Measuring the natural rate of interest. *Review of Economics and Statistics*, 85(4), 1063-1070.
- Holston, K., Laubach, T., & Williams, J. C. (2017). Measuring the natural rate of interest: International trends and determinants. *Journal of International Economics*, 108, S59-S75.

## License

This project is for academic research purposes. The analysis code is provided as-is. The FRBNY replication code should be obtained from the official source and is subject to their terms.

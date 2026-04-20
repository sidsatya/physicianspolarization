library(dplyr)
library(readr)
library(fixest)
library(stringr)
library(glue)
library(tidyr)

county_file <- "output/county_year_stats.csv"

df <- read_csv(county_file, show_col_types = FALSE)

# Create county ID
df <- df %>%
  mutate(
    county_fips = sprintf("%02d%03d", STATEFIP, COUNTYFIP)
  )

reg_df <- df


# Summary stats
rhs_var_labels <- c(
  share_18_25    = "% 18-25",
  share_26_55    = "% 26-55",
  pct_lt_hs      = "% < HS",
  pct_hs         = "% HS",
  pct_black      = "% Black",
  pct_white      = "% White",
  pct_poverty    = "% Poverty",
  pct_in_healthcare_industry = "% Healthcare Industry"
)

lhs_var_labels <- c(
  uninsured_rate = "Uninsured Rate",
  restricted_exposure_rate = "Restricted Exposure Rate"
)
rhs_vars <- names(rhs_var_labels)
lhs_vars <- names(lhs_var_labels)
all_var_labels <- c(rhs_var_labels, lhs_var_labels)

# Create summary table of mean, sd, min, and max for each variable
summary_stats <- reg_df %>%
  select(all_of(c(rhs_vars, lhs_vars))) %>%
  summarise(across(
    everything(),
    list(
      mean = ~ mean(.x, na.rm = TRUE),
      sd   = ~ sd(.x, na.rm = TRUE),
      min  = ~ min(.x, na.rm = TRUE),
      max  = ~ max(.x, na.rm = TRUE),
      n    = ~ sum(!is.na(.x))
    )
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", "stat"),
    names_pattern = "^(.*)_(mean|sd|min|max|n)$"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(variable = all_var_labels[variable]) %>%
  select(variable, mean, sd, min, max, n)
print(summary_stats)

# Output to .tex file
summary_stats %>%
  mutate(
    mean = sprintf("%.3f", mean),
    sd = sprintf("%.3f", sd),
    min = sprintf("%.3f", min),
    max = sprintf("%.3f", max)
  ) %>%
  select(variable, mean, sd, min, max, n) %>%
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Summary Statistics for County-Level Variables",
    label = "tab:summary_stats",
    col.names = c("Variable", "Mean", "SD", "Min", "Max", "N")
  ) %>%
  writeLines("output/summary_stats.tex")


# Exposure 1 regressions
exposure_1_nofe <- feols(
  reformulate(rhs_vars, response = "uninsured_rate"),
  data = reg_df,
  cluster = ~ county_fips
)
exposure_1_fe <- feols(
  reformulate(rhs_vars, response = "uninsured_rate", intercept = TRUE),
  data = reg_df,
  cluster = ~ county_fips,
  fixef = c("county_fips", "YEAR")
)

# Exposure 2 regressions
exposure_2_nofe <- feols(
  reformulate(rhs_vars, response = "restricted_exposure_rate", intercept = TRUE),
  data = reg_df,
  cluster = ~ county_fips
)
exposure_2_fe <- feols(
  reformulate(rhs_vars, response = "restricted_exposure_rate", intercept = TRUE),
  data = reg_df,
  cluster = ~ county_fips,
  fixef = c("county_fips", "YEAR")
)

etable(
  exposure_1_nofe, exposure_1_fe,
  exposure_2_nofe, exposure_2_fe,
  tex = TRUE,
  file = "output/panel_exposure_cov_regressions.tex",
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  digits = 3,
  dict = c(
    share_18_25 = "\\% 18--25",
    share_26_55 = "\\% 26--55",
    pct_lt_hs = "\\% < HS",
    pct_hs = "\\% HS",
    pct_black = "\\% Black",
    pct_white = "\\% White",
    pct_poverty = "\\% Poverty",
    pct_in_healthcare_industry = "\\% Healthcare Industry",
    county_fips = "County",
    YEAR = "Year"
  ),
  headers = c("Exposure Measure 1", "With FE",
              "Exposure Measure 2", "With FE"),
  fitstat = ~ n + r2,
  notes = "Standard errors clustered at the county level. Exposure measure 1 is the county-level uninsured rate. Exposure measure 2 is the county-level restricted exposure rate, which is the share of adults who are uninsured, in poverty, and eligible for Medicaid."
)

#!/usr/bin/env Rscript
#
# make_sumstats.R
# Individual- and county-level summary statistics for the descriptive tables.
#
# Inputs:  analysis/input/physician_cycle.parquet
#          analysis/input/county_cycle.parquet
# Outputs: analysis/output/sumstats_individual.csv
#          analysis/output/sumstats_county.csv

suppressPackageStartupMessages({ library(arrow); library(data.table) })

args <- commandArgs(trailingOnly = FALSE)
fn   <- sub("--file=", "", args[grep("--file=", args)])
BASE <- if (length(fn))
  normalizePath(file.path(dirname(fn), "..", ".."), mustWork = TRUE) else getwd()

OUT <- file.path(BASE, "analysis/output")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

fmt <- function(x, d = 3) formatC(x, format = "f", digits = d, big.mark = ",")
sumrow <- function(name, x, d = 3) {
  x <- x[is.finite(x)]
  data.table(
    var    = name,
    mean   = fmt(mean(x),               d),
    sd     = fmt(sd(x),                 d),
    p10    = fmt(quantile(x, 0.10),     d),
    median = fmt(median(x),             d),
    p90    = fmt(quantile(x, 0.90),     d),
    N      = formatC(length(x), big.mark = ",")
  )
}

# ---- Individual-level (Table 1) -----------------------------------------
ind <- as.data.table(read_parquet(
  file.path(BASE, "analysis/input/physician_cycle.parquet")))

tab_ind <- rbindlist(list(
  sumrow("Partisan tilt",         ind$tilt,                 3),
  sumrow("Republican share",      ind$republican_share,     3),
  sumrow("Total donations (USD)", ind$total_amount,         0),
  sumrow("CF-weighted ideology",  ind$cf_ideology,          3),
  sumrow("Contributor CFscore",   ind$contributor_cfscore,  3)
))
fwrite(tab_ind, file.path(OUT, "sumstats_individual.csv"))
cat(sprintf("Individual-level: %s physicians, %s rows\n",
            format(uniqueN(ind$bonica_cid), big.mark = ","),
            format(nrow(ind),                big.mark = ",")))

# ---- County-level (Table 2) ---------------------------------------------
cty <- as.data.table(read_parquet(
  file.path(BASE, "analysis/input/county_cycle.parquet")))
cty <- cty[!is.na(pre_uninsured_rate)]

tab_cty <- rbindlist(list(
  sumrow("SD of partisan tilt",            cty$sd_tilt,            3),
  sumrow("SD of CF-weighted ideology",     cty$sd_cf_ideology,     3),
  sumrow("Two-pole polarization (CF)",     cty$polarization_cf,    3),
  sumrow("Two-pole polarization (tilt)",   cty$polarization_tilt,  3),
  sumrow("Pre-ACA uninsured rate ($E_c$)", cty$pre_uninsured_rate, 3),
  sumrow("Incumbent donors per cell",      cty$n_donors,           0)
))
fwrite(tab_cty, file.path(OUT, "sumstats_county.csv"))
cat(sprintf("County-level: %s counties, %s states, %s rows\n",
            format(uniqueN(cty$county_fips),  big.mark = ","),
            format(uniqueN(cty$state_fips),   big.mark = ","),
            format(nrow(cty),                  big.mark = ",")))

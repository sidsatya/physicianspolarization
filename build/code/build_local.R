#!/usr/bin/env Rscript
#
# build_local.R
#
# Consolidated LOCAL stage. Reads the Oscar intermediate (physician x
# recipient x cycle parquet) plus auxiliary inputs and produces the
# three analysis-ready datasets used by analysis/code/:
#
#   (i)   physician_cycle.parquet         physician x county x cycle, incumbents <= 2008
#                                          -> individual-level sumstats table
#   (ii)  county_cycle.parquet            county x cycle, incumbents <= 2008
#                                          -> DiD outcomes + county sumstats
#   (iii) cycle_longrun.parquet           cycle x cohort x measure, with mean + 95% CI
#                                          -> long-run time-series plots
#                                            cohorts: incumbents <= 2008 and <= 1996
#
# Inputs:
#   build/output/physician_recipient_cycle.parquet  (Oscar output)
#   build/input/zip_to_county_crosswalk.csv
#   build/input/county_year_sahie_exposure.csv
#
# Outputs (all parquet, written to build/output/):
#   physician_cycle.parquet
#   county_cycle.parquet
#   cycle_longrun.parquet
#
# Run:
#   Rscript build/code/build_local.R

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
})

# ---- paths --------------------------------------------------------------
# Repo root is two levels above this script; fall back to the working
# directory if Rscript was launched with `R --vanilla -e source(...)`.
BASE <- Sys.getenv("PHYSPOL_ROOT", unset = NA_character_)
if (is.na(BASE)) {
  args <- commandArgs(trailingOnly = FALSE)
  fn   <- sub("--file=", "", args[grep("--file=", args)])
  BASE <- if (length(fn)) normalizePath(file.path(dirname(fn), "..", ".."),
                                        mustWork = TRUE) else getwd()
}

IN_RECIPIENT <- file.path(BASE, "build/output/physician_recipient_cycle.parquet")
IN_CROSSWALK <- file.path(BASE, "build/input/zip_to_county_crosswalk.csv")
IN_SAHIE     <- file.path(BASE, "build/input/county_year_sahie_exposure.csv")

OUT_DIR <- file.path(BASE, "build/output")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

ts <- function(msg) message(sprintf("[%s] %s",
                                    format(Sys.time(), "%H:%M:%S"), msg))

# ---- knobs (kept here so the file is self-documenting) ------------------
INCUMBENT_MAIN <- 2008L     # main analysis: incumbent if active <= 2008
INCUMBENT_OLD  <- 1996L     # robustness cohort for long-run figures
KAPPA          <- 0.75      # extreme-tail threshold for L/R/Pi
MIN_DONORS     <- 5L        # minimum incumbent donors per county-cycle cell
SAHIE_YEARS    <- c(2008L, 2009L)

# =========================================================================
# 1. Donor-recipient-cycle -> donor-cycle aggregates
# =========================================================================
ts("Reading Oscar intermediate (physician x recipient x cycle)")
recip <- as.data.table(read_parquet(IN_RECIPIENT))
recip <- recip[, .(bonica_cid, cycle, amount, recipient_cfscore,
                   recipient_party, contributor_zipcode, contributor_state,
                   contributor_cfscore)]
recip <- recip[!is.na(bonica_cid) & !is.na(cycle) & !is.na(amount) & amount > 0]
ts(sprintf("  %s donor-recipient-cycle rows", format(nrow(recip), big.mark = ",")))

ts("Aggregating to donor x cycle")
recip[, party_cat := tolower(trimws(recipient_party))]
donor_cycle <- recip[, .(
  dem_amount         = sum(amount[party_cat == "democrat"],   na.rm = TRUE),
  rep_amount         = sum(amount[party_cat == "republican"], na.rm = TRUE),
  all_amount         = sum(amount,                            na.rm = TRUE),
  scored_amount      = sum(amount[!is.na(recipient_cfscore)], na.rm = TRUE),
  cf_weighted_amount = sum(amount * recipient_cfscore,        na.rm = TRUE),
  contributor_cfscore   = data.table::first(contributor_cfscore[!is.na(contributor_cfscore)]),
  contributor_zipcode   = data.table::first(contributor_zipcode),
  contributor_state     = data.table::first(contributor_state)
), by = .(bonica_cid, cycle)]

# Donor-level measures
donor_cycle[, two_party_total := dem_amount + rep_amount]
donor_cycle[, tilt := fifelse(two_party_total > 0,
                              (rep_amount - dem_amount) / two_party_total,
                              NA_real_)]
donor_cycle[, republican_share := fifelse(two_party_total > 0,
                                          rep_amount / two_party_total,
                                          NA_real_)]
donor_cycle[, cf_ideology := fifelse(scored_amount > 0,
                                     cf_weighted_amount / scored_amount,
                                     NA_real_)]

# =========================================================================
# 2. ZIP -> county allocation (proportional weights from HUD crosswalk)
# =========================================================================
ts("Loading ZIP -> county crosswalk and allocating donors to counties")
clean_zip5 <- function(zip, st = NULL) {
  raw <- gsub("[^0-9]", "", as.character(zip))
  raw[raw == ""] <- NA_character_
  if (!is.null(st)) {
    s <- toupper(trimws(as.character(st)))
    truncated <- !is.na(raw) & nchar(raw) == 8L &
                 s %chin% c("CT","MA","ME","NH","NJ","PR","RI","VT")
    raw[truncated] <- paste0("0", raw[truncated])
  }
  raw <- substr(raw, 1L, 5L)
  out <- formatC(suppressWarnings(as.integer(raw)),
                 width = 5L, flag = "0")
  out[is.na(out) | out == "   NA"] <- NA_character_
  out
}

xw <- fread(IN_CROSSWALK,
            select = c("zip5", "county_fips", "county_state",
                       "county_assignment_weight"),
            colClasses = list(character = c("zip5", "county_fips")))
xw[, zip5        := clean_zip5(zip5)]
xw[, county_fips := formatC(suppressWarnings(as.integer(county_fips)),
                            width = 5L, flag = "0")]
xw <- xw[!is.na(zip5) & !is.na(county_fips) & !is.na(county_assignment_weight)]

donor_cycle[, zip5 := clean_zip5(contributor_zipcode, contributor_state)]
phys_county <- donor_cycle[!is.na(zip5)][
  xw, on = "zip5", nomatch = NULL, allow.cartesian = TRUE]

# Allocate dollar amounts proportionally; donor-level shares/tilt/cf are
# constant across counties (they reflect overall behavior in the cycle).
for (col in c("dem_amount", "rep_amount", "all_amount",
              "scored_amount", "cf_weighted_amount", "two_party_total")) {
  phys_county[, (col) := get(col) * county_assignment_weight]
}

# Collapse to physician x county x cycle (sum across ZIPs mapping to same county)
phys_county <- phys_county[, .(
  dem_amount             = sum(dem_amount),
  rep_amount             = sum(rep_amount),
  all_amount             = sum(all_amount),
  scored_amount          = sum(scored_amount),
  cf_weighted_amount     = sum(cf_weighted_amount),
  two_party_total        = sum(two_party_total),
  contributor_cfscore    = data.table::first(contributor_cfscore),
  tilt                   = data.table::first(tilt),
  republican_share       = data.table::first(republican_share),
  cf_ideology            = data.table::first(cf_ideology)
), by = .(bonica_cid, county_fips, county_state, cycle)]

phys_county[, state_fips := as.integer(substr(county_fips, 1L, 2L))]
ts(sprintf("  %s physician x county x cycle rows | %s unique physicians",
           format(nrow(phys_county),                  big.mark = ","),
           format(uniqueN(phys_county$bonica_cid),    big.mark = ",")))

# =========================================================================
# 3. Output (i) -- individual-level (incumbents <= 2008)
# =========================================================================
ts("Building (i) individual-level dataset")
incumbents_main <- donor_cycle[cycle <= INCUMBENT_MAIN, unique(bonica_cid)]
indiv <- phys_county[bonica_cid %in% incumbents_main & !is.na(tilt),
  .(bonica_cid, county_fips, state_fips, cycle,
    tilt, republican_share, cf_ideology, contributor_cfscore,
    dem_amount, rep_amount, total_amount = two_party_total,
    scored_amount, cf_weighted_amount)]
write_parquet(indiv, file.path(OUT_DIR, "physician_cycle.parquet"))
ts(sprintf("  -> physician_cycle.parquet (%s rows, %s donors)",
           format(nrow(indiv),                big.mark = ","),
           format(uniqueN(indiv$bonica_cid),  big.mark = ",")))

# =========================================================================
# 4. Output (ii) -- county x cycle (incumbents <= 2008)
# =========================================================================
ts("Building (ii) county x cycle dataset")
indiv[, L_tilt := as.integer(tilt < -KAPPA)]
indiv[, R_tilt := as.integer(tilt >  KAPPA)]
indiv[, L_cf   := as.integer(cf_ideology < -KAPPA)]
indiv[, R_cf   := as.integer(cf_ideology >  KAPPA)]

county <- indiv[, .(
  n_donors           = .N,
  sd_tilt            = sd(tilt, na.rm = TRUE),
  share_L_tilt       = mean(L_tilt, na.rm = TRUE),
  share_R_tilt       = mean(R_tilt, na.rm = TRUE),
  sd_cf_ideology     = sd(cf_ideology, na.rm = TRUE),
  mean_cf_ideology   = mean(cf_ideology, na.rm = TRUE),
  share_L_cf         = mean(L_cf, na.rm = TRUE),
  share_R_cf         = mean(R_cf, na.rm = TRUE)
), by = .(county_fips, state_fips, cycle)]
county <- county[n_donors >= MIN_DONORS]
county[, polarization_tilt := 4 * share_L_tilt * share_R_tilt]
county[, polarization_cf   := 4 * share_L_cf   * share_R_cf]

# Pre-ACA exposure from SAHIE (mean of 2008-2009 ACA-eligible uninsured share)
ts("  Merging SAHIE pre-ACA exposure")
sahie <- fread(IN_SAHIE)
exposure <- sahie[year %in% SAHIE_YEARS,
  .(county_fips        = formatC(as.integer(county_fips),
                                 width = 5L, flag = "0"),
    pre_uninsured_rate = pct_uninsured_aca_eligible / 100)]
exposure <- exposure[, .(pre_uninsured_rate = mean(pre_uninsured_rate,
                                                   na.rm = TRUE)),
                    by = county_fips]
exposure <- exposure[!is.na(pre_uninsured_rate)]

county <- merge(county, exposure, by = "county_fips", all.x = TRUE)
setcolorder(county, c("county_fips", "state_fips", "cycle", "n_donors",
                      "pre_uninsured_rate",
                      "sd_tilt", "polarization_tilt",
                      "share_L_tilt", "share_R_tilt",
                      "sd_cf_ideology", "mean_cf_ideology",
                      "polarization_cf", "share_L_cf", "share_R_cf"))
write_parquet(county, file.path(OUT_DIR, "county_cycle.parquet"))
ts(sprintf("  -> county_cycle.parquet (%s rows, %s counties)",
           format(nrow(county),               big.mark = ","),
           format(uniqueN(county$county_fips), big.mark = ",")))

# =========================================================================
# 5. Output (iii) -- cycle x cohort long-run summary (CF-based)
# =========================================================================
# Two cohorts: incumbents <= 2008 and <= 1996.  For each, compute
# county-cycle L_cf, R_cf, Pi_cf, sd_cf_ideology (no MIN_DONORS filter --
# the long-run plots use the unrestricted county-cycle universe), then
# aggregate to cycle with 95% CIs from county-cycle variation.
ts("Building (iii) cycle x cohort long-run dataset")

build_cohort_summary <- function(cutoff, cohort_label) {
  ids <- donor_cycle[cycle <= cutoff, unique(bonica_cid)]
  sub <- phys_county[bonica_cid %in% ids & !is.na(cf_ideology)]
  sub[, L := as.integer(cf_ideology < -KAPPA)]
  sub[, R := as.integer(cf_ideology >  KAPPA)]
  cty <- sub[, .(
    L  = mean(L),
    R  = mean(R),
    sd_p = sd(cf_ideology),
    n  = .N
  ), by = .(county_fips, cycle)]
  cty[, Pi := 4 * L * R]
  m <- melt(cty[, .(county_fips, cycle, L, R, Pi, sd_p)],
            id.vars = c("county_fips", "cycle"),
            variable.name = "measure", value.name = "value",
            variable.factor = FALSE)
  out <- m[!is.na(value), .(
    mean = mean(value),
    sd   = sd(value),
    n    = .N
  ), by = .(cycle, measure)]
  out[, se    := sd / sqrt(n)]
  out[, ci_lo := mean - 1.96 * se]
  out[, ci_hi := mean + 1.96 * se]
  out[, cohort := cohort_label]
  out[, .(cycle, cohort, measure, mean, ci_lo, ci_hi)]
}

longrun <- rbindlist(list(
  build_cohort_summary(INCUMBENT_MAIN, sprintf("incumbents_<=_%d", INCUMBENT_MAIN)),
  build_cohort_summary(INCUMBENT_OLD,  sprintf("incumbents_<=_%d", INCUMBENT_OLD))
))
write_parquet(longrun, file.path(OUT_DIR, "cycle_longrun.parquet"))
ts(sprintf("  -> cycle_longrun.parquet (%s rows)",
           format(nrow(longrun), big.mark = ",")))

ts("Done.")

#!/usr/bin/env Rscript
#
# make_fig_did.R
# Event-study TWFE figure around T = 2010 for four outcomes, journal style.
#
# Input:   analysis/input/county_cycle.parquet
# Output:  analysis/output/fig_did.{pdf,png}

suppressPackageStartupMessages({
  library(arrow); library(data.table); library(fixest); library(ggplot2)
})

# ---- paths ---------------------------------------------------------------
args <- commandArgs(trailingOnly = FALSE)
fn   <- sub("--file=", "", args[grep("--file=", args)])
BASE <- if (length(fn))
  normalizePath(file.path(dirname(fn), "..", ".."), mustWork = TRUE) else getwd()

IN  <- file.path(BASE, "analysis/input/county_cycle.parquet")
OUT <- file.path(BASE, "analysis/output/fig_did")

# ---- knobs ---------------------------------------------------------------
T_U   <- 2010L
K_MIN <- -8L

OUTCOMES <- c(
  sd_tilt           = "SD of partisan tilt",
  sd_cf_ideology    = "SD of CF-weighted ideology",
  polarization_cf   = "Two-pole polarization (CF)",
  polarization_tilt = "Two-pole polarization (tilt)"
)

# ---- estimate ------------------------------------------------------------
panel <- as.data.table(read_parquet(IN))
panel[, k := as.integer(cycle - T_U)]
panel <- panel[k >= K_MIN & !is.na(pre_uninsured_rate)]

estimate <- function(outcome) {
  dat <- panel[!is.na(get(outcome))]
  m <- feols(
    as.formula(sprintf(
      "%s ~ i(k, pre_uninsured_rate, ref = -2L) | county_fips + cycle",
      outcome)),
    data = dat, cluster = ~state_fips, warn = FALSE, notes = FALSE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "est", "se", "tstat", "pval"))
  ct <- ct[grepl("::", term)]
  ct[, k_val := as.integer(sub(".*::(-?[0-9]+):.*", "\\1", term))]
  rbind(
    ct[, .(k_val, est, se,
           ci_lo = est - 1.96 * se, ci_hi = est + 1.96 * se)],
    data.table(k_val = -2L, est = 0, se = 0, ci_lo = 0, ci_hi = 0)
  )
}

coefs <- rbindlist(lapply(names(OUTCOMES), function(o) {
  cbind(estimate(o), outcome = o)
}))
coefs[, year        := k_val + T_U]
coefs[, outcome_lab := factor(OUTCOMES[outcome], levels = OUTCOMES)]
ref_pts <- coefs[k_val == -2L]

# ---- plot ----------------------------------------------------------------
es_theme <- theme_classic(base_size = 11) +
  theme(axis.line   = element_line(color = "black", linewidth = 0.4),
        axis.ticks  = element_line(color = "black", linewidth = 0.3),
        panel.grid  = element_blank(),
        strip.background = element_blank(),
        strip.text   = element_text(size = 11, face = "bold",
                                    margin = margin(b = 4)),
        plot.margin = margin(6, 10, 4, 6),
        axis.text.x = element_text(angle = 45, hjust = 1))

p <- ggplot(coefs, aes(x = year, y = est, group = 1)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  geom_vline(xintercept = T_U - 0.5, linetype = "dashed",
             color = "grey50", linewidth = 0.45) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              fill = "grey70", alpha = 0.45) +
  geom_line(color = "black", linewidth = 0.7) +
  geom_point(color = "black", fill = "white",
             shape = 21, size = 2.2, stroke = 0.8) +
  geom_point(data = ref_pts, color = "black", fill = "black",
             shape = 21, size = 2.2, stroke = 0.8) +
  scale_x_continuous(breaks = seq(min(coefs$year), max(coefs$year), 2L)) +
  facet_wrap(~ outcome_lab, nrow = 1, scales = "free_y") +
  labs(x = "Election cycle",
       y = expression(beta[k]~"  (per unit pre-ACA uninsured rate)")) +
  es_theme

ggsave(paste0(OUT, ".pdf"), p, width = 11, height = 3.6)
ggsave(paste0(OUT, ".png"), p, width = 11, height = 3.6, dpi = 300)
cat("Saved:", OUT, ".{pdf,png}\n", sep = "")

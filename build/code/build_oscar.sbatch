#!/bin/bash
#SBATCH --job-name=physpol_oscar
#SBATCH --output=logs/physpol_oscar_%j.out
#SBATCH --time=24:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=128G
#SBATCH --partition=batch
#
# Oscar stage of the physpol build pipeline.
# Reads the raw DIME mimp .rdata, identifies physician donors via an
# occupation-string regex, and writes a chunked physician x recipient x
# cycle parquet dataset that build_local.R consumes locally.
#
# Input:
#   build/input/dime_mimp_1979_2024.rdata
# Output:
#   build/output/physician_recipient_cycle.parquet
#       (single parquet; the local stage reads it with read_parquet)
#
# Usage:
#   sbatch build/code/build_oscar.sbatch
#
set -euo pipefail

ROOT_DIR="${ROOT_DIR:-$(cd "$(dirname "$0")/../.." && pwd)}"
cd "$ROOT_DIR"
mkdir -p logs

module load miniforge3/25.3.0-3-a6hh

MIMP="$ROOT_DIR/build/input/dime_mimp_1979_2024.rdata"
OUT="$ROOT_DIR/build/output/physician_recipient_cycle.parquet"
START_CYCLE=1980
END_CYCLE=2024
PHYS_BLOCK=25000

mkdir -p "$(dirname "$OUT")"
Rscript --vanilla - "$MIMP" "$OUT" "$START_CYCLE" "$END_CYCLE" "$PHYS_BLOCK" <<'EORS'
suppressPackageStartupMessages({
  library(Matrix); library(data.table); library(arrow)
})

args        <- commandArgs(trailingOnly = TRUE)
mimp_path   <- args[[1]]
out_path    <- args[[2]]
start_cycle <- as.integer(args[[3]])
end_cycle   <- as.integer(args[[4]])
phys_block  <- as.integer(args[[5]])

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
if (file.exists(out_path)) file.remove(out_path)

# Physician detector: contributor_type == "I" and occupation matches
# an MD keyword without matching negative terms (assistants, dentists, etc.).
PHYS_POS <- paste(c(
  "\\bphysician\\b", "medical doctor", "doctor of medicine", "\\bsurgeon\\b",
  "anesthesiolog", "psychiatrist", "pediatrician", "radiologist",
  "cardiolog", "dermatolog", "neurolog", "ophthalmolog", "urologist",
  "hospitalist", "neurosurgeon", "oncolog", "internist", "family physician",
  "family medicine", "emergency physician", "obstetrician", "gynecolog",
  "orthop(?:ae)?dic surgeon", "otolaryngolog", "gastroenterolog",
  "nephrolog", "pulmonolog", "endocrinolog", "pathologist"
), collapse = "|")
PHYS_NEG <- paste(c(
  "assistant", "optometr", "chiropract", "dentist", "physical therapist",
  "physicist", "speech patholog", "podiatr", "acupuncture", "veterinary",
  "nurse anesthet", "pathologist assistant", "physician assistant"
), collapse = "|")
is_physician <- function(ctype, occ) {
  occ <- tolower(trimws(as.character(occ))); occ[is.na(occ)] <- ""
  as.character(ctype) == "I" &
    grepl(PHYS_POS, occ, perl = TRUE) &
   !grepl(PHYS_NEG, occ, perl = TRUE)
}

classify_party <- function(p) {
  p <- tolower(trimws(as.character(p)))
  out <- rep(NA_character_, length(p))
  out[p %in% c("100", "democrat", "democratic", "dem", "d")] <- "democrat"
  out[p %in% c("200", "republican", "rep", "gop", "r")]     <- "republican"
  out
}

pick <- function(dt, candidates) {
  for (n in candidates) if (n %in% names(dt)) return(dt[[n]])
  NULL
}

cat(sprintf("[%s] Loading %s\n", format(Sys.time(), "%H:%M:%S"), mimp_path))
env  <- new.env(); load(mimp_path, envir = env)
mimp <- env[[ls(env)[[1]]]]
first_present <- function(lst, names) {
  for (n in names) if (!is.null(lst[[n]])) return(lst[[n]])
  NULL
}
mat         <- first_present(mimp, c("contrib.matrix.am", "contrib_matrix_am"))
cands_dt    <- as.data.table(first_present(mimp, c("cands", "candidates")))
contribs_dt <- as.data.table(first_present(mimp, c("contribs", "contributors")))
stopifnot(!is.null(mat), !is.null(cands_dt), !is.null(contribs_dt))
setnames(cands_dt,    names(cands_dt),    gsub("[^A-Za-z0-9]+", "_", names(cands_dt)))
setnames(contribs_dt, names(contribs_dt), gsub("[^A-Za-z0-9]+", "_", names(contribs_dt)))

# Candidate (column) metadata
cands_dt[, cycle             := suppressWarnings(as.integer(pick(.SD, c("cycle"))))]
cands_dt[, recipient_cfscore := suppressWarnings(as.numeric(pick(.SD, c("recipient_cfscore", "cfscore"))))]
cands_dt[, recipient_party   := classify_party(pick(.SD, c("party", "Party", "party_orig", "nimsp_party")))]
cands_dt[, matrix_col := .I]

# Contributor (row) metadata
contribs_dt[, bonica_cid          := pick(.SD, c("bonica_cid"))]
contribs_dt[, contributor_type    := pick(.SD, c("contributor_type"))]
contribs_dt[, contributor_zipcode := pick(.SD, c("most_recent_contributor_zipcode", "most_recent_zipcode", "zipcode"))]
contribs_dt[, contributor_state   := pick(.SD, c("most_recent_contributor_state",   "most_recent_state",   "state"))]
contribs_dt[, occupation          := pick(.SD, c("most_recent_contributor_occupation","most_recent_occupation","occupation"))]
contribs_dt[, contributor_cfscore := suppressWarnings(as.numeric(pick(.SD, c("contributor_cfscore"))))]
contribs_dt[, matrix_row := .I]
contribs_dt[, is_phys := is_physician(contributor_type, occupation)]

stopifnot(ncol(mat) == nrow(cands_dt), nrow(mat) == nrow(contribs_dt))

# Restrict to relevant cycles and physicians
cand_cols <- cands_dt[!is.na(cycle) & cycle >= start_cycle & cycle <= end_cycle, matrix_col]
phys_rows <- contribs_dt[is_phys == TRUE, matrix_row]
blocks    <- split(phys_rows, ceiling(seq_along(phys_rows) / phys_block))
cat(sprintf("[%s] Streaming %d blocks (%s physicians, %s candidate cycles)\n",
            format(Sys.time(), "%H:%M:%S"), length(blocks),
            format(length(phys_rows), big.mark=","),
            format(length(cand_cols), big.mark=",")))

# Chunked matrix extraction (for memory); rbind into one data.table.
parts <- vector("list", length(blocks))
for (k in seq_along(blocks)) {
  rs   <- blocks[[k]]
  sub  <- mat[rs, cand_cols, drop = FALSE]
  trip <- as.data.table(summary(sub))
  if (!nrow(trip)) next
  donor_i <- rs[trip$i]
  cand_j  <- cand_cols[trip$j]
  parts[[k]] <- data.table(
    bonica_cid          = contribs_dt$bonica_cid[donor_i],
    cycle               = cands_dt$cycle[cand_j],
    amount              = trip$x,
    recipient_cfscore   = cands_dt$recipient_cfscore[cand_j],
    recipient_party     = cands_dt$recipient_party[cand_j],
    contributor_zipcode = contribs_dt$contributor_zipcode[donor_i],
    contributor_state   = contribs_dt$contributor_state[donor_i],
    contributor_cfscore = contribs_dt$contributor_cfscore[donor_i]
  )
  cat(sprintf("[%s]   block %d/%d  rows=%s\n",
              format(Sys.time(), "%H:%M:%S"), k, length(blocks),
              format(nrow(parts[[k]]), big.mark=",")))
  rm(sub, trip); gc(verbose = FALSE)
}
out_dt <- rbindlist(parts, fill = TRUE)
write_parquet(out_dt, out_path)
cat(sprintf("[%s] Wrote %s rows -> %s\n",
            format(Sys.time(), "%H:%M:%S"),
            format(nrow(out_dt), big.mark=","), out_path))
EORS

echo "Oscar stage complete: $OUT"

library(ipumsr)
library(dplyr)
library(data.table)
library(readr)
library(dotenv)
library(stringr)
library(purrr)

root_dir <- "~/dev/physicianspolarization"
save_dir <- "/data/ipums"

download_dir <- file.path(root_dir, "data", "ipums")
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

# Load .env and set IPUMS API key up front, because we may need to query extract history
dotenv::load_dot_env(file = ".env")
ipums_api_key <- Sys.getenv("IPUMS_API_KEY")

if (is.null(ipums_api_key) || ipums_api_key == "") {
  stop("IPUMS_API_KEY not found in environment or .env file.")
}

set_ipums_api_key(ipums_api_key, overwrite = TRUE)

# Define vars and samples once, so both matching and submission use the same spec
vars_to_pull <- c(
  "YEAR",
  "PERWT",
  "INCWAGE",
  "OCCSOC",
  "IND1990",
  "COUNTYFIP",
  "STATEFIP",
  "CITYPOP",
  "SEX",
  "AGE",
  "POVERTY",
  "EDUC",
  "RACE",
  "CITIZEN",
  "HCOVANY",
  "HCOVPUB",
  "HCOVPRIV",
  "HINSCAID",
  "HINSCARE"
)

samples_to_pull <- c(
  "us2008a",
  "us2009a",
  "us2010a",
  "us2012a",
  "us2013a",
  "us2014a",
  "us2015a",
  "us2016a",
  "us2017a",
  "us2018a",
  "us2019a",
  "us2020a",
  "us2021a",
  "us2022a",
  "us2023a",
  "us2024a"
)

extract_description <- "ACS 2008-2024 extract via API"

# Helper: compare extract definitions to the desired request
extract_matches_spec <- function(extract, samples_to_pull, vars_to_pull, description = NULL) {
  extract_samples <- names(extract$samples)
  extract_vars <- names(extract$variables)

  samples_match <- setequal(extract_samples, samples_to_pull)

  # IPUMS may automatically add preselected variables after submission,
  # so require vars_to_pull to be included rather than exactly equal.
  vars_match <- all(vars_to_pull %in% extract_vars)

  desc_match <- TRUE
  if (!is.null(description)) {
    desc_match <- identical(extract$description, description)
  }

  samples_match && vars_match && desc_match
}

# Helper: find newest local DDI
find_local_ddi <- function(download_dir) {
  ddi_files <- list.files(
    download_dir,
    pattern = "^usa_\\d+\\.xml$",
    full.names = TRUE
  )

  if (length(ddi_files) == 0) return(NULL)

  ddi_files[order(file.info(ddi_files)$mtime, decreasing = TRUE)][1]
}

# 1. Prefer already-downloaded local extract
ddi_file_path <- find_local_ddi(download_dir)

if (!is.null(ddi_file_path) && file.exists(ddi_file_path)) {
  message("Found existing local IPUMS DDI file: ", ddi_file_path)

} else {
  message("No local DDI found. Checking recent IPUMS USA extracts...")

  recent_extracts <- get_extract_history("usa", how_many = 25)

  matching_extracts <- keep(
    recent_extracts,
    ~ extract_matches_spec(
      .x,
      samples_to_pull = samples_to_pull,
      vars_to_pull = vars_to_pull,
      description = extract_description
    )
  )

  ready_matching_extracts <- keep(matching_extracts, is_extract_ready)

  if (length(ready_matching_extracts) > 0) {
    # Use the most recent matching ready extract
    extract_to_download <- ready_matching_extracts[[1]]

    message(
      "Found matching ready IPUMS extract: usa:",
      extract_to_download$number,
      ". Downloading..."
    )

    downloaded_files <- download_extract(
      extract_to_download,
      download_dir = download_dir
    )

  } else {
    message("No matching ready extract found. Submitting a new extract request.")

    extract_def <- define_extract_micro(
      collection = "usa",
      description = extract_description,
      samples = samples_to_pull,
      variables = vars_to_pull,
      data_format = "csv",
      data_structure = "rectangular",
      case_select_who = "individuals",
      data_quality_flags = TRUE
    )

    submitted <- submit_extract(extract_def)

    message("Waiting for extract to complete...")
    completed_extract <- wait_for_extract(submitted)

    message("Downloading new extract files...")
    downloaded_files <- download_extract(
      completed_extract,
      download_dir = download_dir
    )
  }

  downloaded_ddi_file <- downloaded_files[grepl("\\.xml$", downloaded_files)]

  if (length(downloaded_ddi_file) >= 1) {
    ddi_file_path <- downloaded_ddi_file[1]
    message("Using IPUMS DDI file: ", ddi_file_path)
  } else {
    stop("Could not find downloaded DDI file (.xml).")
  }
}

if (is.null(ddi_file_path) || !file.exists(ddi_file_path)) {
  stop("Failed to find or download a valid IPUMS DDI file.")
}

message("Reading IPUMS microdata using the DDI object...")
ipums_data <- read_ipums_micro(ddi_file_path)

if (!exists("ipums_data", inherits = FALSE)) {
  stop("Failed to load IPUMS data.")
} else {
  message("IPUMS data loaded successfully.")
}

healthcare_industries_1990 <- c(812, 820, 821, 822, 830, 831, 832, 840)

filtered_data <- ipums_data %>%
  mutate(in_healthcare_industry = IND1990 %in% healthcare_industries_1990)

cat("Successfully filtered data for healthcare industries only\n")

min_year <- min(filtered_data$YEAR, na.rm = TRUE)
max_year <- max(filtered_data$YEAR, na.rm = TRUE)
cat("The data contains records from", min_year, "to", max_year, "\n")

# Assume your loaded IPUMS microdata is called ipums_data
# If not, replace ipums_data below with your actual object name.

df <- filtered_data %>%
  mutate(
    # unique county id
    county_id = sprintf("%02d%03d", STATEFIP, COUNTYFIP),

    # insurance
    insured = HCOVANY == 2,
    uninsured = HCOVANY == 1,
    restricted_exposure = uninsured & AGE >= 19 & AGE <= 64 & POVERTY == 1 & HINSCARE == 1 & HINSCAID == 1,

    # adult flags
    adult = AGE >= 18,
    age_18_25 = AGE >= 18 & AGE <= 25,
    age_26_55 = AGE >= 26 & AGE <= 55,
    age_56_plus = AGE >= 56,

    # education buckets (general EDUC coding)
    # IPUMS EDUC is harmonized educational attainment.
    # These cutoffs are the standard broad buckets:
    # <HS: less than high school
    # HS: high school graduate / GED / some college / associate
    # College: bachelor's or more
    educ_lt_hs = EDUC < 3,
    educ_hs = EDUC >= 3 & EDUC <= 6,
    educ_college = EDUC >= 7 & EDUC != 99,

    impoverished = POVERTY == 1,

    # race / ethnicity
    white = RACE == 1,
    black_nh = RACE == 2,

    # optional Medicaid/public flags
    medicaid = HINSCAID == 2,
    public_cov = HCOVPUB == 2,
    private_cov = HCOVPRIV == 2,

    # identifiable county flag
    county_identified = !is.na(COUNTYFIP) & COUNTYFIP != 0
  ) %>%
  filter(adult, county_identified)

county_year_stats <- df %>%
  group_by(YEAR, STATEFIP, COUNTYFIP, county_id) %>%
  summarise(
    adult_pop = sum(PERWT, na.rm = TRUE),

    uninsured_rate = sum(PERWT * uninsured, na.rm = TRUE) / adult_pop,
    restricted_exposure_rate = sum(PERWT * restricted_exposure, na.rm = TRUE) / adult_pop,

    share_18_25 = sum(PERWT * age_18_25, na.rm = TRUE) / adult_pop,
    share_26_55 = sum(PERWT * age_26_55, na.rm = TRUE) / adult_pop,
    share_56_plus = sum(PERWT * age_56_plus, na.rm = TRUE) / adult_pop,

    pct_lt_hs = sum(PERWT * educ_lt_hs, na.rm = TRUE) / adult_pop,
    pct_hs = sum(PERWT * educ_hs, na.rm = TRUE) / adult_pop,
    pct_college = sum(PERWT * educ_college, na.rm = TRUE) / adult_pop,

    pct_black = sum(PERWT * black_nh, na.rm = TRUE) / adult_pop,
    pct_white = sum(PERWT * white, na.rm = TRUE) / adult_pop,

    pct_medicaid = sum(PERWT * medicaid, na.rm = TRUE) / adult_pop,
    pct_public_cov = sum(PERWT * public_cov, na.rm = TRUE) / adult_pop,
    pct_private_cov = sum(PERWT * private_cov, na.rm = TRUE) / adult_pop,

    pct_poverty = sum(PERWT * impoverished, na.rm = TRUE) / adult_pop,
    pct_in_healthcare_industry = sum(PERWT * in_healthcare_industry, na.rm = TRUE) / adult_pop,

    .groups = "drop"
  )

write_csv(county_year_stats, "output/county_year_stats.csv")


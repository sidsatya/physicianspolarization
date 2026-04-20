library(ipumsr)
library(dplyr)
library(data.table)
library(readr)
library(dotenv)
library(stringr)

root_dir <- "~/dev/physicianspolarization"
save_dir <- "/data/ipums"

# Define the download directory
download_dir <- file.path(root_dir, "data", "ipums")

# Ensure directory exists
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE) 

# Check for existing IPUMS data files (usa_XXXXX.xml and usa_XXXXX.csv.gz)
existing_ddi_files <- list.files(download_dir, pattern = "^usa_\\d+\\.xml$", full.names = TRUE)
existing_data_files <- list.files(download_dir, pattern = "^usa_\\d+\\.csv\\.gz$", full.names = TRUE)
# Initialize ddi_file_path
ddi_file_path <- NULL

# Check for existing IPUMS DDI file (usa_XXXXX.xml)
existing_ddi_files <- list.files(download_dir, pattern = "^usa_\\d+\\.xml$", full.names = TRUE)

if (length(existing_ddi_files) > 0) {
  # Use the first existing DDI file found (consider sorting by name/date if multiple exist)
  ddi_file_path <- existing_ddi_files[1]
  message("Found existing IPUMS DDI file: ", ddi_file_path)
} else {
  # No existing DDI found, proceed with API download
  message("No existing IPUMS DDI file found. Submitting extract request.")

  # Load .env file and access your IPUMS API key
  dotenv::load_dot_env(file = ".env")
  ipums_api_key <- Sys.getenv("IPUMS_API_KEY")
  if (is.null(ipums_api_key) || ipums_api_key == "") {
      stop("IPUMS_API_KEY not found in environment or .env file.")
  }
  set_ipums_api_key(ipums_api_key, overwrite = TRUE)

  # Define vars to pull
  vars_to_pull <- c("YEAR",
                    "PERWT",
                    "INCWAGE",
                    "OCCSOC",
                    "IND1990",
                    "COUNTYFIP",   # County FIPS code
                    "STATEFIP",    # State FIPS code
                    "CITYPOP",       
                    "SEX",
                    "AGE",
                    "POVERTY",     # Poverty status
                    "EDUC",       # Educational attainment
                    "RACE",
                    "CITIZEN",
                    "HCOVANY",     # Any health insurance coverage
                    "HCOVPUB",     # Public health insurance coverage
                    "HCOVPRIV",     # Private health insurance coverage
                    "HINSCAID",     # Medicaid coverage
                    "HINSCARE"     # Medicare coverage
                )

  # Define the extract
  extract_def <- define_extract_micro(
    collection = "usa",
    description = "ACS 2004-2014 extract via API",
    samples = c(
      "us2008a",
      "us2009a","us2010a", "us2012a", "us2013a", "us2014a"
    ),
    variables = vars_to_pull,
    data_format= "csv",
    data_structure="rectangular",
    case_select_who="individuals",
    data_quality_flags=TRUE
  )

  # Submit, wait, download
  submitted <- submit_extract(extract_def)
  message("Waiting for extract to complete...")
  downloaded <- wait_for_extract(submitted)
  message("Downloading extract files...")
  # download_extract returns paths to both .xml and .csv.gz
  downloaded_files <- download_extract(downloaded, download_dir = download_dir)

  # Find the downloaded DDI (.xml) file path
  downloaded_ddi_file <- downloaded_files[grepl("\\.xml$", downloaded_files)]

  if (length(downloaded_ddi_file) == 1) {
      ddi_file_path <- downloaded_ddi_file
      message("Downloaded new IPUMS DDI file: ", ddi_file_path)
  } else {
      stop("Could not find the downloaded DDI file (.xml). Download might have failed or returned unexpected results.")
  }
}

# Ensure a DDI file path was determined
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

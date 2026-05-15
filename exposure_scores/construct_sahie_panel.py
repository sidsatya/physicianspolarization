import os
import pandas as pd
import numpy as np

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

YEARS = range(2008, 2024)

proj_root = "/Users/sidsatya/dev/physicianspolarization/"
sahie_data_dir = os.path.join(proj_root, "data", "county_insurance_rates")
output_dir = os.path.join(proj_root, "output")
os.makedirs(output_dir, exist_ok=True)

# Header rows differ slightly across files
SKIPROWS = {
    **{y: 79 for y in range(2008, 2021)},
    2021: 83,
    2022: 84,
    2023: 84,
}

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

def read_sahie_year(year: int) -> pd.DataFrame:
    path = os.path.join(sahie_data_dir, f"sahie_{year}.csv")
    df = pd.read_csv(path, skiprows=SKIPROWS[year], low_memory=False)
    df.columns = df.columns.str.strip().str.lower()
    df["year"] = year
    return df


def safe_pct(num: pd.Series, denom: pd.Series) -> pd.Series:
    return np.where(denom.notna() & (denom != 0), 100 * num / denom, np.nan)

# ------------------------------------------------------------
# Read and combine SAHIE files
# ------------------------------------------------------------

sahie = pd.concat([read_sahie_year(y) for y in YEARS], ignore_index=True)

num_cols = [
    "year", "statefips", "countyfips", "geocat",
    "agecat", "racecat", "sexcat", "iprcat",
    "nipr", "nui", "nic", "pctui"
]

for col in num_cols:
    if col in sahie.columns:
        sahie[col] = pd.to_numeric(sahie[col], errors="coerce")

# Keep the updated 2013 file when both original and updated are present
if "version" in sahie.columns:
    sahie["version"] = sahie["version"].astype(str).str.strip()
    sahie["version_rank"] = np.select(
        [sahie["version"].str.lower().eq("updated"),
         sahie["version"].str.lower().eq("original")],
        [2, 1],
        default=2
    )

    primary_key_no_version = [
        "year", "statefips", "countyfips", "geocat",
        "agecat", "racecat", "sexcat", "iprcat"
    ]

    sahie = (
        sahie
        .sort_values(primary_key_no_version + ["version_rank"])
        .drop_duplicates(primary_key_no_version, keep="last")
        .drop(columns="version_rank")
    )

# County-level observations only
county = sahie[sahie["geocat"] == 50].copy()

county["statefips"] = county["statefips"].astype("Int64")
county["countyfips"] = county["countyfips"].astype("Int64")
county["county_fips"] = (
    county["statefips"].astype(str).str.zfill(2) +
    county["countyfips"].astype(str).str.zfill(3)
)

id_cols = ["year", "statefips", "countyfips", "county_fips"]

# ------------------------------------------------------------
# 1. Adult baseline uninsured share
# Numerator: uninsured age 18-64, all income levels
# Denominator: total age 18-64 population
# ------------------------------------------------------------

adult_base = county[
    (county["agecat"] == 1) &
    (county["racecat"] == 0) &
    (county["sexcat"] == 0) &
    (county["iprcat"] == 0)
][id_cols + ["nipr", "nui"]].copy()

adult_base = adult_base.rename(columns={
    "nipr": "pop_18_64",
    "nui": "uninsured_18_64_all_income"
})

adult_base["pct_uninsured_baseline"] = safe_pct(
    adult_base["uninsured_18_64_all_income"],
    adult_base["pop_18_64"]
)

# ------------------------------------------------------------
# 2. ACA-targeted uninsured share
# Numerator: uninsured age 18-64, <=138% FPL
# Denominator: total age 18-64 population
# ------------------------------------------------------------

aca_targeted = county[
    (county["agecat"] == 1) &
    (county["racecat"] == 0) &
    (county["sexcat"] == 0) &
    (county["iprcat"] == 3)
][id_cols + ["nipr", "nui"]].copy()

aca_targeted = aca_targeted.rename(columns={
    "nipr": "pop_18_64_below_138fpl",
    "nui": "uninsured_18_64_below_138fpl"
})

baseline = adult_base.merge(aca_targeted, on=id_cols, how="left")

baseline["pct_uninsured_aca_eligible"] = safe_pct(
    baseline["uninsured_18_64_below_138fpl"],
    baseline["pop_18_64"]
)

baseline = baseline[
    id_cols + [
        "pop_18_64",
        "uninsured_18_64_all_income",
        "pop_18_64_below_138fpl",
        "uninsured_18_64_below_138fpl",
        "pct_uninsured_baseline",
        "pct_uninsured_aca_eligible"
    ]
].copy()

# ------------------------------------------------------------
# 3. Sex composition
# Shares within age 18-64, all races, all income levels
# ------------------------------------------------------------

sex_base = county[
    (county["agecat"] == 1) &
    (county["racecat"] == 0) &
    (county["iprcat"] == 0)
][id_cols + ["sexcat", "nipr"]].copy()

sex_pivot = (
    sex_base
    .pivot_table(index=id_cols, columns="sexcat", values="nipr", aggfunc="first")
    .reset_index()
    .rename(columns={
        0: "pop_18_64_both_sexes",
        1: "pop_18_64_male",
        2: "pop_18_64_female"
    })
)

sex_pivot["pct_male"] = safe_pct(
    sex_pivot["pop_18_64_male"],
    sex_pivot["pop_18_64_both_sexes"]
)
sex_pivot["pct_female"] = safe_pct(
    sex_pivot["pop_18_64_female"],
    sex_pivot["pop_18_64_both_sexes"]
)

sex_pivot = sex_pivot[id_cols + ["pct_male", "pct_female"]]

# ------------------------------------------------------------
# 4. Income group shares
# Shares within age 18-64, all races, both sexes
# ------------------------------------------------------------

income_base = county[
    (county["agecat"] == 1) &
    (county["racecat"] == 0) &
    (county["sexcat"] == 0)
][id_cols + ["iprcat", "nipr"]].copy()

income_pivot = (
    income_base
    .pivot_table(index=id_cols, columns="iprcat", values="nipr", aggfunc="first")
    .reset_index()
    .rename(columns={
        0: "pop_18_64_all_income",
        1: "pop_18_64_below_200fpl",
        2: "pop_18_64_below_250fpl",
        3: "pop_18_64_below_138fpl",
        4: "pop_18_64_below_400fpl",
        5: "pop_18_64_138_to_400fpl"
    })
)

income_share_specs = {
    "pct_income_below_138fpl": "pop_18_64_below_138fpl",
    "pct_income_below_200fpl": "pop_18_64_below_200fpl",
    "pct_income_below_250fpl": "pop_18_64_below_250fpl",
    "pct_income_below_400fpl": "pop_18_64_below_400fpl",
    "pct_income_138_to_400fpl": "pop_18_64_138_to_400fpl",
}

for out_col, num_col in income_share_specs.items():
    if num_col in income_pivot.columns and "pop_18_64_all_income" in income_pivot.columns:
        income_pivot[out_col] = safe_pct(
            income_pivot[num_col],
            income_pivot["pop_18_64_all_income"]
        )
    else:
        income_pivot[out_col] = np.nan

income_pivot = income_pivot[id_cols + list(income_share_specs.keys())]

# ------------------------------------------------------------
# Final county-year dataset
# ------------------------------------------------------------

county_year_sahie = (
    baseline
    .merge(sex_pivot, on=id_cols, how="left")
    .merge(income_pivot, on=id_cols, how="left")
    .sort_values(["county_fips", "year"])
    .reset_index(drop=True)
)

out_path = os.path.join(output_dir, "county_year_sahie_exposure.csv")
county_year_sahie.to_csv(out_path, index=False)

print(f"Saved: {out_path}")
print(f"Rows: {len(county_year_sahie):,}")
print(f"Counties: {county_year_sahie['county_fips'].nunique():,}")

for y in YEARS:
    n_counties = county_year_sahie.loc[
        county_year_sahie["year"] == y, "county_fips"
    ].nunique()
    print(f"Year {y}: {n_counties} unique counties")

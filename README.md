
# RCodingSupport

**RCodingSupport** is a teaching package for the R Coding Support Sessions. It provides synthetic datasets students use to practise R, from basic manipulation to more advanced workflows.

Every dataset describes **one fictional scenario**: endemic, seasonal **malaria in
the fictional Republic of Amani**, under surveillance across 12 districts over
five years. All data are **fictional and simulated** purely for teaching; all
place names are invented.

---

## Installation

```r
# install.packages("remotes")
remotes::install_github("mrc-ide/RCodingSupport")
library(RCodingSupport)
```

Teaching materials pin a tagged version per year, e.g.
`remotes::install_github("mrc-ide/RCodingSupport@teaching-2026")`.

## The single scenario

Almost every object is derived from two **master** datasets:

- **case_linelist** — individual-level line-list of reported malaria cases (the master).
- **environment_weekly** — weekly rainfall and temperature per district (drives transmission).

Supporting masters: **testing_weekly**, **districts**, **facilities**.

## Available data objects

- **case_linelist** — malaria case line-list (~114k cases, 20 variables).
- **environment_weekly** — weekly rainfall + temperature per district (rainfall drives transmission with a six-week lag).
- **testing_weekly** — weekly tests and confirmed cases per district.
- **districts** / **facilities** — geography, population, bednet coverage, health facilities.
- **incidence_weekly_age** — weekly case counts by 5-year age band (matrix).
- **incidence_weekly** — total weekly case counts (vector).
- **patient_records** — 150-patient demographic sample with messy names (data frame).
- **district_weekly_list** — weekly surveillance for the two focus cities (list).
- **allele_freq_matrix** — parasite SNP allele frequencies, with artefacts and missingness (matrix).
- **resistance_trajectories** — drug-resistance marker prevalence + modelled bands (list).
- **analysis_bundle** — bednet coverage vs malaria incidence: data, summary stats, fitted `lm` (list).
- **posterior_density** — 2D posterior over transmission-model parameters (list).
- **chw_trial** — community health worker intervention trial summary (data frame).

## Data-import practice

`export_examples()` writes file copies of these datasets (in various formats, with
deliberate "messiness") so students can practise reading data from disk — the file
they import is the same data they can also load directly from the package:

```r
dir.create("data_raw")
RCodingSupport::export_examples("data_raw")
```

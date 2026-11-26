# Practice-Level Digital Exclusion Risk (DERA) and Digital Primary Care Delivery in England

Official code and data repository for the study investigating the relationship between practice-level Digital Exclusion Risk Assessment (DERA) scores, digital primary care consultation activity, and patient-reported care experiences across English GP practices (2025).

**DOI:** 
---

## Overview

This repository contains the practice-level dataset construction script, statistical modelling scripts, and the derived practice-level analytical dataset (`gp_level.csv`) used to reproduce the analyses in *"Practice-Level Digital Exclusion Risk (DERA) and Digital Primary Care Delivery in England"*.

Specifically, the codebase covers:
1. **Practice-level data construction (`Make GP dataset.R`):** Unifying and preparing publicly available source datasets into the practice-level analytical file (`gp_level.csv`).
2. **Synchronous virtual consultation models (`Remote appt models.R` and `Sensitivity - remote appt models.R`):** Modelling the proportion of synchronous appointments occurring virtually using binomial mixed-effects regression (with total synchronous appointments as the denominator), alongside sensitivity analyses.
3. **Asynchronous online consultation models (`OC rate models.R`):** Modelling asynchronous online consultation activity using negative binomial regression (`nbinom2`) to account for overdispersion, with an offset for practice list size.
4. **GP Patient Survey (GPPS) models (`GPPS analyses.R`):** Modelling 18 GPPS items using beta regression (`beta_family`), weighted by the number of survey responses and adjusted for practice synchronous virtual appointment percentage and log-transformed list size.

---

## Data & Repository Contents

Two datasets underpin the analyses:
* **`gp_level.csv` (included in repository):** Practice-level dataset constructed from publicly available data sources via `Make GP dataset.R`.
* **GP Patient Survey (GPPS) data (not re-uploaded):** Sourced directly from the GP Patient Survey rather than redistributed in this repository. Researchers wishing to run `GPPS analyses.R` should obtain the GPPS dataset directly from its official source and place/reference it locally according to the path or object expected in `GPPS analyses.R`.

### Files in This Repository
* **`gp_level.csv`**: Practice-level dataset derived from publicly available sources for modelling across English GP practices.
* **`Make GP dataset.R`**: Data preparation and linkage script showing how public source datasets were unified into `gp_level.csv`.
* **`Remote appt models.R`**: Primary binomial mixed-effects models for synchronous virtual consultation proportions.
* **`Sensitivity - remote appt models.R`**: Sensitivity analyses for the synchronous virtual consultation models.
* **`OC rate models.R`**: Negative binomial mixed-effects models (`nbinom2`) for asynchronous online consultation rates with a practice list size offset.
* **`GPPS analyses.R`**: Response-weighted beta mixed-effects models (`beta_family`) across the 18 GPPS items (requires local GPPS data obtained directly from the GPPS source).

---

## Statistical & Modelling Summary

All regression models are fitted in `glmmTMB` and adjust for practice-level covariates:
* **DERA score** (focal predictor)
* **Urban score**
* **Female percentage**
* **White percentage**
* **FTE GPs**

**GPPS models** additionally adjust for:
* **Practice synchronous virtual appointment percentage**
* **Log-transformed practice list size**

**Clustering & Interpretability:**
* Final models account for clustering by **Integrated Care Board (ICB)** using random slopes allowing the effect of DERA to vary by ICB (selected over random-intercept-only models based on AIC, BIC, and log-likelihood; see Supplementary Table S2).
* Model estimates include coefficients and 95% confidence intervals (CIs), predicted values across focal predictor values holding other covariates at their means (synchronous and asynchronous consultation outcomes), and average marginal effects on the outcome scale for a 10-unit increase in the focal predictor (GPPS outcomes) via `marginaleffects`.

---

## Software & Package Versions

* **R:** `4.6.1`
* **Key R packages:** `tidyverse` (`2.0.0`), `glmmTMB` (`1.1.14`), `marginaleffects` (`0.32.0`)

```r
install.packages(
  c("tidyverse", "glmmTMB", "marginaleffects"),
  repos = "[https://cloud.r-project.org](https://cloud.r-project.org)"
)

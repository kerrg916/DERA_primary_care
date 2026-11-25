# Practice-Level Digital Exclusion Risk (DERA) and Digital Primary Care Delivery in England

Official code and data repository for the study investigating the relationship between practice-level Digital Exclusion Risk Assessment (DERA) scores, digital primary care consultation activity, and patient-reported care experiences across English GP practices (2025).

**<doi here>**

## Overview
This repository contains the practice-level dataset construction script and statistical modelling code (used to reproduce all analyses in the manuscript. 

Specifically, the codebase reproduces:
1. **Practice-level data linkage & filtering:** Compiling of publicly available data sources into the analytical dataset.
2. **Primary regression models:** Integrated Care Board (ICB) random-slope mixed-effects models (`binomial`, `nbinom2`, and `beta_family`) evaluating overall DERA scores against synchronous virtual consultations, asynchronous online consultations, and 18 GP Patient Survey (GPPS) items.

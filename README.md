# DS4SI-GroupD-Final-Project

## Reproducibility & Modeling Code

**Course:** APSTA-GE 2331 — Data Science for Social Impact  
**Semester:** Fall 2025  
**Authors:** Megha Chouthai, Sihua Kang, David Krane, Sophia Sohail  

---

## Overview

This repository contains code supporting the **Reproducibility** and **Modeling** sections of our final project for *Data Science for Social Impact*.  
We reproduce and extend analyses from:

> Oestman, J. & Wilson, R. K. (2023). *The Effect of Biased Peacekeepers on Building Trust*.

The goal is to (1) replicate key results from the original paper using the authors’ materials and (2) explore additional modeling choices to better understand how peacekeeper bias affects trust and cooperation.

---

## Repository Structure

|----- reproducibility_code.R (Reproducible code of the original analyses)<br>
|----- modeling_code.R (Extended modeling & additional analyses)<br>
|----- data.csv (Input dataset(s) from authors)<br>
|----- original_research.pdf (copy of the original research paper) 
|----- README.md <br>

## Files

### `reproducibility.R`
**Purpose:** Reproducibility  
- Reproduces key tables and figures from the original paper  
- Uses the same statistical software (R) and structure as the authors  
- Requires minimal setup beyond setting file paths  
- Demonstrates transparency and ease of reproduction, while noting missing data dictionaries  

This script supports **Section 5 (Reproducibility)** of the written report.

---

### `modeling.R`
**Purpose:** Modeling & Extensions  
- Explores alternative specifications not emphasized in the paper  
- Introduces a continuous “monitor bias” coefficient  
- Examines heterogeneity, zero-return behavior, correlations, and belief accuracy  
- Visualizes results to highlight individual-level risk not captured by averages  

This script supports **Section 6 (Modeling)** of the written report.

---

## Requirements

- R (≥ 4.0)
- Packages used:
  - `dplyr`, `ggplot2`, `psych`, `VGAM`, `AER`, `plm`, `censReg`, `stargazer`, `corrplot`, `heatmaply`

Install missing packages with:
```r
install.packages(c("dplyr", "ggplot2", "psych", "VGAM", "AER", "plm",
                   "censReg", "stargazer", "corrplot", "heatmaply"))

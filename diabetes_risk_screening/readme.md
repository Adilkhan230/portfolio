# Type 2 Diabetes Risk Screening Tool

## Overview
A pre-diagnostic **screening tool** for **Type 2 Diabetes** using **machine learning** and self-reported survey data.  
Built on NHANES 2013–2016 data, it predicts individual risk before clinical diagnosis, supporting early prevention. This was a group project for BioInformatics Hackaton 2025, Almaty, Kazakhstan.

## Goal
- Identify key risk factors through questionnaire-based assessment.
- Classify individuals into:
  - **Green** – Low risk
  - **Yellow** – Moderate risk
  - **Red** – High risk
- Provide tailored prevention recommendations.

## Data & Methodology
- **Source:** National Health and Nutrition Examination Survey (NHANES) 2013–2016.
- **Features:** Demographics, lifestyle, health history (no clinical measurements).
- **Processing:** Data cleaning, encoding, missing value handling.
- **Models:** XGBoost, Short XGBoost, LightGBM, Random Forest, SVM.
- **Ensemble:** Mean probability from all models.

## Risk Zones
| Zone   | Criteria                      | Recommendation                                  |
|--------|--------------------------------|--------------------------------------------------|
| Green  | < 75th percentile              | Maintain current lifestyle                      |
| Yellow | 75th–89th percentile           | Monitor & consider medical advice               |
| Red    | ≥ 90th percentile              | Seek immediate medical attention & lab testing  |

## Application
- **app.R** – R Shiny app with interactive survey and risk scoring.
- Not hosted online due to Shiny free tier RAM limitations.



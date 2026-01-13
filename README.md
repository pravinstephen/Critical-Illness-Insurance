# Critical Illness Insurance: Predictive Risk & Underwriting Analysis

## Project Overview
This project performs a statistical analysis of a health insurance portfolio to determine if higher premiums for policyholders with chronic conditions (diabetes, hypertension, etc.) are justified by actual claim frequency and severity[cite: 77, 82]. 

Using anonymized data from ~5,000 policyholders, the analysis evaluates whether advances in medicine have narrowed the risk gap traditionally assumed in actuarial pricing models[cite: 80, 82].

## Key Technical Features
* **Language:** R
* **Libraries:** `tidyverse`, `readr`
* **Methodologies:**
    * **Distribution Modeling:** Fitted Poisson distributions to claim frequency for both chronic and non-chronic cohorts.
    * **Statistical Inference:** Calculated 95% Confidence Intervals for lambda (λ) parameters using Central Limit Theorem.
    * **Dispersion Analysis:** Computed dispersion statistics to validate the suitability of the Poisson assumption.
    * **Severity Modeling:** Conducted pooled variance testing to compare claim amounts across demographic segments.

## Key Actuarial Insights
* **Frequency Gap:** Confirmed statistically significant differences in claim rates between policyholders with and without chronic conditions.
* **Interaction Effects:** Analyzed how smoking status interacts with chronic illness to amplify risk, providing a basis for segmented underwriting.
* **Data Quality:** Identified critical missingness checks and range summaries to ensure pricing health insurance policies is based on robust data.

## How to Use
1. Ensure `critical_illness.csv` is in your working directory.
2. Run `Critical Illness Insurance.R` to reproduce the summary statistics and parameter estimations.

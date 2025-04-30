# Project Title

Statistical Analysis of Student Perceptions on AI Adoption in Higher Education

## Overview
This R project performs an exploratory statistical study on how students perceive the adoption of artificial intelligence (AI) within a higher education context. It includes data preprocessing, descriptive and inferential analyses, reliability tests, and hypothesis testing using a Likert-scale questionnaire.

## Features
- **Data Import** from an Excel questionnaire
- **Data Preprocessing**: type conversion, missing value and outlier handling
- **Descriptive Statistics**: univariate and bivariate analyses
- **Reliability Analysis**: Cronbach’s alpha
- **Inferential Tests**: normality checks, non-parametric tests (Shapiro-Wilk, Wilcoxon, Kruskal-Wallis, Chi-squared, Fisher’s exact)
- **Diagnostic Plots**: histograms, boxplots, density plots

## Prerequisites
- R (version ≥ 4.0)
- R packages:
  - `readxl`
  - `moments`
  - `psych`
  - `ltm`

Install packages with:
```r
install.packages(c("readxl", "moments", "psych", "ltm"))
```

## Project Structure
```
├── data/
│   └── questionnaire_responses.xlsx  # Place your cleaned survey file here
├── scriptR.R                        # Main analysis script
└── README.md
```

## Usage
1. Place the survey Excel file in the `data/` directory and rename it to `questionnaire_responses.xlsx`.
2. Open `scriptR.R` in RStudio or your preferred R environment.
3. Run the script step-by-step or source it:
   ```r
   source("scriptR.R")
   ```
4. Review console output and plots for insights.

## Data Handling
- **Input**: A Likert-scale survey Excel file with demographic and questionnaire items (Q1–Q6).
- **Processing Steps**:
  1. Convert character variables to factors and numeric scales.
  2. Handle missing values and check for outliers.
  3. Encode responses on a 1–5 scale (1 = Strongly Disagree to 5 = Strongly Agree).
  4. Conduct normality tests and choose appropriate inferential tests.

## Results
- Summary statistics and frequency tables for demographics and survey items.
- Reliability of instrument (Cronbach’s alpha).
- Hypothesis tests on six research hypotheses covering familiarity, personalization, optimization, ethics, role preference, and transparency.

## Authors
- ENSA Kenitra : Ilyas El Asri

## License
This project is released under the MIT License.


# COVID-19 Data Analysis in R

This project performs statistical analysis and visualization on the **country_wise_latest.csv** dataset containing global COVID-19 data. It uses custom-built R functions to compute statistical measures and compare regions using hypothesis testing and ANOVA.

---

## Dataset

- **File**: `country_wise_latest.csv`
- **Columns include**:
  - `New.cases`
  - `Confirmed`
  - `Deaths...100.Cases`
  - `Recovered...100.Cases`
  - `WHO.Region`

---

## Features & Modules

### 1. **Descriptive Statistics**
- Calculates **mean, median, standard deviation** of:
  - New cases
  - Deaths per 100 cases
  - Recovered per 100 cases
- **Grouped by** WHO region
- **Visualization**: Bar chart of mean new cases by region

### 2. **Correlation Analysis**
- Pearson correlation between:
  - Confirmed cases vs Deaths per 100
  - Confirmed cases vs Recovered per 100
- **Visualization**: Scatter plot with regression line

### 3. **Linear Regression**
- Performs linear regression:
  - Predicting Confirmed cases from New cases
- **Manual implementation**
- **Visualization**: Scatter plot with regression line

### 4. **Hypothesis Testing (t-test)**
- Compares **Africa vs Europe**:
  - Deaths per 100 cases
  - New cases
- **Manual t-statistic computation**
- **Visualization**: Boxplots

### 5. **ANOVA (Analysis of Variance)**
- One-way ANOVA comparing **Deaths per 100 cases** across all WHO regions
- Computes:
  - SST, SSB, SSE, MSB, MSE, F-ratio
  - Degrees of freedom
- **Hypothesis Test Conclusion**: Accept/Reject H₀
- **Visualization**: Histogram of death rate distribution by region

---

## 🔧 Manual Statistical Functions Implemented

- `manual_mean()`
- `manual_median()`
- `manual_sd()` – Sample standard deviation
- `manual_correlation()` – Pearson correlation
- `manual_regression()` – Linear regression slope/intercept
- `manual_t_test()` – 2-sample t-test statistic
- `manual_anova()` – F-ratio for ANOVA

---

## Dependencies

```r
library(dplyr)
library(ggplot2)

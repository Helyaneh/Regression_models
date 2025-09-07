# DM1 Final Estimation Code - Statistical Analysis of Collaborative Work Spaces

## Project Overview

This R script performs comprehensive statistical analysis of Collaborative Work Spaces (CWS) distribution across Italian municipalities using zero-inflated models and logistic regression. The analysis examines factors influencing both the count and presence of Collaborative Work Spaces at the Local Administrative Unit (LAU) level.
## Author & Citation

**Author**: Helyaneh Aboutalebi Tabrizi  
**Email**: Helyaneh.aboutalebi@polimi.it  
**Institution**: Politecnico di Milano  
**Year**: 2025

This script is part of PhD research work. Data and results belong to the author and can be provided upon request.

##  License

This code is provided for academic and research purposes. Please cite appropriately when using or adapting this work.


---

*For questions or issues with the analysis, please refer to the inline documentation within the R script.*
## Dataset

- **File**: `ITA_dataset_FE.xlsx`
- **Scope**: Italian municipalities data
- **Variables**: Socioeconomic, geographic, and infrastructure indicators

## Key Variables

### Dependent Variables
- **Q1**: Number of CWS in LAU (count outcome)
- **Q2**: CWS in LAU (binary outcome - Yes/No)

### Independent Variables
- **Q3**: Rural area dummy (DEGURBA level 1, GHS DUC)
- **Q5**: Log population density of municipalities 2020 per km²
- **Q7**: Log average download speed LAU fixed network 2022 Mbps
- **Q9**: Log average download speed of mobile network in LAU 2022 Mbps
- **Q13**: Municipalities in FUA OECD 2022 dummy
- **Q20/Q21**: Tourism capacity (log and raw number of rooms)
- **Q24**: Log number of cafe/bar/pubs in LAU
- **Q26**: Log tertiary education in LAU percentage
- **Q28**: Strong Innovation region 2022-2023 (RIS)
- **Q30**: Log employed persons in selected enterprises from Ateco in NUTS3 2022
- **Q32**: Inner Alpine dummy
- **Q41**: NUTS2 Regions (factor)

## Model Specifications

### Model 1 (Basic Geographic and Economic)
- **Variables**: Q3, Q20, Q28, Q26, Q30, Q41
- **Zero-inflation predictor**: Q3 (Rural area)

### Model 2 (Infrastructure and Connectivity)
- **Variables**: Q5, Q9, Q7, Q13, Q28, Q26, Q30, Q41
- **Zero-inflation predictor**: Q13 (FUA membership)

### Model 3 (Comprehensive Services and Amenities)
- **Variables**: Q13, Q21, Q7, Q28, Q24, Q26, Q30, Q32, Q41
- **Zero-inflation predictor**: Q13 (FUA membership)

## Statistical Methods

### Regression Models
1. **Zero-Inflated Poisson (ZIP)**: Models count data with excess zeros
2. **Zero-Inflated Negative Binomial (ZINB)**: Accounts for overdispersion
3. **Logistic Regression**: Models binary presence/absence

### Model Evaluation
- **AIC/BIC Comparison**: Information criteria for model selection
- **Vuong Tests**: Compares zero-inflated models against standard Poisson
- **Correlation Analysis**: Spearman correlation matrices

## Required R Packages

```r
install.packages(c(
  "readxl", "ggplot2", "dplyr", "corrplot", 
  "pscl", "MASS", "knitr", "stargazer", 
  "gridExtra", "nonnest2", "labelled"
))
```

## Output Files

### Visualizations
- `correlation_model_1.png`, `correlation_model_2.png`, `correlation_model_3.png`: Correlation heatmaps
- `density_heatmap_model_1.png`, `density_heatmap_model_2.png`, `density_heatmap_model_3.png`: Density heatmaps
- `contour_model_X_Y.png`: Contour plots for each model and method combination

### Analysis Results
- `correlation_pairs.txt`: High correlation pairs (|ρ| > 0.7)
- `model_comparison.html`: AIC and BIC comparison table
- `vuong_tests.html`: Vuong test results comparing models

### Model Summaries
- Individual model summaries: `zeroinfl_[model_name].html`, `logistic_[model_name].html`
- Combined summaries: `zeroinfl_models.html`, `logistic_models.html`

## Usage Instructions

1. **Prepare Environment**
   ```r
   # Set working directory to folder containing the script and data
   setwd("path/to/your/project")
   ```

2. **Run Analysis**
   ```r
   source("1_DM1_Final_Estimation_Code_Updated.R")
   ```

3. **Check Outputs**: All visualizations and results will be saved to the working directory

## Data Requirements

- Excel file named `ITA_dataset_FE.xlsx` in the working directory
- Minimum 10 observations per NUTS2 region (rare levels automatically aggregated)
- No missing values in key variables (handled by `na.omit()`)

## Key Features

### Data Preprocessing
- Automatic handling of rare factor levels in Q41 (NUTS2 regions)
- Missing value removal
- Variable labeling for interpretation

### Robustness Checks
- Model convergence verification
- Variance-covariance matrix validation
- Error handling for prediction failures

### Diagnostic Tools
- Correlation analysis with multicollinearity detection
- Density plots for variable relationships
- Contour plots for predicted outcomes

## Interpretation Guidelines

### Zero-Inflated Models
- **Count component**: Factors affecting CWS count given presence
- **Zero-inflation component**: Factors affecting structural zeros (areas unlikely to have CWS)

### Model Selection
- Lower AIC/BIC indicates better fit
- Significant Vuong test favors zero-inflated over standard Poisson
- Consider theoretical justification alongside statistical criteria

## Technical Notes

- Uses Spearman correlation for non-parametric relationships
- Handles factor variables with automatic reference level selection
- Generates publication-ready tables with stargazer
- Includes comprehensive error handling and validation

## Author & Citation

**Author**: Helyaneh Aboutalebi Tabrizi  
**Email**: Helyaneh.aboutalebi@polimi.it  
**Institution**: Politecnico di Milano  
**Year**: 2025

This script is part of PhD research work. Data and results belong to the author and can be provided upon request.

##  License

This code is provided for academic and research purposes. Please cite appropriately when using or adapting this work.


---

*For questions or issues with the analysis, please refer to the inline documentation within the R script.*
## Version Information

This analysis script is designed for R version 4.0+ and requires the specified package versions for optimal compatibility.
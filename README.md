Code Repository for: Global Burden of CKD and Dementia (1990-2021)
This repository contains the custom R and Python scripts used for the epidemiological and statistical analyses in our manuscript.

File Descriptions:

01_data_cleaning_and_ASR_recalculation: Scripts for cleaning raw GBD 2021 data and manually rescaling the global standard population weights to recalculate Age-Standardized Rates (ASRs) exclusively for the population aged ≥40 years.

02_das_gupta_decomposition_analysis: Implementation of the Das Gupta method to decompose the absolute change in disease burden into population growth, aging, and epidemiological changes.

03_APC_model_intrinsic_estimator: Scripts for the Age-Period-Cohort modeling utilizing the intrinsic estimator method to resolve collinearity.

04_ecological_correlation_pearson: Scripts for logarithmic transformations of ASDRs and calculating Pearson correlation coefficients across 21 GBD regions.

05_ARIMA_forecasting_auto_arima: R script utilizing the auto.arima algorithm for projecting disease trends through 2040.

06_data_visualization_heatmaps_and_trends: Code for generating the spatial distribution maps, heatmaps, and temporal trend figures.

Software Note:
Joinpoint regression analysis was performed using the standalone National Cancer Institute (NCI) Joinpoint Regression Program (Version 5.0.2) via its graphical user interface. Therefore, no custom code is provided for that specific analysis step.

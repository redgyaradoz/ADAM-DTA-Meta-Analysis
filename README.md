# ADAM-DTA-Meta-Analysis
A one-stop Shiny R application for performing Diagnostic Test Accuracy meta-analysis. This application performs diagnostic test accuracy (DTA) meta-analysis using R. Generates HSROC curves, coupled forest plots, and QUADAS-2 risk of bias assessments.  Access the live web application here: https://dvignesh.shinyapps.io/DTA_tool/


# Long Description
# ADAM: Automated Diagnostic Accuracy Meta-analysis

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18195913.svg)](https://doi.org/10.5281/zenodo.18195913)
**ADAM** is a free, open-source R Shiny application designed to make Diagnostic Test Accuracy (DTA) meta-analysis accessible to researchers without coding experience. 

## 🚀 Key Features
* **Coupled Forest Plots:** Visualize Sensitivity and Specificity side-by-side with distinct colors.
* **HSROC Analysis:** Automated Hierarchical Summary ROC curves with prediction regions.
* **Risk of Bias (QUADAS-2):** Generate "robvis" style traffic light and summary plots instantly.
* **Subgroup Analysis:** Filter studies based on Risk of Bias (High/Low/Unclear) or study design.


## 📊 Statistical Methods
The tool uses industry-standard R packages ensuring acceptance by major journals:
* **HSROC:** `mada` package (Reitsma et al. bivariate model)
* **Pooled Estimates:** `meta` package (DerSimonian-Laird random effects)
* **Publication Bias:** Deeks' funnel plot asymmetry test.

## 📥 How to Use
You can run ADAM locally or access the web version:
1.  **Web Version:** https://dvignesh.shinyapps.io/DTA_tool/
2.  **Local RStudio:** Download this repository, open `app.R`, and click "Run App".

## 📜 Citation
If you use ADAM in your research, please cite:
> Vignesh D. (2026). ADAM: Automated Diagnostic Accuracy Meta-Analysis (1.1.0). Zenodo. https://doi.org/10.5281/zenodo.18195913

## 📬 Contact & Bug Reports
Created by **Dr. Vignesh D**
Asst. Professor, Community Medicine
ESIC Medical College and Hospital, Chennai
e-mail: d.vignesh1991@gmail.com

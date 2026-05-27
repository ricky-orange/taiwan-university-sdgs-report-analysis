# Analyzing the SDGs Actions of Taiwanese Universities Through Sustainability Reports

This repository contains the code, standardized prompt, and analysis scripts used in the master's thesis:

**Analyzing the SDGs Actions of Taiwanese Universities Through Sustainability Reports**  
黃俊肇（2026）  
Institute of Public Affairs Management, National Sun Yat-sen University

This project applies ChatGPT-assisted text analysis to Taiwanese university sustainability reports, focusing on:

- SDGs mapping
- Disclosure quality
- Deviation between claimed SDGs and textual content
- Keyword analysis
- Inter-rater reliability analysis
- Statistical visualization

## Repository Files

### `GPT_API.py`

Main Python script for calling the ChatGPT API and conducting semantic analysis.

It is used for:

- SDGs classification
- Disclosure quality evaluation
- SDGs deviation analysis
- CSV output generation

### `Standardized_Prompt_API.txt`

The standardized prompt used in this study.

It defines:

- SDGs mapping rules
- Disclosure quality criteria
- Deviation judgment criteria
- Required output format
- CSV column structure

### `dummy.py`

Python script used to transform wide-format data into long-format data.

### `dummyR.R`

R script for calculating Cohen’s kappa.

This script is used to evaluate agreement between AI coding and human coding.

### `dummyR_AC1.R`

R script for calculating Gwet’s AC1.

This script is used as an additional reliability measure.

### `prepare_keyword_analysis.R`

R script for keyword preprocessing.

It is used to:

- Split keyword fields
- Convert keyword data into long format
- Prepare keyword frequency analysis data

### `chapter5_figures_R_script.R`

R script for generating statistical figures used in Chapter 5.

It includes figures related to:

- SDGs distribution
- Disclosure quality
- Deviation rates
- Cross-university comparison

### `create_keyword_analysis_figures.R`

R script for generating keyword-related statistical figures.

It includes figures related to:

- Keyword frequency
- Keyword distribution
- Keyword deviation analysis

## Reproducibility

This repository is intended to improve methodological transparency and reproducibility.

The repository includes:

- Analysis scripts
- Standardized prompt
- Reliability analysis scripts
- Keyword analysis scripts
- Figure generation scripts

The repository does **not** include:

- Full sustainability report texts
- Copyrighted university report contents
- API keys
- Private or sensitive data

## Environment

### Python

Recommended version:

```text
Python 3.10+

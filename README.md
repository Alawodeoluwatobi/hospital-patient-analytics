# hospital-patient-analytics
Analysis of 55,500 hospital patient encounters answering 10 healthcare business questions across financial, utilization, and quality domains using R and Excel.Analysis of 55,500 hospital patient encounters answering 10 healthcare business questions across financial, utilization, and quality domains using R and Excel.
# Hospital Patient Analytics — 10 Healthcare Business Questions

**Tools:** R (tidyverse, ggplot2, rstatix, scales) | Microsoft Excel  
**Dataset:** 55,500 patient encounters | 2019–2024  
**Author:** Oluwatobi A. Alawode, PhD  

## Project Overview
This project analyzes a hospital patient encounter dataset containing 
55,500 records spanning 5 years (2019–2024). Using R and Excel, I 
answered 10 business questions across three analytical domains — 
Financial & Revenue, Utilization & Operations, and Quality & Outcomes 
— and translated findings into strategic recommendations for clinical 
and operational decision-makers.

The project demonstrates an end-to-end healthcare data analytics 
workflow: data cleaning and preparation, exploratory analysis, 
statistical testing, visualization, and executive-level reporting.

## Business Questions Answered

### Financial & Revenue
- Q1. Which insurance providers account for the highest total billing?
- Q2. What is the average billing amount by insurance provider?
- Q3. Do emergency admissions cost more than elective or urgent?
- Q4. Which medical conditions are associated with the highest billing?

### Utilization & Operations
- Q5. What is the average length of stay by admission type?
- Q6. Are older patients associated with longer stays or higher costs?
- Q7. Which hospitals handle the highest volume of emergency admissions?

### Quality & Outcomes
- Q8. Are abnormal test results associated with higher billing or longer stays?
- Q9. Does admission type influence test outcomes?
- Q10. Are certain conditions more likely to result in emergency admissions?

## Key Findings

- **Balanced payer mix:** All 5 insurance providers fall within an 
  $8.3M revenue range — no single payer dominates the portfolio
- **Counterintuitive billing pattern:** Elective admissions ($25,603) 
  cost marginally more than emergency admissions ($25,498) — 
  challenging conventional assumptions about emergency care costs
- **LOS is acuity-blind:** Emergency, elective, and urgent admissions 
  all average ~15.5 days — suggesting discharge planning protocols 
  are not differentiated by clinical acuity
- **Age is not a utilization predictor:** Correlation between age and 
  LOS (r = 0.008) and billing (r = -0.004) are near zero and 
  not statistically significant
- **Obesity is a dual-priority target:** Highest average billing 
  ($25,807) AND highest emergency admission rate (33.9%) — 
  making it the strongest candidate for population health intervention


## Statistical Methods

| Method | Application |
|---|---|
| Descriptive statistics | Mean, median, SD across all groupings |
| Pearson correlation | Age vs. LOS and Age vs. Billing (Q6) |
| One-way ANOVA | Test results vs. Billing and LOS (Q8) |
| Chi-square test | Admission type vs. Test results (Q9); Condition vs. Admission type (Q10) |

## Repository Contents

| File | Description |
|---|---|
| `analysis.R` | Complete annotated R script for all 10 questions |
| `Hospital_Analytics_Report.pdf` | Full analytical report with findings and recommendations |
| `Healthcare_Analytics.xlsx` | Source dataset (55,500 patient records) |
| `figures/` | All visualizations exported from R |
| `README.md` | This file |

## How to Reproduce This Analysis
1. Clone this repository
2. Open `analysis.R` in RStudio
3. Install required packages:

```r
   install.packages(c("tidyverse", "readxl", 
                      "rstatix", "scales", "patchwork"))
```

4. Set your working directory to the repo folder
5. Run the script top to bottom

## Dataset
The dataset (`Healthcare_Analytics.xlsx`) contains simulated hospital 
patient encounter records with the following variables: patient 
demographics (age, gender, blood type), clinical information 
(medical condition, admission type, test results, medication), 
operational data (admission date, discharge date, hospital, doctor), 
and financial data (billing amount, insurance provider).

## About the Author
Oluwatobi A. Alawode is a PhD-trained population health researcher 
and data analyst with expertise in health equity, healthcare 
utilization, and data science. 

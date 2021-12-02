# SOC_Tier_2_New

## Code index

1 - 00_formatting_bug_algae_hydo.R
2a - 01_csci_median_logistic regression thresholds.R
2b - 01a_asci_median_logistic regression_thresholds.R
3 - 03_GLM_figures.R
4 - 04_correlation_matrix.R


## code and data descriptions

# 1 - 00_formatting_bug_algae_hydo.R

Formatting raw CSCI and ASCI data extracted from SMC (By Liesl)
Combining delta H FFM woth CSCI ASCI data
Taking the median delta H over all years
Output is dataframe with bio (CSCI/ASCI) and hydro (delta H FFM) for GLMs:

00_csci_delta_formatted_median_updated_Nov2021.csv
00_asci_delta_formatted_median_Nov2021.csv

# 2a - 01_csci_median_logistic regression thresholds.R

Logistic regression (GLMs) for CSCI and FFM (delta H)
output is predicted probability for all combinations, with input binary response and hydro

## just CSCI
01_csci_neg_pos_logR_metrics_figures_April2021.csv

## OoverE, MMI & CSCI

01_bugs_all_data_neg_pos_logR_metrics_figures_April2021.csv

# 2b - 01a_asci_median_logistic regression_thresholds.R

Same as above but with ASCI

## Just Hybrid ASCI

01_h_asci_neg_pos_logR_metrics_figures_April2021.csv

## Hybrid and Diatoms

01_algae_all_data_neg_pos_logR_metrics_figures_April2021.csv

# 3 - 03_GLM_figures.R

Figures for all FFM and bio indices using 10th percentile threshold (CSCI = 0.79, ASCI = 0.86)
These figures are to be used in the FFM filtering process and are avilable in the manuscript supplementary material (Figures xxx)

# 4 - 04_correlation_matrix.R

Correlation of FFM delta H values at sites matched with CSCI and ASCI. Produces two correltion matrices:

04_ffm_cor_csci.csv
04_ffm_cor_asci.csv


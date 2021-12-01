# SOC_Tier_2_New

## Code index

1 - 00_formatting_bug_algae_hydo.R
2a - 01_csci_median_logistic regression thresholds.R
2b - 01a_asci_median_logistic regression_thresholds.R


## code and data descriptions

1 - 00_formatting_bug_algae_hydo.R

Formatting raw CSCI and ASCI data extracted from SMC (By Liesl)
Combining delta H FFM woth CSCI ASCI data
Taking the median delta H over all years
Output is dataframe with bio (CSCI/ASCI) and hydro (delta H FFM) for GLMs:

00_csci_delta_formatted_median_updated_Nov2021.csv
00_asci_delta_formatted_median_Nov2021.csv

2a - 01_csci_median_logistic regression thresholds.R

Logistic regression (GLMs) for CSCI and FFM (delta H)
output is predicted probability for all combinations, with input binary response and hydro

01_csci_neg_logR_metrics_figures_April2021.csv
01_csci_pos_logR_metrics_figures_April2021.csv

2b - 01a_asci_median_logistic regression_thresholds.R

Same as above but with ASCI

01_h_asci_pos_logR_metrics_figures_April2021.csv
01_h_asci_neg_logR_metrics_figures_April2021.csv

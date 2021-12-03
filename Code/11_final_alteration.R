## biological alteration

library(tidyverse)
library(tidyr)

## Kris - some of the data for the basemap is in folder "Data". Paths can be found in script 10_Alteration_maps.R

## data

## current
current <- read.csv("output_data/Manuscript/08_metric_suitability_tally_condensed_all_sites_current.csv")
head(current)
## water conservation
watercons <- read.csv("output_data/Manuscript/08_metric_suitability_tally_condensed_all_sites_watercons.csv") 

## thresholds
# CSCI - Bio_threshold = 0.92, Threshold = Threshold25
# ASCI - Bio_threshold = 0.94, Threshold = Threshold50
### details for manuscript

library(ggplot2)
library(purrr)
library(dplyr)
library(tidyverse)


# GLM coefs ---------------------------------------------------------------


## standard error function
std <- function(x) sd(x)/sqrt(length(x))

csci <- read.csv("output_data/manuscript/01_csci_glm_coefs.csv")

asci <- read.csv("output_data/manuscript/01_asci_glm_coefs.csv")
head(asci)
csci <- filter(csci, thresholds == 0.79)
asci <- filter(asci, thresholds == 0.86)
csci <- csci %>%
  select( -McFadden, -Nagelkerke, - comb_code, -X, -thresholds) %>%
  # select(biol.endpoints, hydro.endpoints, thresholds) %>%
  # rename(IndexThreshold = thresholds) %>%
  filter(biol.endpoints == "CSCI")

asci <- asci %>%
  select( -McFadden, -Nagelkerke, - comb_code, -X,  -thresholds) %>%
  # select(biol.endpoints, hydro.endpoints, thresholds) %>%
  # rename(IndexThreshold = thresholds) %>%
  filter(biol.endpoints == "H_ASCI") %>%
  mutate(biol.endpoints = gsub("H_", "", biol.endpoints))

alldata <- rbind(csci, asci)

write.csv(alldata, "output_data/manuscript/14_glm_coefs.csv") ## supplementary material


range(csci$AIC) #13.54557 396.32787
range(asci$AIC) #  15.41918 324.00396

mean(csci$AIC) # 215.3436
mean(asci$AIC) # 175.0036

std(csci$AIC) # 20.93858
std(asci$AIC) # 17.12798



# comparison analysis -----------------------------------------------------


delta <- read.csv("output_data/manuscript/08_delta_H_all_years_and_subbasins.csv")
head(delta)

## filter to wet bfl dur

delta <- delta %>%
  filter(flow_metric ==  "DS_Dur_WS")

range(na.omit(delta$deltah_cur_ref_final))
#-196  176
196 + 176
372
mean(na.omit(delta$deltah_cur_ref_final))
##  2.131119

# cons delta limit
# -26.17 to 1.95
26.17+ 1.95
28.12

# liberal delta limited
# --146.5 to 148.4
146.5 + 148.4
294.9

#min 28.12
# max 294.9
( 28.12+294.9)/2
# 161.51
294.9 -  28.12
# 266.78
(266.78/161.51)
(266.78/161.51)*100
# 165.1786 - relative percent change

delta$deltah_cur_ref_final

# ## difference in combination delta h limits of entire range of delta h
118.27 - 19.27 # sum
266.78/372*100



# Altered High Subbasin ---------------------------------------------------

data <- read.csv("output_data/Manuscript/08_altered_metrics_direction_current_overall.csv")
head(data)

  

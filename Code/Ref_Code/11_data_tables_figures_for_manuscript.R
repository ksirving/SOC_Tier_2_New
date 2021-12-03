### ASCI & CSCI delta H analysis
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyverse)
## run delta h thresholds over all san juan sites at different thresholds
## 25%, 50%, 75%
getwd()

# dalta h   ---------------------------------------------------

## upload thresholds 
thresholds <-read.csv( "output_data/results/08_all_delta_thresholds_scaled_April2021.csv")
head(thresholds)

thresholds <- thresholds %>%
  select(-X.1, -X, -n) %>%
  rename(Hydro_Metric = Hydro_endpoint) %>% 
  pivot_longer(Threshold25:Threshold75, names_to = "Threshold") %>%
  rename(DeltaH = value) #%>%

thresholdsall <- thresholds %>%
  pivot_wider(names_from = Type, values_from = DeltaH)

## save data - all delta limits for table
write.csv(thresholdsall, "output_data/manuscript/11_all_delta_h_limits_all_combinations_thresh_combs.csv")

thresholdsall <- thresholdsall %>%
  mutate(codeP = paste(Bio_endpoint,Bio_threshold,Threshold, sep="_")) %>%
  mutate(codeH = paste(Bio_endpoint, "_", Hydro_Metric, sep="")) %>%
  filter(codeH %in% c("H_ASCI_Q99", "H_ASCI_DS_Mag_50", "H_ASCI_SP_Dur",
                      "CSCI_Q99", "CSCI_DS_Mag_50",  "CSCI_SP_Tim")) %>%
  ungroup() %>%
  select( -Bio_endpoint, -codeP, -codeH)

## save data - all delta limits for table
write.csv(thresholdsall, "output_data/manuscript/11_all_delta_h_limits_thresh_combs.csv")

str(thresholds)

head(thresholdsall)
metrics <- unique(thresholdsall$Hydro_Metric)
metrics
## upload delta h 

delta <- read.csv("Data/DeltaH_master.csv")
head(delta)
## subset to only chosen metrics
delta <- delta %>%
  filter(flow_metric %in% c("Q99", "DS_Mag_50", "SP_Tim", "SP_Dur"))

head(delta)

write.csv(delta, "output_data/manuscript/11_delta-H_all_years_subbasins.csv")

## rename 
delta <- delta %>%
  rename(Hydro_Metric = flow_metric) #%>%

## join thresholds with delta data
delta_df <- full_join(delta, thresholdsall, by="Hydro_Metric")
head(delta_df)

## alteration per subbasin, per year - within limits
delta_dfx <- delta_df %>%
  group_by(site, year, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  mutate(Alteration_Current = ifelse(deltaH_cur_ref <= positive & deltaH_cur_ref >= negative, "Unaltered", "Altered")) %>%
  mutate(Alteration_WaterCon = ifelse(deltaH_watercon_ref <= positive & deltaH_watercon_ref >= negative, "Unaltered", "Altered"))


View(delta_dfx) 

write.csv(delta_dfx, "output_data/manuscript/11_alteration_by_year_site_all_sites_June_2021.csv")
delta_dfx <- read.csv("output_data/manuscript/11_alteration_by_year_site_all_sites_June_2021.csv")
unique(delta_dfx$Hydro_Metric)
head(delta_dfx)

ranges <-delta_dfx %>%
  group_by(Biol) %>%
  summarise(rg = range(na.omit(Alteration_Current)))

ranges

# alteration direction  ---------------------------------------------------

## current

##  direction per year
names(metric_tally_current_dir)
metric_tally_current_dir <- delta_dfx %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  filter(Alteration_Current == "Altered") %>%
  mutate(Alteration_Current_direction = ifelse(deltaH_cur_ref > positive, "High", "Low")) %>%
  select(site, region, year, Hydro_Metric, Biol, Alteration_Current, Alteration_Current_direction)

write.csv(metric_tally_current_dir, "output_data/manuscript/11_altered_metrics_direction_current_by_year_June2021.csv")


### count number of years altered - overall
metric_tally_current_dir <- delta_dfx %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  filter(Alteration_Current == "Altered") %>%
  mutate(Alteration_Current_direction = ifelse(deltaH_cur_ref > positive, "High", "Low")) %>%
  count(Alteration_Current_direction) %>%
  mutate(Percentage = n/sum(n)*100) %>%
  select(-n) %>%
  pivot_wider(names_from = Alteration_Current_direction, values_from = Percentage) #%>%

dirs <- seq(1, dim(metric_tally_current_dir)[1], 1)  
metric_tally_current_dir$Direction <- NULL

for(p in 1: length(dirs)) {
  
  metric_tally_current_dir$Direction[p] =  if(is.na(metric_tally_current_dir$High[p])) {
    paste("Low")
  } else if (metric_tally_current_dir$High[p] == 50) {
    paste("Uncertain")
  } else  if(metric_tally_current_dir$High[p] >= 60){
    paste("High")
  } else if (metric_tally_current_dir$High[p] <= 40) {
    paste("Low")
  } else if(is.na(metric_tally_current_dir$High[p])) {
    paste("Low")
  } else {
    paste("Uncertain")
  }
  
}

write.csv(metric_tally_current_dir, "output_data/manuscript/11_altered_metrics_direction_current_June2021.csv")
metric_tally_current_dir <- read.csv("output_data/manuscript/11_altered_metrics_direction_current_June2021.csv")
head(metric_tally_current_dir)

metric_tally_current <- delta_dfx %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  count(Alteration_Current)

metric_tally_current <- na.omit(metric_tally_current)
metric_tally_current

write.csv(metric_tally_current, "output_data/manuscript/11_metric_suitability_tally_all_sites_current_June2021.csv")
# ?pivot_wider

## make %
metric_tally_current <- metric_tally_current %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  mutate(Percentage = n/sum(n)*100, YearsWithData = sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = Alteration_Current, values_from = Percentage)

metric_tally_current

YearsWithData <- metric_tally_current %>%
  ungroup() %>%
  select(site,Hydro_Metric, YearsWithData, Unaltered, Altered) %>%
  distinct()

write.csv(YearsWithData , "output_data/manuscript/11_sites_metrics_number_of_years_with_delta.csv")

metric_tally_current
metric_tally_current[is.na(metric_tally_current)] <- 0

# write.csv(metric_tally, "output_data/09_metric_suitability_tally_condensed_aliso_oso_small_creeks_April2021.csv")
write.csv(metric_tally_current, "output_data/manuscript/11_metric_suitability_tally_condensed_all_sites_current_June2021.csv")
metric_tally_current <- read.csv("output_data/manuscript/11_metric_suitability_tally_condensed_all_sites_current_June2021.csv")

ranges <- metric_tally_current %>%
  group_by(Biol) %>%
  summarise(rg <- range(Altered))
ranges
#### water conservation

metric_tally_water_dir <- delta_dfx %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  filter(Alteration_WaterCon == "Altered") %>%
  mutate(Alteration_WaterCon_direction = ifelse(deltaH_cur_ref > positive, "High", "Low"))  %>%
  select(site, region, year, Hydro_Metric, Biol, Alteration_WaterCon, Alteration_WaterCon_direction)

write.csv(metric_tally_water_dir, "output_data/manuscript/11_altered_metrics_direction_watercon_by_year_June2021.csv")


### count number of years altered
metric_tally_water_dir <- delta_dfx %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  filter(Alteration_WaterCon == "Altered") %>%
  mutate(Alteration_WaterCon_direction = ifelse(deltaH_cur_ref > positive, "High", "Low")) %>%
  count(Alteration_WaterCon_direction) %>%
  mutate(Percentage = n/sum(n)*100) %>%
  select(-n) %>%
  pivot_wider(names_from = Alteration_WaterCon_direction, values_from = Percentage) #%>%

dirs <- seq(1, dim(metric_tally_water_dir)[1], 1)  
metric_tally_water_dir$Direction <- NULL

for(p in 1: length(dirs)) {
  
  metric_tally_water_dir$Direction[p] =  if(is.na(metric_tally_water_dir$High[p])) {
    paste("Low")
  } else if (metric_tally_water_dir$High[p] == 50) {
    paste("Uncertain")
  } else  if(metric_tally_water_dir$High[p] >= 60){
    paste("High")
  } else if (metric_tally_water_dir$High[p] <= 40) {
    paste("Low")
  } else if(is.na(metric_tally_water_dir$High[p])) {
    paste("Low")
  } else {
    paste("Uncertain")
  }
  
}

write.csv(metric_tally_water_dir, "output_data/manuscript/11_altered_metrics_direction_water_conservation_June2021.csv")

metric_tally_water <- delta_dfx %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  count(Alteration_WaterCon)

metric_tally_water <- na.omit(metric_tally_water)
metric_tally_water

# write.csv(metric_tally, "output_data/09_metric_suitability_tally_aliso_oso_small_creeks_April2021.csv")
write.csv(metric_tally_water, "output_data/manuscript/11_metric_suitability_tally_all_sites_watercons_June2021.csv")
# ?pivot_wider

## make %
metric_tally_water <- metric_tally_water %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  mutate(Percentage = n/sum(n)*100) %>%
  select(-n) %>%
  pivot_wider(names_from = Alteration_WaterCon, values_from = Percentage)

metric_tally_water
metric_tally_water[is.na(metric_tally_water)] <- 0

# write.csv(metric_tally, "output_data/09_metric_suitability_tally_condensed_aliso_oso_small_creeks_April2021.csv")
write.csv(metric_tally_water, "output_data/manuscript/11_metric_suitability_tally_condensed_all_sites_watercons_June2021.csv")


# prioritisation ----------------------------------------------------------

# get high priority sites to see if ffm are higher/lower
pr <- read.csv("output_data/manuscript/SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary.csv")
head(pr)

table(pr$hydro.alteration.ASCI)
# Likely Altered Likely Unaltered 
# 25               35 

table(pr$hydro.alteration.CSCI)
# Likely Altered Likely Unaltered 
# 21               39

sum(pr$synthesis_alteration == "High Priority") #11

high_pr <- pr %>%
  filter(synthesis_alteration ==  "High Priority")

high_pr$New_Name

metric_tally_current_dir <- read.csv("output_data/manuscript/11_altered_metrics_direction_current_June2021.csv")
head(metric_tally_current_dir)

#lookuptable to convert subbasin codes for model output subbasin names - doesn't exist!!!
subbasin_lookup <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/Old_Runs/191220_Interim_Calibration/site_name_lookupletternumbers.csv")
subbasin_lookup
#convert basin orig name to outputfile name (model subbasin name)
pr$pr_site <- pr$New_Name
pr$pr_site

for(z in 1:length(subbasin_lookup$Letter)){
  pr$pr_site <- gsub(subbasin_lookup$Letter[z], subbasin_lookup$Number[z], pr$pr_site)
}

#find and replace - in new.subbasinname with nothing, make consistent with file name
pr$pr_site <- gsub("-", "", pr$pr_site)
pr$pr_site <- as.numeric(pr$pr_site)

head(pr)

high_pr <- pr %>%
  filter(synthesis_alteration ==  "High Priority")
high_pr

high_sites <- high_pr$pr_site

alt_dir <- metric_tally_current_dir %>%
  filter(site %in% high_sites)

head(alt_dir)
alt_dir_asci <- alt_dir %>%
  filter(Biol == "ASCI", Threshold == "Threshold75", Bio_threshold == 0.94)
alt_dir_asci
alt_dir_csci <- alt_dir %>%
  filter(Biol == "CSCI", Threshold == "Threshold25", Bio_threshold == 0.92)



# comparison of decisions -------------------------------------------------


#### over time

delta_dfx <- read.csv( "output_data/manuscript/11_alteration_by_year_site_all_sites_June_2021.csv")
names(delta_dfx)

delta_dfx <- na.omit(delta_dfx)

## get number of sites per year altered and combination code

Year_Tally <- delta_dfx %>%
  select(-current_value, -reference_value, -X, -water_year_type, -positive, - negative, -metric) %>%
  mutate(CombCode = paste0(Bio_threshold, "_", Threshold )) %>%
  group_by(year, Hydro_Metric,Biol, Bio_threshold, Threshold, CombCode) %>%
  count(Alteration_Current) %>%
  mutate(Percentage = n/sum(n)*100) %>%  select(-n) %>%
  pivot_wider(names_from = Alteration_Current, values_from = Percentage)

Year_Tally[is.na(Year_Tally)] <- 0

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("CSCI","ASCI"))

head(Tally0)

meantally <- Tally0 %>%
  ungroup() %>%
  group_by(Hydro_Metric,Biol, Bio_threshold, Threshold, CombCode) %>%
  summarise(MeanAlteration = mean(Altered))

rangetally <- Tally0 %>%
  ungroup() %>%
  group_by(Hydro_Metric,Biol, Bio_threshold, Threshold, CombCode) %>%
  summarise(MeanAlteration = range(Altered))
rangetally

write.csv(meantally, "output_data/manuscript/11_mean_alteration_years.csv")

######### figures
getwd()
out.dir <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SOC_tier2/SOC_tier_2/output_data/manuscript/figures/"


asci_metrics <- c("Q99", "SP_Dur", "DS_Mag_50")
csci_metrics <-c("Q99", "SP_Tim","DS_Mag_50" )

# SP_ROC ## used this one for sfs
Year_Tally$Bio_threshold <- as.character(Year_Tally$Bio_threshold)

Tally <- Year_Tally %>%
  group_by(CombCode, Hydro_Metric, Biol) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) 

write.csv(Tally, "output_data/manuscript/11_subbasin_alteration.csv")

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("CSCI"), Hydro_Metric == "SP_Tim")

head(Tally0)
sum(is.na(Tally0))

csci1 <- ggplot(data=Tally0, aes(x = year, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 40, ymax = 60, xmin = 1994, xmax = 2018,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Spring Timing", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_colour_discrete(name  ="CSCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))


csci1
?scale_y_continuous
out.filename <- paste0(out.dir,"CSCI_SP_Tim_Alt_over_time_June2021_for_ppt.jpg")
ggsave(csci1, file = out.filename, dpi=300, height=4, width=6)

## find metrics within limits

Tally0x <- Tally0 %>%
  group_by(CombCode, Hydro_Metric) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) #%>%
  filter(AlteredMean > 25 & AlteredMean < 75)
Tally0x
unique(Tally0x$CombCode)

# "0.63_Threshold50" "0.79_Threshold25" "0.92_Threshold25" - old

# "0.79_Threshold50" "0.79_Threshold75" "0.92_Threshold50" "0.92_Threshold75" - updated

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("CSCI"), Hydro_Metric == "DS_Mag_50")

head(Tally0)

csci2 <- ggplot(data=Tally0, aes(x = year, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 1994, xmax = 2018,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Dry Season Baseflow Magnitude", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_discrete(name  ="CSCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))

csci2

out.filename <- paste0(out.dir,"CSCI_DS_Mag_50_Alt_over_time.jpg")
ggsave(csci2, file = out.filename, dpi=300, height=4, width=6)


## find metrics within limits

Tally0x <- Tally0 %>%
  group_by(CombCode) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) #%>%
  filter(AlteredMean > 25 & AlteredMean < 75)
  Tally0x
unique(Tally0x$CombCode) 

#"0.63_Threshold75" "0.79_Threshold25" "0.79_Threshold50" "0.92_Threshold25" - old
# "0.79_Threshold75" "0.92_Threshold25" - new

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("CSCI"), Hydro_Metric == "Q99")

head(Tally)

csci2 <- ggplot(data=Tally0, aes(x = year, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 1994, xmax = 2018,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Magnitude of Largest Storm", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_discrete(name  ="CSCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))

csci2

out.filename <- paste0(out.dir,"CSCI_Q99_Alt_over_time.jpg")
ggsave(csci2, file = out.filename, dpi=300, height=4, width=6)

## find metrics within limits

Tally0x <- Tally0 %>%
  group_by(CombCode) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) #%>%
  filter(AlteredMean > 25 & AlteredMean < 75)
  Tally0x
unique(Tally0x$CombCode) 

#  "0.92_Threshold25" "0.92_Threshold50" "0.92_Threshold75"

## CSCI - old!!!
# "0.63_Threshold50" "0.79_Threshold25" "0.92_Threshold25"
#"0.63_Threshold75" "0.79_Threshold25" "0.79_Threshold50" "0.92_Threshold25"
# "0.92_Threshold75"

# CSCI - new!!

# "0.92_Threshold25" "0.92_Threshold50" "0.92_Threshold75" q99
# "0.79_Threshold75" "0.92_Threshold25" - ds_mag_50
# "0.79_Threshold50" "0.79_Threshold75" "0.92_Threshold50" "0.92_Threshold75" - SP_tim

### Q99 - 0.79_Threshold75 = 18.5% alteration
### sp_tim - 0.92_Threshold25 = 16.4
### ds_mag_50 = 0.92_Threshold50 = 79.1

## use - old
## 0.92_Threshold25 or 0.79_Threshold25
## Q99 0.92_Threshold75

## use new
##0.92_Threshold50 = 79.1% ds_mag_50 - closest to the cut off


asci_metrics <- c("Q99", "SP_Dur", "DS_Mag_50")

Tally1 <- Year_Tally %>%
  filter(Biol %in% c("ASCI"), Hydro_Metric == "DS_Mag_50")

head(Tally1)

asci1 <- ggplot(data=Tally1, aes(x = year, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 1994, xmax = 2018,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Dry Season Magnitude", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_discrete(name  ="ASCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))
asci1
out.filename <- paste0(out.dir,"ASCI_DS_Mag_50_Alt_over_time.jpg")
ggsave(asci1, file = out.filename, dpi=300, height=4, width=6)

## find metrics within limits

Tally1x <- Tally1 %>%
  group_by(CombCode) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
  filter(AlteredMean > 25 & AlteredMean < 75)
Tally1x
unique(Tally1x$CombCode) ## "0.75_Threshold75" "0.86_Threshold75" "0.94_Threshold75"
## "0.75_Threshold75" "0.86_Threshold75" "0.94_Threshold25" "0.94_Threshold50" "0.94_Threshold75 - new!

Tally1 <- Year_Tally %>%
  filter(Biol %in% c("ASCI"), Hydro_Metric == "SP_Dur")

head(Tally1)

asci2 <- ggplot(data=Tally1, aes(x = year, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 1994, xmax = 2018,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Spring Duration", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_discrete(name  ="ASCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))

asci2

out.filename <- paste0(out.dir,"ASCI_Sp_Dur_Alt_over_time.jpg")
ggsave(asci2, file = out.filename, dpi=300, height=4, width=6)

## find metrics within limits

Tally1x <- Tally1 %>%
  group_by(CombCode) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
  filter(AlteredMean > 25 & AlteredMean < 75)
  Tally1x
unique(Tally1x$CombCode) ## 0.94_Threshold75
## "0.75_Threshold50" "0.86_Threshold50" "0.94_Threshold25" - new!!!
# "0.75_Threshold25" "0.75_Threshold50" "0.75_Threshold75" "0.86_Threshold25"
# [5] "0.86_Threshold50" "0.94_Threshold25" "0.94_Threshold50"

Tally1 <- Year_Tally %>%
  filter(Biol %in% c("ASCI"), Hydro_Metric == "Q99")

head(Tally1)

asci2 <- ggplot(data=Tally1, aes(x = year, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 1994, xmax = 2018,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Magnitude of Largest Storm", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_discrete(name  ="ASCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))

asci2

out.filename <- paste0(out.dir,"ASCI_Q99_Alt_over_time.jpg")
ggsave(asci2, file = out.filename, dpi=300, height=4, width=6)

## find metrics within limits

Tally1x <- Tally1 %>%
  group_by(CombCode) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) #%>%
filter(AlteredMean > 25 & AlteredMean < 75)
Tally1x
unique(Tally1x$CombCode) ## 0.94_Threshold75
## 0.94_Threshold75"



## ASCI
# 0.94_Threshold75" == Q99
## ##  "0.75_Threshold25" "0.75_Threshold50" "0.75_Threshold75" "0.86_Threshold25" "0.86_Threshold50" "0.94_Threshold25" "0.94_Threshold50" = SP_Dur
## "0.94_Threshold50" - DS_Mag  "0.75_Threshold75" "0.86_Threshold75" "0.94_Threshold25" "0.94_Threshold50"
# [5] "0.94_Threshold75

## Q99 -  0.94_Threshold50 = 23.1
## SP_Dur = 0.94_Threshold50 = 69.4
## DS_MAG 0.94_Threshold50 = 71.7


## Q99 -  0.94_Threshold25 = 16.8
## SP_Dur = 0.94_Threshold25 = 48.4
## DS_MAG 0.94_Threshold25 = 26

(25/60)*100 # asci
(21/60)*100 # csci

(11/60)*100 # high
(24/60)*100 # medium
(25/60)*100 # low

### combine mean dfs

head(Tally) ## subbasin percentage
Tally <- rename(Tally, SubbasinAlteration = AlteredMean)
head(meantally) ## years of alteration
meantally <- rename(meantally, YearsAlteration = MeanAlteration)

alltally <- full_join(Tally, meantally, by = c("Hydro_Metric", "CombCode", "Biol"))
### means are all same
## add in alteration by year and alteration by subbasin in supp mat
write_csv(alltally, "output_data/manuscript/11_all_alteration.csv")
alltally

# GLM results -------------------------------------------------------------
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

write.csv(alldata, "output_data/manuscript/11_glm_coefs.csv")



range(csci$AIC) # 20.14221 496.69499
range(asci$AIC) #  24.73884 355.65710

mean(csci$AIC) # 275.9993
mean(asci$AIC) # 197.4792

std(csci$AIC) # 12.2837
std(asci$AIC) # 10.99946

# range(csci$VariablePvalue)
# range(asci$VariablePvalue)
# 
# mean(csci$VariablePvalue)
# mean(asci$VariablePvalue)
# 
# std(csci$VariablePvalue)
# std(asci$VariablePvalue)

## n per ffm



# correlation matrix ------------------------------------------------------

data <- read.csv("output_data/00_csci_delta_formatted_median_updated_Nov2021.csv")
head(data)

data <- select(data, DS_Dur_WS:Wet_Tim)
str(data)
?cor
data_cor <- cor(data, method="spearman", use="complete.obs") ## spearman

write.csv(data_cor, "output_data/manuscript/11_ffm_cor_csci.csv")

data <- read.csv("output_data/00_asci_delta_formatted_median_Nov2021.csv")
head(data)

data <- select(data, DS_Dur_WS:Wet_Tim)
str(data)
?cor
data_cor <- cor(data, method="spearman", use="complete.obs") ## spearman

write.csv(data_cor, "output_data/manuscript/11_ffm_cor_asci.csv")


# relative importance and FFM labels --------------------------------------
getwd()
out.dir <- "output_data/manuscript/figures/"
### CSCI 
gbm_fin_RI_csci <- read.csv("models/07_rel_imp_csci_labels.csv")
gbm_fin_RI_asci <- read.csv("models/07_rel_imp_asci_labels.csv")

gbm_fin_RI_csci <- gbm_fin_RI_csci %>%
  mutate(Index = "CSCI")

gbm_fin_RI_asci <- gbm_fin_RI_asci %>%
  mutate(Index = "ASCI")

gbm_fin_RI <- rbind(gbm_fin_RI_csci, gbm_fin_RI_asci)

gbm_fin_RI

# labels <- read.csv("Data/ffm_names.csv")
# labels <- labels[1:24, ]
# labels <- labels %>% rename(var = Flow.Metric.Code)
# labels[25, 1] <- "Magnitude of largest annual storm"
# labels[25, 2] <- "Q99"
# labels[25, 3] <- "Peak Flow"
# labels
# 
# gbm_fin_RI <- left_join(gbm_fin_RI, labels, by ="var")

c1 <- ggplot(data=gbm_fin_RI, aes(x=reorder(var,-rel.inf), y=rel.inf, fill = Flow.Component)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))+
  # scale_x_continuous(limits = c(0, 35)) +
  facet_wrap(~Index) +
  labs(title = "",
       x = "",
       y = "Relative Importance (%)") #+ theme_bw(base_size = 15)
c1

out.filename <- paste0(out.dir,"11_rel_imp_csci_asci_bar_plot_Nov2021.jpg")
ggsave(c1, file = out.filename, dpi=300, height=4, width=6)

csci_RI <- gbm_fin_RI %>%
  rename(CSCI = rel.inf) %>%
  select(-X)

## ASCI

gbm_fin_RI <- read.csv("models/07_rel_imp_asci_labels.csv")


a1 <- ggplot(data=gbm_fin_RI, aes(x=reorder(Flow.Metric.Name,-rel.inf), y=rel.inf, fill = Flow.Component)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=8), axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))+
  # scale_x_continuous(limits = c(0, 35)) +
  labs(title = "ASCI",
       x = "Flow Metric",
       y = "Relative Importance (%)") #+ theme_bw(base_size = 15)

out.filename <- paste0(out.dir,"11_rel_imp_asci_bar_plot.jpg")
ggsave(a1, file = out.filename, dpi=300, height=4, width=6)

asci_RI <- gbm_fin_RI %>%
  rename(ASCI = rel.inf) %>%
  select(var, ASCI)

all_rf <- merge(asci_RI, csci_RI, by = "var")
head(all_rf)

all_rf <- all_rf %>%
  select(var, Flow.Metric.Name, Flow.Component, CSCI, ASCI)

write.csv(all_rf, "output_data/manuscript/11_relative_imp_table.csv")

# comparison analysis -----------------------------------------------------


delta <- read.csv("output_data/manuscript/11_delta-H_all_years_subbasins.csv")
head(delta)

## filter to wet bfl dur

delta <- delta %>%
  filter(flow_metric ==  "Wet_BFL_Dur")
  
range(na.omit(delta$deltaH_cur_ref))
#-134  167
134 + 167
301
mean(na.omit(delta$deltaH_cur_ref))
## 33.19919

# cons delta limit
# -12.63 to 6.64
12.63+6.64
19.27

# liberal delta limited
# -46.48 to 71.79
46.48 + 71.79
118.27

#min 19.27
# max 118.27
(19.27+118.27)/2
#69.77
118.27 - 19.27
# 99
(99/69.77)
(99/69.77)*100
# 141.8948 - relative percent change


# ## difference in combination delta h limits of entire range of delta h
118.27 - 19.27 # sum
99/301*100




# final combinations ------------------------------------------------------


thresholdsx <- thresholds %>%
  pivot_wider(names_from = Type, values_from = DeltaH) %>%
  mutate(codeH = paste(Bio_endpoint, "_", Hydro_Metric, sep="")) %>%
  filter(codeH %in% c("H_ASCI_Q99", "H_ASCI_DS_Mag_50", "H_ASCI_Wet_BFL_Mag_50",
                      "CSCI_Q99", "CSCI_Wet_BFL_Dur",  "CSCI_SP_Dur")) %>%
  select(-codeH)

write.csv(thresholdsx, "output_data/manuscript/11_delta_h_thresholds_all.csv")

thresholdsx <- thresholdsx %>%
  mutate(codeP = paste(Bio_endpoint,Bio_threshold,Threshold, sep="_")) %>%
  filter(codeP %in% c("CSCI_0.92_Threshold25", "H_ASCI_0.94_Threshold75")) %>%
 
  ungroup() %>%
  select(-codeP,-Bio_endpoint)

write.csv(thresholdsx, "output_data/manuscript/11_delta_h_thresholds_final.csv")


# flow ecology curves -----------------------------------------------------
out.dir <- "output_data/manuscript/figures/"
## upload s curve data 
## negative delta h 
neg_asci <- read.csv("output_data/01a_asci_all_data_neg_logR_metrics_figures_April2021.csv")
head(neg_asci)
sum(is.na(neg_asci))

## scale data
neg_asci <- neg_asci %>%
  group_by(comb_code) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  filter(biol.endpoints == "H_ASCI") 
  

hist(neg_asci$PredictedProbability)
hist(neg_asci$PredictedProbabilityScaled)

## positive delta h 
pos_asci <- read.csv("output_data/01a_asci_all_data_pos_logR_metrics_figures_April2021.csv")
pos_asci

# pos_asci$thresholds <- as.character(pos_asci$thresholds)
# ggplot(subset(pos_asci,hydro.endpoints == "DS_Mag_50") , aes(x=hydro.threshold, y=PredictedProbabilityScaled, color=thresholds))+
#   # geom_point(size=1)+
#   geom_path()+#stat_summary(fun.y="mean", geom="line")+
#   facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
#   geom_vline(xintercept=0, linetype="dashed")+
#   geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)

## scaling changes plots from 0.94 most conservation, to 0.75 most conservative

sum(is.na(pos_asci))
pos_asci <- pos_asci %>%
  group_by(comb_code) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  filter(biol.endpoints == "H_ASCI")

unique(pos_asci$hydro.endpoints)
## full names for labels
labels <- read.csv("Data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(hydro.endpoints = Flow.Metric.Code)
labels[25, 1] <- "Magnitude of largest annual storm"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow"
labels

pos_asci <- left_join(pos_asci, labels, by ="hydro.endpoints")
neg_asci <- left_join(neg_asci, labels, by ="hydro.endpoints")
head(pos_asci)
head(neg_asci)
unique(neg_asci$hydro.endpoints)
all_asci <- rbind(pos_asci, neg_asci)
asci_metrics <- c("Q99", "SP_Dur", "DS_Mag_50")

## subset to only important metrics
all_asci_sub <- subset(all_asci, hydro.endpoints %in% asci_metrics)
head(all_asci_sub)

all_asci_sub <- all_asci_sub %>%
  mutate(Thresholds = as.character(thresholds))
unique(all_asci_sub$hydro.endpoints)

  metric.labs <-  c("Magnitude of largest annual storm", "Dry-season median baseflow", "Spring Duration")
  names(metric.labs) <- c("Q99", "DS_Mag_50", "SP_Dur")

 type.labs <-  c("", "")
  names(type.labs) <- c("Negative", "Positive")


 unique(all_asci_sub$Flow.Metric.Name) 
 
 test2 <- all_asci_sub %>%
   ungroup() %>%
   filter(hydro.endpoints == "Q99", Type == "Positive") %>%
   select(hydro.endpoints, thresholds, PredictedProbabilityScaled, hydro.threshold, hydro,-X) %>%
   pivot_wider(names_from = "thresholds", values_from = "PredictedProbabilityScaled")
   test2
   
   testq <- all_asci_sub %>%
     filter(hydro.endpoints == "Q99", Type =="Negative", thresholds == 0.86)
   
   names(all_asci_sub)
   str(all_asci_sub)
   ggplot(testq, aes(x=hydro.threshold, y=PredictedProbabilityScaled, color=Thresholds))+
     geom_path()

q1 <- ggplot(subset(all_asci_sub,hydro.endpoints == "Q99" ), aes(x=hydro.threshold, y=PredictedProbabilityScaled, color=Thresholds))+
  geom_path()+
  # facet_grid(rows=vars(hydro.endpoints), cols = vars(Type), labeller = labeller(hydro.endpoints = metric.labs, Type = type.labs, scales = "free_x")) +
  facet_wrap(~Type, scales = "free_x") +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  scale_y_continuous(limits=c(0,1))+
  theme_minimal()+
  theme(text = element_text(size=15)) +
  labs(title = "Magnitude of largest annual storm",
       x = "Delta H (CFS)",
       y = "Probability of Good ASCI") #+ theme_bw(base_size = 15)
q1
out.filename <- paste0(out.dir,"11_Q99_asci.jpg")
ggsave(q1, file = out.filename, dpi=300, height=4, width=6)

q2 <- ggplot(subset(all_asci_sub,hydro.endpoints == "DS_Mag_50" ), aes(x=hydro.threshold, y=PredictedProbabilityScaled, color=Thresholds))+
  geom_path()+
  # facet_grid(rows=vars(hydro.endpoints), cols = vars(Type), labeller = labeller(hydro.endpoints = metric.labs, Type = type.labs, scales = "free_x")) +
  facet_wrap(~Type, scales = "free_x") +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  scale_y_continuous(limits=c(0,1))+
  theme_minimal()+
  theme(text = element_text(size=15)) +
  labs(title = "Dry-season median baseflow",
       x = "Delta H (CFS)",
       y = "Probability of Good ASCI") #+ theme_bw(base_size = 15)
q2
out.filename <- paste0(out.dir,"11_DS_Mag_50_asci.jpg")
ggsave(q2, file = out.filename, dpi=300, height=4, width=6)

q3 <- ggplot(subset(all_asci_sub,hydro.endpoints == "SP_Dur" ), aes(x=hydro.threshold, y=PredictedProbabilityScaled, color=Thresholds))+
  geom_path()+
  # facet_grid(rows=vars(hydro.endpoints), cols = vars(Type), labeller = labeller(hydro.endpoints = metric.labs, Type = type.labs, scales = "free_x")) +
  facet_wrap(~Type, scales = "free_x") +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  scale_y_continuous(limits=c(0,1))+
  theme_minimal()+
  theme(text = element_text(size=15)) +
  labs(title = "Spring Recession Duration",
       x = "Delta H (Days)",
       y = "Probability of Good ASCI") #+ theme_bw(base_size = 15)
q3
out.filename <- paste0(out.dir,"11_SP_Dur_asci.jpg")
ggsave(q3, file = out.filename, dpi=300, height=4, width=6)

###### CSCI

neg_csci <- read.csv("output_data/01_csci_neg_logR_metrics_figures_April2021.csv")
head(neg_csci)
sum(is.na(neg_csci))

## scale data
neg_csci <- neg_csci %>%
  group_by(comb_code) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) 


hist(neg_csci$PredictedProbability)
hist(neg_csci$PredictedProbabilityScaled)

## positive delta h 
pos_csci <- read.csv("output_data/01_csci_pos_logR_metrics_figures_April2021.csv")
pos_csci
sum(is.na(pos_csci))
pos_csci <- pos_csci %>%
  group_by(comb_code) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) 

pos_csci <- left_join(pos_csci, labels, by ="hydro.endpoints")
neg_csci <- left_join(neg_csci, labels, by ="hydro.endpoints")
head(pos_csci)
head(neg_csci)
unique(neg_csci$hydro.endpoints)
all_csci <- rbind(pos_csci, neg_csci)
csci_metrics <- c("Q99", "SP_Tim", "DS_Mag_50")

## subset to only important metrics
all_csci_sub <- subset(all_csci, hydro.endpoints %in% csci_metrics)
head(all_csci_sub)

all_csci_sub <- all_csci_sub %>%
  mutate(Thresholds = as.character(thresholds))
unique(all_csci_sub$hydro.endpoints)

# metric.labs <-  c("Magnitude of largest annual storm", "Dry-season median baseflow", "Wet-season median baseflow")
# names(metric.labs) <- c("Q99", "DS_Mag_50", "Wet_BFL_Mag_50")
# 
# type.labs <-  c("", "")
# names(type.labs) <- c("Negative", "Positive")
# 

unique(all_csci_sub$Flow.Metric.Name) 

# test2 <- all_csci_sub %>%
#   filter(hydro.endpoints == "Q99", Type == "Negative", !thresholds == 0.86)
# test2

q1 <- ggplot(subset(all_csci_sub,hydro.endpoints == "Q99" ), aes(x=hydro.threshold, y=PredictedProbabilityScaled, color=Thresholds))+
  geom_path()+
  # facet_grid(rows=vars(hydro.endpoints), cols = vars(Type), labeller = labeller(hydro.endpoints = metric.labs, Type = type.labs, scales = "free_x")) +
  facet_wrap(~Type, scales = "free_x") +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  scale_y_continuous(limits=c(0,1))+
  theme_minimal()+
  theme(text = element_text(size=15)) +
  labs(title = "Magnitude of largest annual storm",
       x = "Delta H (CFS)",
       y = "Probability of Good CSCI") #+ theme_bw(base_size = 15)
q1
out.filename <- paste0(out.dir,"11_Q99_csci.jpg")
ggsave(q1, file = out.filename, dpi=300, height=4, width=6)

q2 <- ggplot(subset(all_csci_sub,hydro.endpoints == "SP_Tim" ), aes(x=hydro.threshold, y=PredictedProbabilityScaled, color=Thresholds))+
  geom_path()+
  # facet_grid(rows=vars(hydro.endpoints), cols = vars(Type), labeller = labeller(hydro.endpoints = metric.labs, Type = type.labs, scales = "free_x")) +
  facet_wrap(~Type, scales = "free_x") +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  scale_y_continuous(limits=c(0,1))+
  theme_minimal()+
  theme(text = element_text(size=15)) +
  labs(title = "Spring Recession Timing",
       x = "Delta H (Days)",
       y = "Probability of Good CSCI") #+ theme_bw(base_size = 15)
q2
out.filename <- paste0(out.dir,"11_SP_Tim_csci.jpg")
ggsave(q2, file = out.filename, dpi=300, height=4, width=6)

q3 <- ggplot(subset(all_csci_sub,hydro.endpoints == "DS_Mag_50" ), aes(x=hydro.threshold, y=PredictedProbabilityScaled, color=Thresholds))+
  geom_path()+
  # facet_grid(rows=vars(hydro.endpoints), cols = vars(Type), labeller = labeller(hydro.endpoints = metric.labs, Type = type.labs, scales = "free_x")) +
  facet_wrap(~Type, scales = "free_x") +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  scale_y_continuous(limits=c(0,1))+
  theme_minimal()+
  theme(text = element_text(size=15)) +
  labs(title = "Dry-season Magnitude",
       x = "Delta H (CFS)",
       y = "Probability of Good CSCI") #+ theme_bw(base_size = 15)
q3
out.filename <- paste0(out.dir,"11_DS_Mag_50_csci.jpg")
ggsave(q3, file = out.filename, dpi=300, height=4, width=6)


# All ffm flow eco curves -------------------------------------------------

head(all_csci)
head(all_asci)

### n per ffm
 nsites <- all_csci %>%
   group_by()

all_csci <- all_csci %>%
  mutate(Thresholds = as.character(thresholds)) %>%
  filter(thresholds == 0.79)

HydroEnds <- unique(all_asci$hydro.endpoints)
HydroEnds
m=2

for(m in 1:length(HydroEnds)) {
  
  main.title <- all_csci %>%
    ungroup() %>%
    filter(hydro.endpoints == paste(HydroEnds[m])) %>%
    select(Flow.Metric.Name) %>%
    distinct(Flow.Metric.Name)
  
  
  q3 <- ggplot(subset(all_csci,hydro.endpoints == paste(HydroEnds[m]) ), aes(x=hydro.threshold, y=PredictedProbabilityScaled, color=Thresholds))+
    geom_path()+
    # facet_grid(rows=vars(hydro.endpoints), cols = vars(Type), labeller = labeller(hydro.endpoints = metric.labs, Type = type.labs, scales = "free_x")) +
    facet_wrap(~Type, scales = "free_x") +
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) +
    scale_y_continuous(limits=c(0,1))+
    theme_minimal()+
    theme(text = element_text(size=15)) +
    labs(title = paste(main.title),
         x = "Delta H",
         y = "Probability of Good CSCI") #+ theme_bw(base_size = 15)
  q3
  out.filename <- paste0(out.dir,"11_other_csci_", paste(HydroEnds[m]), "updated_Nov2021.jpg")
  ggsave(q3, file = out.filename, dpi=300, height=4, width=6)
  
  
}

all_asci <- all_asci %>%
  mutate(Thresholds = as.character(thresholds)) %>%
  filter(thresholds == 0.86)

HydroEnds <- unique(all_asci$hydro.endpoints)
HydroEnds[m]
m=2

for(m in 1:length(HydroEnds)) {
  
  main.title <- all_asci %>%
    ungroup() %>%
    filter(hydro.endpoints == paste(HydroEnds[m])) %>%
    select(Flow.Metric.Name) %>%
    distinct(Flow.Metric.Name)
  
  
  q3 <- ggplot(subset(all_asci,hydro.endpoints == paste(HydroEnds[m]) ), aes(x=hydro.threshold, y=PredictedProbabilityScaled, color=Thresholds))+
    geom_path()+
    # facet_grid(rows=vars(hydro.endpoints), cols = vars(Type), labeller = labeller(hydro.endpoints = metric.labs, Type = type.labs, scales = "free_x")) +
    facet_wrap(~Type, scales = "free_x") +
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) +
    scale_y_continuous(limits=c(0,1))+
    theme_minimal()+
    theme(text = element_text(size=15)) +
    labs(title = paste(main.title),
         x = "Delta H",
         y = "Probability of Good ASCI") #+ theme_bw(base_size = 15)
  q3
  out.filename <- paste0(out.dir,"11_other_asci_", paste(HydroEnds[m]), "updated_Nov2021.jpg")
  ggsave(q3, file = out.filename, dpi=300, height=4, width=6)
  
  
}


# number of sites ---------------------------------------------------------
delta <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/DeltaH_FINAL/deltaH_summary_badyrs_nonzero.csv")
head(delta)

delta <- delta %>%
  select(site) %>%
  rename(StationCode = site) %>%
  distinct()

dim(delta) ## 516

asci<-read.csv("output_data/00_asci_delta_formatted_median_Nov2021.csv")
csci <- read.csv("output_data/00_csci_delta_formatted_median_updated_Nov2021.csv")
head(csci)
head(asci)

asci <- asci %>%
  select(StationCode)

csci <- csci %>%
  select(stationcode) %>%
  rename(StationCode = stationcode)

all_sites <- rbind(asci, csci)

dim(all_sites) # 686

length(unique(all_sites$StationCode)) ## 466

## missing sites, delta H vs bio
## asci
sum(delta$StationCode %in% asci$StationCode) ## 351
mis <- which(delta$StationCode %in% asci$StationCode)

missing_asci <- delta$StationCode[-mis]
missing_asci

sum(delta$StationCode %in% csci$StationCode) ## 397
mis <- which(delta$StationCode %in% csci$StationCode)

missing_csci <- delta$StationCode[-mis]
missing_csci
getwd()
write.csv(missing_asci, "11_missing_asci_sites_165.csv")
write.csv(missing_asci, "11_missing_csci_sites_119.csv")

# alteration maps ---------------------------------------------------------


## use Kris code for maps - need to link to server
## packages
# install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
# install.packages("viridis")
# head(AltIntense)
library(spDataLarge)
library(viridis)
library(readxl)
library(sf)
library(ggsn)
library(ggmap)
library(mapview)
library(spData)      
citation("sf")
citation("ggmap")
library(geosphere)
library(rgeos)
library(tidyverse)

#UPDATE
getwd()
#set output directory
out.dir <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/SOC_tier2/SOC_tier_2/output_data/manuscript/figures/"
#out.dir <- paste0(level3.dir, "Suitability_Maps/", "All_species_suit_class_wide_option2_strict_prob_shorter_time/")

#read in information on subbasin and New_Name
basin_comid_lookup <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/SpatialData/v13_pourpoints_NHD_comids.csv") 

#read in shapefiles subbasins and reaches
#subbasin polygon shapefile
basins <- st_read("Data/subbasin_boundaries_forSCCWRP.shp", quiet = T)
basins
#reach polylines
reaches <- st_read('Data/reaches_forSCCWRP.shp', quiet = T)
reaches
plot(reaches)
#lookuptable to convert subbasin codes for model output subbasin names - doesn't exist!!!
subbasin_lookup <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/Old_Runs/191220_Interim_Calibration/site_name_lookupletternumbers.csv")
subbasin_lookup
#convert basin orig name to outputfile name (model subbasin name)
new.subbasinname <- basin_comid_lookup$Subbasin

for(z in 1:length(subbasin_lookup$Letter)){
  new.subbasinname <- gsub(subbasin_lookup$Letter[z], subbasin_lookup$Number[z], new.subbasinname)
}

#find and replace - in new.subbasinname with nothing, make consistent with file name
new.subbasinname <- gsub("-", "", new.subbasinname)
basin_comid_lookup$site <- as.numeric(new.subbasinname)

#join new subbasin name with 
data <- read.csv("output_data/manuscript/11_metric_suitability_tally_condensed_all_sites_current_June2021.csv")
head(data)

suit_data2 <- data %>% 
  inner_join(basin_comid_lookup, by = c('site')) %>% 
  select(c(names(data), Subbasin)) %>% 
  rename(New_Name = Subbasin) %>%
  select(-Unaltered, -X)# %>%
# pivot_wider(names_from = Threshold, values_from = Altered)

##  upload ffm names
labels <- read.csv("Data/ffm_names.csv")
labels <- labels[1:24, ]
labels
labels <- labels %>% rename(Hydro_Metric = Flow.Metric.Code)
labels[25, 1] <- "Magnitude of largest annual storm"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow"
labels

suit_data2 <- left_join(suit_data2, labels, by ="Hydro_Metric")
suit_data2

#names
cols <- names(suit_data2)
# col.names <- c("Probability_25", "overall.altered.2metric", "overall.altered.3metric")
cols
suit_data2$Bio_endpoint <- gsub("H_", "", suit_data2$Bio_endpoint)
suit_data2$Biol
i=7
rm(metric.threshold)

indices <- c("ASCI", "CSCI")
z = "ASCI"

for(z in indices){
  #subset either csci or asci
  subset.index <- suit_data2[suit_data2$Biol == z,]
  subset.index
  metrics <- na.omit(unique(subset.index$Hydro_Metric))
       metrics
  # h = metrics[1]
  ## loop through metrics
  for(h in metrics){ 
    
    #subset based on hydro metric
    # h = "DS_Mag_50"
    subset.index.metric <- subset.index[subset.index$Hydro_Metric ==  h,]
    #bio value
    subset.index.metric
    metric <- paste(subset.index.metric$Flow.Metric.Name)[1]
    metric
    
    #merge with basins
    subset.join <- subset.index.metric %>% 
      full_join(basins, by = c('New_Name'))
    
    
    
    subset.join <- na.omit(subset.join)
    # head(subset.join)
   
    title <- paste0(z, ": ", metric)
    # New facet label names for probability variable
    prob.labs <-  c("Prob: 0.25", "Prob: 0.5", "Prob: 0.75")
    names(prob.labs) <- c("Threshold25", "Threshold50", "Threshold75")
    
    # New facet label names for bio variable
    
    if(z == "CSCI") {
      bio.labs <-  c("Bio: 0.63", "Bio: 0.79", "Bio: 0.92")
      names(bio.labs) <- c("0.63", "0.79", "0.92")
    } else {
      bio.labs <-  c("Bio: 0.75", "Bio: 0.86", "Bio: 0.94")
      names(bio.labs) <- c("0.75", "0.86", "0.94")
    }
    
    head(subset.join)
    
    #plot
    #Set up base map 
    study <- ggplot(basins) + 
      geom_sf(color = "#969696", fill="white", size = 0.1) +
      labs(title=title,  x ="", y = "")  + 
      theme(panel.background = element_rect(fill = "white"),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_line(color = "white", size = 0.1),
            plot.title = element_text(size=20)) 
    
    study
    # head(subset.join)
    #synthesis map
    syn.plot <- study + geom_sf(data = subset.join, aes(fill=Altered, geometry = geometry), size = 0.2) +
      scale_fill_gradientn(colours=rev(viridis(6)), name = "Alteration (%)") +
      geom_sf(data = reaches, color = "#67a9cf", size = 0.1) +
      facet_grid(cols=vars(Threshold), rows = vars(Bio_threshold), labeller = labeller(Threshold = prob.labs, Bio_threshold = bio.labs))
    
    syn.plot
    ?scale_fill_gradientn
    #print
    # print(syn.plot)
    # indices[z]
    #write plot
    
    # out.filename
    out.filename <- paste0(out.dir, z,"_", h, "_percentage_alteration_map.jpg")
    ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
    
    # }
    
  }
  
}



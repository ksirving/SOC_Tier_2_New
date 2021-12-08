## finding the best threshold combination

library(ggplot2)
library(dplyr)
library(tidyverse)



# Format data -------------------------------------------------------------


#### over time

delta_dfx <- read.csv( "output_data/Manuscript/08_alteration_by_year_site_all_sites.csv")
names(delta_dfx)
head(delta_dfx)

delta_dfx <- delta_dfx %>%
  select(-scenario)

delta_dfx <- na.omit(delta_dfx)

## get number and percentage of sites per year altered and combination code

Year_Tally <- delta_dfx %>%
  select(site, year, Hydro_Metric, metric, Biol, Bio_threshold, Threshold, Alteration_Current) %>%
  mutate(CombCode = paste0(Bio_threshold, "_", Threshold )) %>%
  group_by(year, Hydro_Metric,Biol, Bio_threshold, Threshold, CombCode) %>%
  count(Alteration_Current) %>%
  mutate(Percentage = n/sum(n)*100) %>%  select(-n) %>%
  pivot_wider(names_from = Alteration_Current, values_from = Percentage)

Year_Tally[is.na(Year_Tally)] <- 0

head(Year_Tally)

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("CSCI","ASCI"))



# Figures -----------------------------------------------------------------

getwd()
out.dir <- "output_data/Manuscript/Figures/"


asci_metrics <- c("Q99", "SP_Dur", "DS_Dur_WS")
csci_metrics <-c("Q99", "SP_Tim","DS_Dur_WS" )

# format thresholds
Year_Tally$Bio_threshold <- as.character(Year_Tally$Bio_threshold)

Tally <- Year_Tally %>%
  group_by(CombCode, Hydro_Metric, Biol) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) 

write.csv(Tally, "output_data/Manuscript/09_subbasin_alteration_median_mean.csv")

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("CSCI"), Hydro_Metric == "SP_Tim")

head(Tally0)


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

out.filename <- paste0(out.dir,"CSCI_SP_Tim_Alt_over_time.jpg")
ggsave(csci1, file = out.filename, dpi=300, height=4, width=6)

## find metrics within limits

Tally0x <- Tally0 %>%
  group_by(CombCode, Hydro_Metric) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
filter(AlteredMean > 40 & AlteredMean < 60)
Tally0x
unique(Tally0x$CombCode)


# [1] "0.63_Threshold75" "0.79_Threshold50" "0.79_Threshold75"
# [4] "0.92_Threshold25" "0.92_Threshold50" "0.92_Threshold75" 

Tally0 <- Year_Tally %>%
  filter(Biol %in% c("CSCI"), Hydro_Metric == "DS_Dur_WS")

head(Tally0)

csci2 <- ggplot(data=Tally0, aes(x = year, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 1994, xmax = 2018,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Dry Season Duration", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_discrete(name  ="CSCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))

csci2

out.filename <- paste0(out.dir,"CSCI_DS_Dur_WS_Alt_over_time.jpg")
ggsave(csci2, file = out.filename, dpi=300, height=4, width=6)


## find metrics within limits

Tally0x <- Tally0 %>%
  group_by(CombCode) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
filter(AlteredMean > 40 & AlteredMean < 60)
Tally0x
unique(Tally0x$CombCode) 

# 0.92_Threshold25 ## other options in more lenient category

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
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
filter(AlteredMean > 25 & AlteredMean < 75)
Tally0x
unique(Tally0x$CombCode) 


# "0.92_Threshold25" "0.92_Threshold50"

## CSCI - all
# "0.63_Threshold75" "0.79_Threshold50" "0.79_Threshold75" "0.92_Threshold25" "0.92_Threshold50" "0.92_Threshold75" 
#  0.92_Threshold25
# "0.92_Threshold25" "0.92_Threshold50"

## winner is - 0.92_Threshold25 



asci_metrics <- c("Q99", "SP_Dur", "DS_Dur_WS")

Tally1 <- Year_Tally %>%
  filter(Biol %in% c("ASCI"), Hydro_Metric == "DS_Dur_WS")

head(Tally1)

asci1 <- ggplot(data=Tally1, aes(x = year, y= Altered, group = CombCode, color = Bio_threshold, linetype = Threshold )) +
  annotate("rect", ymin = 25, ymax = 75, xmin = 1994, xmax = 2018,
           alpha = .2) +
  geom_smooth(method = "loess", se = FALSE ) +
  labs(title = "Dry Season Duration", x = "Year", y = "Altered Subbasins (%)") + 
  theme(text = element_text(size=10)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_discrete(name  ="ASCI Theshold") +
  scale_linetype_discrete(name  ="Probability Threshold",
                          breaks=c("Threshold25", "Threshold50", "Threshold75"),
                          labels=c("0.25", "0.50", "0.75"))
asci1
out.filename <- paste0(out.dir,"ASCI_DS_Dur_WS_Alt_over_time.jpg")
ggsave(asci1, file = out.filename, dpi=300, height=4, width=6)

## find metrics within limits

Tally1x <- Tally1 %>%
  group_by(CombCode) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
  filter(AlteredMean > 40 & AlteredMean < 60)
Tally1x
unique(Tally1x$CombCode) ## "0.75_Threshold75" "0.86_Threshold75" "0.94_Threshold50"

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

out.filename <- paste0(out.dir,"ASCI_SP_Dur_Alt_over_time.jpg")
ggsave(asci2, file = out.filename, dpi=300, height=4, width=6)

## find metrics within limits

Tally1x <- Tally1 %>%
  group_by(CombCode) %>%
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
  filter(AlteredMean > 25 & AlteredMean < 75)
Tally1x
unique(Tally1x$CombCode) 

# [1] "0.75_Threshold25" "0.75_Threshold50" "0.75_Threshold75"
# [4] "0.86_Threshold25" "0.86_Threshold50" "0.86_Threshold75"
# [7] "0.94_Threshold25" "0.94_Threshold50"

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
  summarise(AlteredMean = mean(Altered), AlteredMedian = median(Altered)) %>%
filter(AlteredMean > 25 & AlteredMean < 75)
Tally1x
unique(Tally1x$CombCode) 
## "0.94_Threshold50" "0.94_Threshold75"

## winner is - "0.94_Threshold50"



# Overall Figures ----------------------------------------------------------
library(ggplot2)
# library(purrr)
library(dplyr)
library(tidyverse)

out.dir <- "output_data/Manuscript/Figures/"

## full names for labels
labels <- read.csv("Data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(hydro.endpoints = Flow.Metric.Code)
labels[25, 1] <- "Magnitude of largest annual storm"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow"
labels



# data ASCI ---------------------------------------------------------------

## upload data
all_asci <- read.csv("output_data/01_h_asci_neg_pos_logR_metrics_figures_April2021.csv")

## scale probability
all_asci <- all_asci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) 

all_asci <- left_join(all_asci, labels, by ="hydro.endpoints")

head(all_asci)


# data CSCI ---------------------------------------------------------------

## upload data
all_csci <- read.csv("output_data/01_CSCI_neg_pos_logR_metrics_figures_April2021.csv")

## scale probability
all_csci <- all_csci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) 

all_csci <- left_join(all_csci, labels, by ="hydro.endpoints")

head(all_csci)

## final combinations

## Thresholds
# CSCI - Bio_threshold = 0.92, Threshold = Threshold25
# ASCI - Bio_threshold = 0.94, Threshold = Threshold50

asci_metrics <- c("Q99", "SP_Dur", "DS_Dur_WS")
csci_metrics <-c("Q99", "SP_Tim","DS_Dur_WS" )


all_csci <- all_csci %>%
  filter(biol.endpoints %in% c("CSCI"), thresholds == 0.92, hydro.endpoints %in% csci_metrics ) %>%
  mutate(Thresholds = as.character(thresholds)) 

## subset data and put in order for geom.path
all_cscix <- all_csci[order(all_csci$PredictedProbabilityScaled, all_csci$hydro),]

head(csci)
?facet_grid
q3 <- ggplot(all_cscix, aes(x=hydro, y=PredictedProbabilityScaled))+
  geom_path()+
  facet_grid(cols=vars(Type), rows = vars(Flow.Metric.Name), scales = "free_x")+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  scale_y_continuous(limits=c(0,1))+
  theme_minimal()+
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5)) +
  labs( x = "Delta H",
       y = "Probability of Good CSCI") #+ theme_bw(base_size = 15)
q3
out.filename <- paste0(out.dir,"03_csci_", paste(HydroEnds[m]), "_0.79.jpg")
ggsave(q3, file = out.filename, dpi=300, height=4, width=6)


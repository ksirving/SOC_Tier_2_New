### Relating bio relevant flow alteration with observed and predicted CSCI/ASCI scores
# Management recommendations for Level 2 analysis

###############################################################################
# Install packages - only need to install once
#install.packages("ggsn")
#install.packages("ggmap")
#install.packages("mapview")
#install.packages("geosphere")
#install.packages("rgeos")
#install.packages("ggspatial")
# To install spDataLarge
#devtools::install_github("robinlovelace/geocompr")
#install.packages("spDataLarge")
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")

#load libaries
library(spDataLarge)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(ggsn)
library(ggmap)
library(ggspatial)
library(mapview)
library(spData)      
library(geosphere)
library(rgeos)

###############################################################################
## Load in data

#read in bio-relevant flow alteration for CSCI and ASCI
bio.flow.alt <- read.csv("output_data/11_SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary_Current.csv")

# read csvs with observed and predicted CSCI/ASCI score [will use the scores and updated the bio relevant flow alteration]
observed.data <- read.csv("Data/SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary_Observed.csv")
predicted.data <- read.csv("Data/CSCI_ASCI_hydro_alteration_predicted2.csv")

# read in predicted CSCI and ASCI
pred.csci.asci <- read.csv("L:/San Juan WQIP_KTQ/Data/SpatialData/CSCI_ASCI_Scores_COMID_full_rf_results_Heili.csv")


# read in subbasin COMID lookup table
comid.lookup <- read.csv("Data/SOC_FESS_Subbasin_NHD.COMID_Lookup_Table.csv")

# read in list of modeled subbasins
modeled.subbasins <- read.csv("Data/Agg_Boundaries_v14_modeledsites.csv")

# read in information on subbasin and New_Name
basin_comid_lookup <- read.csv("Data/v13_pourpoints_NHD_comids.csv") 

# read in shapefiles subbasins and reaches
# subbasin polygon shapefile
basins <- st_read("Data/Agg_Boundaries_v14.shp", quiet = T)
# reach polylines
reaches <- st_read('Data/reaches_forSCCWRP.shp', quiet = T)

######################################################################################################
## Develop management recommendation categories
# Data checking and prep

# filter comid lookup with modeled.subbasins, check to see if it's the same length
comid.lookup2 <- comid.lookup[which(comid.lookup$Subbasin %in% modeled.subbasins$New_Name),]
length(comid.lookup2$Subbasin)
length(modeled.subbasins$New_Name)

# check to see if subbasins are missing from predicted.data
`%notin%` <- Negate(`%in%`)
comid.missing <- comid.lookup2[which(comid.lookup2$Subbasin %notin% predicted.data$New_Name),]
#if length of comid.missing is 0, you aren't missing any predicted data
length(comid.missing$Subbasin)


## Join bio-relevant flow alteration CSCI and ASCI into observed and predicted df

# observed join with bio-relevant flow alteration
observed.data.join <- observed.data %>%
  left_join(bio.flow.alt, by = c("Subbasin" =  "New_Name"))
#update the new bio relevant alteration (hydro.alteration.CSCI and hydro.alteration.ASCI) into old columns (Hydro.Alteration.CSCI and Hydro.Alteration.ASCI)
observed.data$Hydro.Alteration.CSCI <- observed.data.join$hydro.alteration.CSCI
observed.data$Hydro.Alteration.ASCI <- observed.data.join$hydro.alteration.ASCI

# predicted join with bio-relevant flow alteration
predicted.data.join <- predicted.data %>%
  left_join(bio.flow.alt, by = c("New_Name"))
#update the new bio relevant alteration (hydro.alteration.CSCI and hydro.alteration.ASCI) into old columns (Hydro.Alteration.CSCI and Hydro.Alteration.ASCI)
predicted.data$Hydro.Alteration.CSCI <- predicted.data.join$hydro.alteration.CSCI
predicted.data$Hydro.Alteration.ASCI <- predicted.data.join$hydro.alteration.ASCI

## Update stream characterization CSCI and ASCI based on rules:

# Make all columns NA to be updated
observed.data$Stream.Characterization.CSCI <- NA
observed.data$Stream.Characterization.ASCI <- NA 
predicted.data$Stream.Characterization.CSCI <- NA
predicted.data$Stream.Characterization.ASCI <- NA 

# 1. if bio likely or very likely altered and hydro alt is likely altered, then Prioritized for Flow Management

# rule1 for observed data:
# CSCI
observed.data$Stream.Characterization.CSCI[observed.data$Biological.Condition.CSCI == "Likely altered" & observed.data$Hydro.Alteration.CSCI == "Likely Altered"] <- "Prioritized for Flow Management"
observed.data$Stream.Characterization.CSCI[observed.data$Biological.Condition.CSCI == "Very likely altered" & observed.data$Hydro.Alteration.CSCI == "Likely Altered"] <- "Prioritized for Flow Management"
# ASCI
observed.data$Stream.Characterization.ASCI[observed.data$Biological.Condition.ASCI == "Likely altered" & observed.data$Hydro.Alteration.ASCI == "Likely Altered"] <- "Prioritized for Flow Management"
observed.data$Stream.Characterization.ASCI[observed.data$Biological.Condition.ASCI == "Very likely altered" & observed.data$Hydro.Alteration.ASCI == "Likely Altered"] <- "Prioritized for Flow Management"

# rule1 for predicted data:
# CSCI
predicted.data$Stream.Characterization.CSCI[predicted.data$Biological.Condition.CSCI == "Likely altered" & predicted.data$Hydro.Alteration.CSCI == "Likely Altered"] <- "Prioritized for Flow Management"
predicted.data$Stream.Characterization.CSCI[predicted.data$Biological.Condition.CSCI == "Very likely altered" & predicted.data$Hydro.Alteration.CSCI == "Likely Altered"] <- "Prioritized for Flow Management"
# ASCI
predicted.data$Stream.Characterization.ASCI[predicted.data$Biological.Condition.ASCI == "Likely altered" & predicted.data$Hydro.Alteration.ASCI == "Likely Altered"] <- "Prioritized for Flow Management"
predicted.data$Stream.Characterization.ASCI[predicted.data$Biological.Condition.ASCI == "Very likely altered" & predicted.data$Hydro.Alteration.ASCI == "Likely Altered"] <- "Prioritized for Flow Management"

# 2. if likely or very likely altered and hydro alt is likely unaltered, then Prioritized for Separate Stressor Evaluations

# rule2 for observed data:
# CSCI
observed.data$Stream.Characterization.CSCI[observed.data$Biological.Condition.CSCI == "Likely altered" & observed.data$Hydro.Alteration.CSCI == "Likely Unaltered"] <- "Prioritized for Separate Stressor Evaluation"
observed.data$Stream.Characterization.CSCI[observed.data$Biological.Condition.CSCI == "Very likely altered" & observed.data$Hydro.Alteration.CSCI == "Likely Unaltered"] <- "Prioritized for Separate Stressor Evaluation"
# ASCI
observed.data$Stream.Characterization.ASCI[observed.data$Biological.Condition.ASCI == "Likely altered" & observed.data$Hydro.Alteration.ASCI == "Likely Unaltered"] <- "Prioritized for Separate Stressor Evaluation"
observed.data$Stream.Characterization.ASCI[observed.data$Biological.Condition.ASCI == "Very likely altered" & observed.data$Hydro.Alteration.ASCI == "Likely Unaltered"] <- "Prioritized for Separate Stressor Evaluation"

# rule2 for predicted data:
# CSCI
predicted.data$Stream.Characterization.CSCI[predicted.data$Biological.Condition.CSCI == "Likely altered" & predicted.data$Hydro.Alteration.CSCI == "Likely Unaltered"] <- "Prioritized for Separate Stressor Evaluation"
predicted.data$Stream.Characterization.CSCI[predicted.data$Biological.Condition.CSCI == "Very likely altered" & predicted.data$Hydro.Alteration.CSCI == "Likely Unaltered"] <- "Prioritized for Separate Stressor Evaluation"
# ASCI
predicted.data$Stream.Characterization.ASCI[predicted.data$Biological.Condition.ASCI == "Likely altered" & predicted.data$Hydro.Alteration.ASCI == "Likely Unaltered"] <- "Prioritized for Separate Stressor Evaluation"
predicted.data$Stream.Characterization.ASCI[predicted.data$Biological.Condition.ASCI == "Very likely altered" & predicted.data$Hydro.Alteration.ASCI == "Likely Unaltered"] <- "Prioritized for Separate Stressor Evaluation"

# 3. if bio possibly altered or likely intact and hydro unaltered, then Prioritized for Protection

# rule3 for observed data:
# CSCI
observed.data$Stream.Characterization.CSCI[observed.data$Biological.Condition.CSCI == "Possibly altered" & observed.data$Hydro.Alteration.CSCI == "Likely Unaltered"] <- "Prioritized for Protection"
observed.data$Stream.Characterization.CSCI[observed.data$Biological.Condition.CSCI == "Likely intact" & observed.data$Hydro.Alteration.CSCI == "Likely Unaltered"] <- "Prioritized for Protection"
# ASCI
observed.data$Stream.Characterization.ASCI[observed.data$Biological.Condition.ASCI == "Possibly altered" & observed.data$Hydro.Alteration.ASCI == "Likely Unaltered"] <- "Prioritized for Protection"
observed.data$Stream.Characterization.ASCI[observed.data$Biological.Condition.ASCI == "Likely intact" & observed.data$Hydro.Alteration.ASCI == "Likely Unaltered"] <- "Prioritized for Protection"

# rule3 for predicted data:
# CSCI
predicted.data$Stream.Characterization.CSCI[predicted.data$Biological.Condition.CSCI == "Possibly altered" & predicted.data$Hydro.Alteration.CSCI == "Likely Unaltered"] <- "Prioritized for Protection"
predicted.data$Stream.Characterization.CSCI[predicted.data$Biological.Condition.CSCI == "Likely intact" & predicted.data$Hydro.Alteration.CSCI == "Likely Unaltered"] <- "Prioritized for Protection"
# ASCI
predicted.data$Stream.Characterization.ASCI[predicted.data$Biological.Condition.ASCI == "Possibly altered" & predicted.data$Hydro.Alteration.ASCI == "Likely Unaltered"] <- "Prioritized for Protection"
predicted.data$Stream.Characterization.ASCI[predicted.data$Biological.Condition.ASCI == "Likely intact" & predicted.data$Hydro.Alteration.ASCI == "Likely Unaltered"] <- "Prioritized for Protection"

# 4. if bio possibly altered or likely intact and hydro alt is likely altered, then Prioritized for Monitoring

# rule4 for observed data:
# CSCI
observed.data$Stream.Characterization.CSCI[observed.data$Biological.Condition.CSCI == "Possibly altered" & observed.data$Hydro.Alteration.CSCI == "Likely Altered"] <- "Prioritized for Monitoring"
observed.data$Stream.Characterization.CSCI[observed.data$Biological.Condition.CSCI == "Likely intact" & observed.data$Hydro.Alteration.CSCI == "Likely Altered"] <- "Prioritized for Monitoring"
# ASCI
observed.data$Stream.Characterization.ASCI[observed.data$Biological.Condition.ASCI == "Possibly altered" & observed.data$Hydro.Alteration.ASCI == "Likely Altered"] <- "Prioritized for Monitoring"
observed.data$Stream.Characterization.ASCI[observed.data$Biological.Condition.ASCI == "Likely intact" & observed.data$Hydro.Alteration.ASCI == "Likely Altered"] <- "Prioritized for Monitoring"

# rule4 for predicted data:
# CSCI
predicted.data$Stream.Characterization.CSCI[predicted.data$Biological.Condition.CSCI == "Possibly altered" & predicted.data$Hydro.Alteration.CSCI == "Likely Altered"] <- "Prioritized for Monitoring"
predicted.data$Stream.Characterization.CSCI[predicted.data$Biological.Condition.CSCI == "Likely intact" & predicted.data$Hydro.Alteration.CSCI == "Likely Altered"] <- "Prioritized for Monitoring"
# ASCI
predicted.data$Stream.Characterization.ASCI[predicted.data$Biological.Condition.ASCI == "Possibly altered" & predicted.data$Hydro.Alteration.ASCI == "Likely Altered"] <- "Prioritized for Monitoring"
predicted.data$Stream.Characterization.ASCI[predicted.data$Biological.Condition.ASCI == "Likely intact" & predicted.data$Hydro.Alteration.ASCI == "Likely Altered"] <- "Prioritized for Monitoring"


######################################################################################################
## Write updated predicted and observed data to output_data folder

# update in synthesis alteration in observed
observed.data$Synthesis.Alteration <- observed.data.join$synthesis_alteration
write.csv(observed.data, file="output_data/12_SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary_Observed.csv", row.names=FALSE)

# update in synthesis alteration in predicted
predicted.data$Synthesis.Alteration <- predicted.data.join$synthesis_alteration
write.csv(predicted.data, file="output_data/12_CSCI_ASCI_hydro_alteration_predicted.csv", row.names=FALSE)

# summarize predicted categories
pred.summary.cscsi <- predicted.data %>% 
  group_by(Stream.Characterization.CSCI) %>% 
  tally()

pred.summary.ascsi <- predicted.data %>% 
  group_by(Stream.Characterization.ASCI) %>% 
  tally()


######################################################################################
## Create management recommendation maps

# set colors for suitability categories used in maps and legend
colors <- c("#ca0020", "#fdae61", "#ffffbf", "#0571b0", "white")
priority <- c("Prioritized for Flow Management",  "Prioritized for Separate Stressor Evaluation", "Prioritized for Monitoring", "Prioritized for Protection", NA)
# lookup table for legend and maps
lookup <- data.frame(cbind(colors, priority))

# set output directory 
out.dir <- "output_data/Manuscript/Figures/Maps/"

## CSCI Observed maps

# merge observed data with basins for spatial data
subset.join <- observed.data %>% 
  full_join(basins, by = c('New_Name')) 

## Plot observed
# Set up base map 
study <- ggplot(basins) + 
  labs(title="Recommendations using Observed CSCI Data",x ="", y = "")  + 
  geom_sf(color = "lightgrey", fill="white") +
  annotation_scale() +
  annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                         width = unit(.8, "cm")) +
  #labs(x ="", y = "")  + 
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_line(color = "white", size = 0.8)) 
# print study map
study

# subset lookup categories and tables
lookup.sub <- lookup[lookup$priority %in% unique(subset.join$Stream.Characterization.CSCI),]

# save as factor
lookup.sub$priority <- factor(lookup.sub$priority, levels = unique(lookup.sub$priority))
subset.join$Stream.Characterization.CSCI <- factor(subset.join$Stream.Characterization.CSCI, levels = unique(lookup.sub$priority))

# synthesis map
syn.plot <- study + geom_sf(data = subset.join, color= "lightgrey", aes(fill=Stream.Characterization.CSCI, geometry = geometry)) +
  scale_fill_manual(name = "Management Recommendations", labels = lookup.sub$priority, values=lookup.sub$colors) +
  geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 

# print map
print(syn.plot)

#write plot
out.filename <- paste0(out.dir, "12_CSCI_Observed_Recommendations.jpg")
ggsave(syn.plot, file = out.filename, dpi=500, height=6, width=8)

## ASCI Observed maps

# Set up base map 
study <- ggplot(basins) + 
  labs(title="Recommendations using Observed ASCI Data",x ="", y = "")  + 
  geom_sf(color = "lightgrey", fill="white") +
  annotation_scale() +
  annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                         width = unit(.8, "cm")) +
  #labs(x ="", y = "")  + 
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_line(color = "white", size = 0.8)) 
study

# subset lookup categories and tables
lookup.sub <- lookup[lookup$priority %in% unique(subset.join$Stream.Characterization.ASCI),]

# save as factor
lookup.sub$priority <- factor(lookup.sub$priority, levels = unique(lookup.sub$priority))
subset.join$Stream.Characterization.ASCI <- factor(subset.join$Stream.Characterization.ASCI, levels = unique(lookup.sub$priority))

# synthesis map
syn.plot <- study + geom_sf(data = subset.join, color= "lightgrey", aes(fill=Stream.Characterization.ASCI, geometry = geometry)) +
  scale_fill_manual(name = "Management Recommendations", labels = lookup.sub$priority, values=lookup.sub$colors) +
  geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 

# print
print(syn.plot)

# write plot
out.filename <- paste0(out.dir, "12_ASCI_Observed_Recommendations.jpg")
ggsave(syn.plot, file = out.filename, dpi=500, height=6, width=8)


#############
## Maps using predicted data

## CSCI Predicted

# merge with basins
subset.join <- predicted.data %>% 
  full_join(basins, by = c('New_Name')) 

# Set up base map 
study <- ggplot(basins) + 
  labs(title="Recommendations using Predicted CSCI Data",x ="", y = "")  + 
  geom_sf(color = "lightgrey", fill="white") +
  annotation_scale() +
  annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                         width = unit(.8, "cm")) +
  #labs(x ="", y = "")  + 
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_line(color = "white", size = 0.8)) 
study

# subset lookup categories and tables
lookup.sub <- lookup[lookup$priority %in% unique(subset.join$Stream.Characterization.CSCI),]

# save as factor
lookup.sub$priority <- factor(lookup.sub$priority, levels = unique(lookup.sub$priority))
subset.join$Stream.Characterization.CSCI <- factor(subset.join$Stream.Characterization.CSCI, levels = unique(lookup.sub$priority))

# synthesis map
syn.plot <- study + geom_sf(data = subset.join, color= "lightgrey", aes(fill=Stream.Characterization.CSCI, geometry = geometry)) +
  scale_fill_manual(name = "Management Recommendations", labels = lookup.sub$priority, values=lookup.sub$colors) +
  geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 

# print
print(syn.plot)

# write plot
out.filename <- paste0(out.dir, "12_CSCI_predicted_Recommendations.jpg")
ggsave(syn.plot, file = out.filename, dpi=500, height=5, width=7)

######
# ASCI predicted

# Set up base map 
study <- ggplot(basins) + 
  labs(title="Recommendations using Predicted ASCI Data",x ="", y = "")  + 
  geom_sf(color = "lightgrey", fill="white") +
  annotation_scale() +
  annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                         width = unit(.8, "cm")) +
  #labs(x ="", y = "")  + 
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_line(color = "white", size = 0.8)) 
study

# subset lookup categories and tables
lookup.sub <- lookup[lookup$priority %in% unique(subset.join$Stream.Characterization.ASCI),]

# save as factor
lookup.sub$priority <- factor(lookup.sub$priority, levels = unique(lookup.sub$priority))
subset.join$Stream.Characterization.ASCI <- factor(subset.join$Stream.Characterization.ASCI, levels = unique(lookup.sub$priority))

# synthesis map
syn.plot <- study + geom_sf(data = subset.join, color= "lightgrey", aes(fill=Stream.Characterization.ASCI, geometry = geometry)) +
  scale_fill_manual(name = "Management Recommendations", labels = lookup.sub$priority, values=lookup.sub$colors) +
  geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 

# print
print(syn.plot)

# write plot
out.filename <- paste0(out.dir, "12_ASCI_predicted_Recommendations.jpg")
ggsave(syn.plot, file = out.filename, dpi=500, height=5, width=7)

#summary of subbasins in each category
length(subset.join$New_Name)
length(unique(subset.join$New_Name))

######################################################################################
## Compare the predicted and observed management recommendation categories

# combine datasets together
dataset.combined <- observed.data %>% 
  left_join(predicted.data, by = "new_subbasinname") %>% 
  rename(Stream.Characterization.CSCI.obs = Stream.Characterization.CSCI.x,
         Stream.Characterization.ASCI.obs = Stream.Characterization.ASCI.x,
         Stream.Characterization.CSCI.pred = Stream.Characterization.CSCI.y,
         Stream.Characterization.ASCI.pred = Stream.Characterization.ASCI.y)

# subset to only compare CSCI stream characterization
csci.comparison <- dataset.combined %>% 
  select( Stream.Characterization.CSCI.obs, Stream.Characterization.CSCI.pred) %>% 
  na.omit()

# tally comparison for contingency table
site.length <- length(csci.comparison$Stream.Characterization.CSCI.obs)
csci.obs.pred.summary <- csci.comparison %>% 
  group_by(Stream.Characterization.CSCI.obs, Stream.Characterization.CSCI.pred) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.2metric = 100*n/site.length) %>% 
  data.frame()

# subset to only compare ASCI stream characterization
ASCI.comparison <- dataset.combined %>% 
  select( Stream.Characterization.ASCI.obs, Stream.Characterization.ASCI.pred) %>% 
  na.omit()

# tally comparison for contingency table
site.length <- length(ASCI.comparison$Stream.Characterization.ASCI.obs)
ASCI.obs.pred.summary <- ASCI.comparison %>% 
  group_by(Stream.Characterization.ASCI.obs, Stream.Characterization.ASCI.pred) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.2metric = 100*n/site.length) %>% 
  data.frame()


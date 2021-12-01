### CSCI brts

setwd("/Users/katieirving/Documents/git/SOC_tier2/SOC_tier_2")

library(gbm)
# install.packages("dismo")
library(dismo)
library(ggplot2)
library(purrr)
source("code/functions/My.gbm.step.R")
library(dplyr)
library(tidyverse)

set.seed(321) # reproducibility

## upload data

data <- read.csv("output_data/00_csci_delta_formatted_median_updated_Nov2021.csv")
head(data)
## remove peak timing
names(data)
data <- data[,-c(1)]

## gbm functions in brt.functions script

## CSCI

# set up tuning params
hyper_grid <- expand.grid(
  shrinkage = c(0.001, 0.003, 0.005), 
  interaction.depth = c(5), 
  n.minobsinnode = c(3, 5, 10), 
  bag.fraction = c(0.75, 0.8) 
)

# double check and view
hyper_grid

# load the GBM.step function (requires dismo and function loaded)
gbm_fit_step <- function(
  shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_step <- My.gbm.step(
    gbm.y = 6, # response in training data
    gbm.x = 9:24, # hydro dat
    family = "gaussian",
    data = data,
    #max.trees = 8000, # can specify but don't for now
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = FALSE,
    verbose = FALSE
  )
  
  # Compute the Deviance Explained: (total dev - cv dev) / total dev
  if(!is.null(m_step)){ # this helps if there's an error above
    (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) /
      m_step$self.statistics$mean.null
  } else { 
    return(NA)
  }
}

# use PURRR: this part can take awhile...get some coffee
hyper_grid$dev_explained <-purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data) # CHECK AND CHANGE!!
)

# look at results:
hyper_grid %>% 
  dplyr::arrange(desc(dev_explained)) %>%
  head(5) # top 5 models

# pick the best solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))


# based on above, run final BRT and save:
gbm_final_step <- function(
  shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_final <- My.gbm.step(
    gbm.y = 6, # response in training data
    gbm.x = 9:24, # hydro dat
    family = "gaussian",
    data = data,
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = TRUE,
    verbose = TRUE
  )
}

# set up filename for best model outputs
(gbm_best_file <- paste0("models/07_gbm_final_csci_model_output_Nov2020.txt"))

# run best option with PURR
capture.output(gbm_fin_out <- purrr::pmap(
  hyper_best,
  ~ gbm_final_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data # CHECK AND CHANGE!!
  )
), file=gbm_best_file, append=T)

#strip off a list layer to view data
(gbm_fin_out <- gbm_fin_out[[1]])

# add hyperbest to capture output file:
cat("\nBest parameters for GBM.STEP:\n\n", 
    file = gbm_best_file, append=TRUE)

# add the parameters used to run the model
write.csv(hyper_best, "output_data/07_best_model_csci_output_Nov2020.csv")

# % percent explained
(gbm_fin_out$self.statistics$mean.null - gbm_fin_out$cv.statistics$deviance.mean) / gbm_fin_out$self.statistics$mean.null 
# 0.387123

# # gbm.step(data, gbm.x = 10:31, gbm.y = 7)
#          
# tbrt <- gbm.step(data, gbm.x = 10:33, gbm.y = 7, 
#                  family ="gaussian", tree.complexity = 5, learning.rate = 0.0001, bag.fraction = 0.5) 
# 
# 
# tbrt2 <- gbm.step(data, gbm.x = 10:33, gbm.y = 7, 
#                  family ="gaussian", tree.complexity = 5, learning.rate = 0.001, bag.fraction = 0.5) 

# 10. SAVE FINAL GBM AND DATA ---------------------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(paste0("gbm_final_csci")), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = paste0("gbm_final_csci")))

# save to RDS
write_rds(x = get(fileToSave), path = paste0("models/07_",fileToSave, "_model_Nov2020.rds"), compress = "gz")

# Save all the datasets used in the model:
save(list = ls(pattern="data_"), file = tolower(paste0("models/05_",fileToSave,"_model_data_Nov2020.rda")))

gbm_final <- read_rds("models/07_gbm_final_csci_model_Nov2020.rds")
class(gbm_final)

gbm_fin_RI<-as.data.frame(summary(gbm_final, plotit = F, method=relative.influence)) 
gbm_fin_RI  



# Plots and metrics-------------------------------------------------------------------

## upload full FFM names

labels <- read.csv("Data/ffm_names.csv")
labels <- labels[1:24, ]
labels
labels <- labels %>% rename(var = Flow.Metric.Code)
labels[25, 1] <- "Magnitude of largest annual storm"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow"
labels

gbm_fin_RI <- left_join(gbm_fin_RI, labels, by ="var")

write.csv(gbm_fin_RI, "models/07_rel_imp_csci_labels.csv")

png("figures/Updated_ffm_figures/rel_imp_csci_bar_plot_Nov2021.png", width = 640, height = 550)

ggplot(data=gbm_fin_RI, aes(x=reorder(Flow.Metric.Name,-rel.inf), y=rel.inf, fill = Flow.Component)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))+
  # scale_x_continuous(limits = c(0, 35)) +
  labs(title = "Relative Importance of FFM on CSCI",
       x = "Flow Metric",
       y = "Relative Importance (%)") #+ theme_bw(base_size = 15)

dev.off()


gbm_fin_RI





### November 2020
#               var   rel.inf                  Flow.Metric.Name
# 1             Q99 36.891524 Magnitude of largest annual storm
# 2          SP_Tim 20.503851                     Spring timing
# 3     Wet_BFL_Dur  7.942475               Wet-season duration
# 4          SP_Dur  5.076464                   Spring duration
# 5          SP_Mag  4.343020        Spring recession magnitude
# 6       DS_Mag_50  3.925646        Dry-season median baseflow
# 7  Wet_BFL_Mag_10  3.428230           Wet-season low baseflow
# 8          SP_ROC  3.000139             Spring rate of change
# 9  Wet_BFL_Mag_50  2.496241        Wet-season median baseflow
# 10      DS_Mag_90  2.349376          Dry-season high baseflow
# 11         DS_Tim  2.311228                 Dry-season timing
# 12         FA_Tim  2.221499                 Fall pulse timing
# 13         FA_Mag  1.902631              Fall pulse magnitude
# 14      DS_Dur_WS  1.268785               Dry-season duration
# 15        Wet_Tim  1.265155                 Wet-season timing
# 16         FA_Dur  1.073737               Fall pulse duration


# Correlation -------------------------------------------------------------


## correlation matrix
names(data)
cor_data <- data[,c(9:24)]
corel <- cor(cor_data, method="spearman", use="complete.obs")
write.csv(corel, "output_data/07_correlation_matrix_ffm_v3_Nov2020.csv")
### figures for only top 5 most important vars

## upload s curve data 
## negative delta h 
neg_csci <- read.csv("output_data/01_csci_neg_logR_metrics_figures_Nov2020.csv")

## positive delta h 
pos_csci <- read.csv("output_data/01_csci_pos_logR_metrics_figures_Nov2020.csv")
labels <- labels %>%
  rename(hydro.endpoints = var)
head(pos_csci)
head(neg_csci)

# labels <- read.csv("Data/ffm_names.csv")
# labels <- labels[1:24, ]
# labels <- labels %>% rename(hydro.endpoints = Flow.Metric.Code)
# labels[25, 1] <- "Q99"
# labels[25, 2] <- "Q99"
# labels[25, 3] <- "Peak Flow"
# labels

pos_csci <- left_join(pos_csci, labels, by ="hydro.endpoints")
neg_csci <- left_join(neg_csci, labels, by ="hydro.endpoints")

## metrics chosen


## Q99, 
metrics <-c("Q99", "SP_Dur","Wet_BFL_Dur" )
# metrics_cor <-c("SP_Tim", "Peak_2", "Peak_Dur_10", "Peak_Fre_10","SP_Mag", "Peak_5", "Wet_BFL_Dur")

## subset to only important metrics
pos_csci_sub <- subset(pos_csci, hydro.endpoints %in% metrics)
head(pos_csci_sub)


# pos_csci_sub_cor_mets <- subset(pos_csci, hydro.endpoints %in% metrics_cor)
# head(pos_csci_sub_cor_mets)

neg_csci_sub <- subset(neg_csci, hydro.endpoints %in% metrics)
head(neg_csci_sub)

# neg_csci_sub_cor_mets <- subset(neg_csci, hydro.endpoints %in% metrics_cor)
# head(neg_csci_sub_cor_mets)


# Figures -----------------------------------------------------------------



## postive delta H - CSCI
png("figures/Updated_ffm_figures/top_metrics_csci_pos_curves_Nov2021.png", width = 640, height = 550)

ggplot(pos_csci_sub, aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  geom_path()+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=2)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1) +
  theme_minimal()+
  theme(text = element_text(size=15),legend.position="none") +
  labs(title = "Probability of Good CSCI",
       x = "Delta H (+ve)",
       y = "Predicted Probability") #+ theme_bw(base_size = 15)
dev.off()

## negative delta H - CSCI
png("figures/Updated_ffm_figures/top_metrics_csci_neg_curves_Nov2021.png", width = 640, height = 550)

ggplot(neg_csci_sub, aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  geom_path()+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=2)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1) +
  theme_minimal()+
  theme(text = element_text(size=15),legend.position="none") +
  labs(title = "Probability of Good CSCI",
       x = "Delta H (-ve)",
       y = "Predicted Probability") #+ theme_bw(base_size = 15)

dev.off()


# GLMs for top metrics ----------------------------------------------------

load(file="models/01_CSCI_negative_GLM_all_delta_mets_Nov2020.RData") ## neg.glm
load(file="models/01_CSCI_positive_GLM_all_delta_mets_Nov2020.RData") ## pos.glm

hydro <-read.csv("output_data/01_hydro_endpoints_order_Nov2020.csv")
length(neg.glm) ## 48
dim(hydro)

# metrics <-c("SP_Tim", "Peak_2", "Peak_Dur_10", "Peak_Fre_10", "SP_Mag", "Peak_5", "Wet_BFL_Dur")
gbm_fin_RI

## get index of model combination
hydro$index <- row.names(hydro)

hydrox <- hydro %>%
  filter(biol.endpoints == "CSCI",
         hydro.endpoints %in% metrics)
str(hydrox)
## negative models

df <- as.data.frame(matrix(ncol=8, nrow=3))
colnames(df) <- c("metric", "index", "deviance", "null_deviance", "perc_explained", "AIC", "relative_importance", "n")
mod <- neg.glm[[1]]
neg.glm
metrics <- unique(hydrox$hydro.endpoints)
metrics
# i=1

for(i in 1: length(metrics)) {
  
  met <- metrics[i]
  
  hydroxx <- hydrox %>%
    filter(hydro.endpoints == met) %>%
    select(index) #%>%
  
  gbm_fin_RIx <- gbm_fin_RI %>%
    filter(var == met) %>%
    select(rel.inf)
  
  ri <-gbm_fin_RIx$rel.inf
  
  ind <- as.numeric(hydroxx$index) 
  ind
  mod <- neg.glm[[ind]] 
    
  df[i, 1] <- met
  df[i, 2] <- ind
  df[i, 3] <- mod$deviance
  df[i, 4] <- mod$null.deviance
  df[i, 5] <- (mod$null.deviance-mod$deviance)/mod$null.deviance*100
  df[i ,6] <- mod$aic
  df[i ,7] <- ri
  df[i, 8] <- length(mod$y)
  
}

df
write.csv(df, "output_data/07_csci_neg_top_mets_glm_results_Nov2020.csv")
  
## positive models

df <- as.data.frame(matrix(ncol=8, nrow=3))
colnames(df) <- c("metric", "index", "deviance", "null_deviance", "perc_explained", "AIC", "relative_importance", "n")
mod <- neg.glm[[1]]

# i=1

for(i in 1: length(metrics)) {
  
  met <- metrics[i]
  
  hydroxx <- hydrox %>%
    filter(hydro.endpoints == met) %>%
    select(index) #%>%
  
  gbm_fin_RIx <- gbm_fin_RI %>%
    filter(var == met) %>%
    select(rel.inf)
  
  ri <-gbm_fin_RIx$rel.inf
  
  ind <- as.numeric(hydroxx$index) 
  
  mod <- pos.glm[[ind]] 
  
  df[i, 1] <- met
  df[i, 2] <- ind
  df[i, 3] <- mod$deviance
  df[i, 4] <- mod$null.deviance
  df[i, 5] <- (mod$null.deviance-mod$deviance)/mod$null.deviance*100
  df[i ,6] <- mod$aic
  df[i ,7] <- ri
  df[i, 8] <- length(mod$y)
  
}

df
write.csv(df, "output_data/07_csci_pos_top_mets_glm_results_Nov2020.csv")

neg.glm[[2]]
pos.glm[[2]]

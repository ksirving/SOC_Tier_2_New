## ASCI GLMs

library(reshape)
library(CSCI)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(rcompanion)
library(tidyr)


## thresholds for asci - Susie paper, table 8 - hybrid MMI 0.86
## thresholds - 0.75, 0.86, 0.94

## upload asci data
asci<-read.csv("output_data/00_asci_delta_formatted_median_Nov2021.csv")
head(asci)
dim(asci)
asci <- asci[, -c(1)]

## bio 

biol.endpoints<-c("H_ASCI","D_ASCI")#

## hydro
hydro.endpoints<- colnames(asci)[6:21]

## thresholds

thresholds <- c(0.75, 0.86, 0.94) ## hybrid and diatom are the same

bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, thresholds = thresholds,  stringsAsFactors = F)
bio_h_summary

write.csv(bio_h_summary, "output_data/01a_asci_hydro_endpoints_order_April2021.csv")
i=1
neg.glm<-lapply(1:nrow(bio_h_summary), function(i)
{
  
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<- as.character(bio_h_summary[i,"thresholds"])

  mydat<-na.omit(asci[,c(hmet, bmet)])
  names(mydat)<-c( "hydro","bio")
  
  mydat <- mydat[which(mydat$hydro<=0 ),]

  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]
  
  glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
  
  
})
save(neg.glm, file="models/01a_ASCI_negative_GLM_all_delta_mets_April2021.RData")
length(neg.glm)
data <- NULL

data <- as.data.frame(data)
# data
bio_h_summary$comb_code <- paste(bio_h_summary$biol.endpoints, "_", bio_h_summary$hydro.endpoints, "_", bio_h_summary$thresholds, sep="")
length(bio_h_summary)

code <- bio_h_summary$comb_code
code
i=1

for(i in 1: length(code)) {
  
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<- as.character(bio_h_summary[i,"thresholds"])
  
  mydat<-na.omit(asci[,c( hmet, bmet)])
  names(mydat)<-c( "hydro","bio")

  mydat <- mydat[which(mydat$hydro<=0 ),]

  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]
  mydat$site_num <- rownames(mydat)
  # write.csv(mydat, paste("output_data/glm_data/06_", bmet,"_neg_", hmet,  "_glm.csv", sep=""))
  
  # glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
  mydat
  mydat$hydro_code <- hmet
  mydat$bio <- bmet
  mydat$threshold <- bmet.thresh
  names(data)
  data
  names(mydat)
  # mydat <- mydat[c(4,1,5,2,6, 3)]
  data <- bind_rows(data, mydat)
  
  
}
head(data)
data_neg <- data
data_neg
bio_h_summary
i =32

pos.glm<-lapply(1:nrow(bio_h_summary), function(i)
{
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<-as.character(bio_h_summary[i,"thresholds"])
  
  mydat<-na.omit(asci[,c( hmet, bmet)])
  names(mydat)<-c( "hydro","bio")
  
  mydat <- mydat[which(mydat$hydro>=0),]

  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]

  # write.csv(mydat, paste("output_data/glm_data/06_", bmet,"_pos_", hmet,  "_glm.csv", sep=""))
  
  glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
  
})

save(pos.glm, file="models/01a_ASCI_positive_GLM_all_delta_mets_April2021.RData")

data <- NULL
data <- as.data.frame(data)


bio_h_summary$comb_code <- paste(bio_h_summary$biol.endpoints, "_", bio_h_summary$hydro.endpoints, "_", bio_h_summary$thresholds, sep="")
length(bio_h_summary)

code <- bio_h_summary$comb_code
code

for(i in 1: length(code)) {
  
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<-as.character(bio_h_summary[i,"thresholds"])
  
  mydat<-na.omit(asci[,c( hmet, bmet)])
  names(mydat)<-c( "hydro","bio")
  
  mydat <- mydat[which(mydat$hydro>=0  ),]

  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]
  mydat$site_num <- rownames(mydat)
  # write.csv(mydat, paste("output_data/glm_data/06_", bmet,"_neg_", hmet,  "_glm.csv", sep=""))
  
  # glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
  
  mydat
  mydat$hydro_code <- hmet
  mydat$bio <- bmet
  mydat$threshold <- bmet.thresh
  names(data)
  names(mydat)
  data <- bind_rows(data, mydat)
  
  
}

data_pos <- data
data_pos

##### glm coeficients
library(rcompanion)

coefs <- data.frame(matrix(ncol=11, nrow=96))
colnames(coefs) <- c("InterceptCoef", "VariableCoef", "Deviance", "AIC", "NullDeviance",
                     "InterceptPvalue", "VariablePvalue", "Delta", "McFadden", "Nagelkerke", "n")

for(i in 1:length(neg.glm)) {
  
  mod <- neg.glm[[i]]
  rsq <- nagelkerke(mod)
  # rsq$Number.of.observations[1]
  # summary(mod)
  # mod
  
  coefs[i,1] <- coef(mod)[1]
  coefs[i,2] <- coef(mod)[2]
  coefs[i,3] <- mod$deviance
  coefs[i,4] <- mod$aic
  coefs[i,5] <- mod$null.deviance
  coefs[i,6:7] <- coef(summary(mod))[,4]
  coefs[i,8] <- "Negative"
  coefs[i,9] <- rsq$Pseudo.R.squared.for.model.vs.null[1]
  coefs[i,10] <- rsq$Pseudo.R.squared.for.model.vs.null[3]
  coefs[i,11] <- rsq$Number.of.observations[1]
}


## join with main summary df
bio_h_summary
ascinegcoefs <- cbind(bio_h_summary, coefs)
ascinegcoefs
dim(bio_h_summary)
## positive

coefs <- data.frame(matrix(ncol=11, nrow=96))
colnames(coefs) <- c("InterceptCoef", "VariableCoef", "Deviance", "AIC", "NullDeviance",
                     "InterceptPvalue", "VariablePvalue", "Delta", "McFadden", "Nagelkerke", "n")
pos.glm
for(i in 1:length(pos.glm)) {
  
  mod <- pos.glm[[i]]
  rsq <- nagelkerke(mod)
  
  coefs[i,1] <- coef(mod)[1]
  coefs[i,2] <- coef(mod)[2]
  coefs[i,3] <- mod$deviance
  coefs[i,4] <- mod$aic
  coefs[i,5] <- mod$null.deviance
  coefs[i,6:7] <- coef(summary(mod))[,4]
  coefs[i,8] <- "Positive"
  coefs[i,9] <- rsq$Pseudo.R.squared.for.model.vs.null[1]
  coefs[i,10] <- rsq$Pseudo.R.squared.for.model.vs.null[3]
  coefs[i,11] <- rsq$Number.of.observations[1]
}



## join with main summary df
bio_h_summary
asciposcoefs <- cbind(bio_h_summary, coefs)
asciposcoefs

### join pos and neg 

asci_coefs <- rbind(asciposcoefs, ascinegcoefs)
getwd()
write.csv(asci_coefs, "output_data/manuscript/01_asci_glm_coefs.csv")

hydro.m<-na.omit(unique(melt(asci[,hydro.endpoints])))
hydro.m<-hydro.m[order(hydro.m$variable,hydro.m$value),]
names(hydro.m)<-c("hydro.endpoints","hydro.threshold")
head(hydro.m)
# i=6124

## hydro threshold here - just used to see whether delta h is negative or positive and predict
head(bio_h_summary)
bio_h_summary2<-merge(bio_h_summary, hydro.m)
head(bio_h_summary2)
dim(bio_h_summary2)
neg.glm

bio_h_summary2 <- na.omit(bio_h_summary2)

i
bio_h_summary2$PredictedProbability<-
  sapply(1:nrow(bio_h_summary2), function(i)
  {
    hmet<-bio_h_summary2[i,"hydro.endpoints"]
    bmet<-bio_h_summary2[i,"biol.endpoints"]
    hthresh <- bio_h_summary2[i,"thresholds"]
    thresh<-bio_h_summary2[i,"hydro.threshold"]
    thresh
    # print(paste(hmet,bmet))
    modnum<-  which(bio_h_summary$hydro.endpoints==hmet & bio_h_summary$biol.endpoints==bmet 
                    & bio_h_summary$thresholds==hthresh)
    modnum
    if(thresh<0) {
      mymod<-neg.glm[[modnum]]
    } else {
      mymod<-pos.glm[[modnum]]
    }
        

    mydata<-data.frame(hydro=thresh)
    mydata
    predict(mymod, newdata=mydata, type="response")
    
  })

bio_h_summary2$Type<-ifelse(bio_h_summary2$hydro.threshold<0,"Negative","Positive")

head(bio_h_summary2)
tail(bio_h_summary2)
sum(is.na(data_neg))
## negative delta H

## subset only negatives - data seyt with predicted probability
neg_pred <- subset(bio_h_summary2, Type!="Positive")

data_neg$comb_code <- paste(data_neg$bio, "_", data_neg$hydro_code,"_", data_neg$threshold, sep="")
# dim(data_neg)
all_data <- merge(neg_pred, data_neg, by=c("comb_code"), all=T)

head(all_data)
all_data$thresholds <- as.character(all_data$thresholds)
write.csv(all_data, "output_data/01a_asci_all_data_neg_logR_metrics_figures_April2021.csv")

### ASCI H endpoint
all_data_hybrid <- subset(all_data,biol.endpoints=="H_ASCI")
head(all_data_hybrid)
write.csv(all_data_hybrid, "output_data/01_h_asci_neg_logR_metrics_figures_April2021.csv")
unique(all_data_hybrid$hydro.endpoints)


### ASCI D endpoint
all_data_dia <- subset(all_data,biol.endpoints=="D_ASCI")
head(all_data_dia)
write.csv(all_data_dia, "output_data/01_d_asci_neg_logR_metrics_figures_April2021.csv")


## positive

## subset only positive numbers
pos_pred <- subset(bio_h_summary2, Type!="Negative")
## merge dataframes

data_pos$comb_code <- paste(data_pos$bio, "_", data_pos$hydro_code,"_", data_pos$threshold, sep="")

all_data <- merge(pos_pred, data_pos, by=c("comb_code"), all=T)

## changhe thresholds to character for figures
all_data$thresholds <- as.character(all_data$thresholds)
# unique(all_data$Type)
write.csv(all_data, "output_data/01a_asci_all_data_pos_logR_metrics_figures_April2021.csv")

### ASCI H endpoint
all_data_hybrid <- subset(all_data,biol.endpoints=="H_ASCI")
head(all_data_hybrid)
write.csv(all_data_hybrid, "output_data/01_h_asci_pos_logR_metrics_figures_April2021.csv")

all_data_hybrid <- read.csv("output_data/01_h_asci_pos_logR_metrics_figures_April2021.csv")
### ASCI D endpoint
all_data_dia <- subset(all_data,biol.endpoints=="D_ASCI")
head(all_data_dia)
write.csv(all_data_dia, "output_data/01_d_asci_pos_logR_metrics_figures_April2021.csv")




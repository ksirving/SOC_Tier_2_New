## glms for csci and median deltah

library(reshape)
library(CSCI)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(rcompanion)
library(tidyr)

## below defines thresholds from all csci

ref<-loadRefData()

## upload new formatted data

new_csci <- read.csv("output_data/00_csci_delta_formatted_median_updated_Nov2021.csv")
head(new_csci)
dim(new_csci)

## remove X column, change names
names(new_csci)
new_csci <- new_csci[, -c(1)]
names(new_csci)[4:6] <- c("OoverE", "MMI", "CSCI")

## define index thresholds, bio & hydro endpoints
thresholds <- c(0.63, 0.79, 0.92) 
biol.endpoints<-c("CSCI","OoverE","MMI")
hydro.endpoints<- colnames(new_csci)[9:24]

## create df with all combinations of threshold and endpoints
bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, 
                             thresholds = thresholds,  stringsAsFactors = F)
bio_h_summary

## save
write.csv(bio_h_summary, "output_data/01_hydro_endpoints_order_Nov2020.csv")


## GLMs per ffm, threshold and endpoints
## negative delta

neg.glm<-lapply(1:nrow(bio_h_summary), function(i)
  {
  
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<- as.character(bio_h_summary[i,"thresholds"])
  
  mydat<-na.omit(new_csci[,c(hmet, bmet)])
  names(mydat)<-c( "hydro","bio")
  
  mydat <- mydat[which(mydat$hydro<=0 ),]
  
  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]
  
  glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
    
    
})

save(neg.glm, file="models/01_CSCI_negative_GLM_all_delta_mets_April2021.RData")


## extract data points from models
data <- NULL
data <- as.data.frame(data)
bio_h_summary$comb_code <- paste(bio_h_summary$biol.endpoints, "_", bio_h_summary$hydro.endpoints, "_", bio_h_summary$thresholds, sep="")
length(bio_h_summary)

code <- bio_h_summary$comb_code

  for(i in 1: length(code)) {
  
    hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
    bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
    bmet.thresh<- as.character(bio_h_summary[i,"thresholds"])
    
    mydat<-na.omit(new_csci[,c(hmet, bmet)])
    names(mydat)<-c( "hydro","bio")
    
    mydat <- mydat[which(mydat$hydro<=0 ),]
    
    mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
    mydat<-mydat[order(mydat$bio),]
    mydat$site_num <- rownames(mydat)

    mydat$hydro_code <- hmet
    mydat$bio <- bmet
    mydat$threshold <- bmet.thresh

    data <- bind_rows(data, mydat)

}

data_neg <- data

## positive delta
pos.glm<-lapply(1:nrow(bio_h_summary), function(i)
{
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<-as.character(bio_h_summary[i,"thresholds"])
  
  mydat<-na.omit(new_csci[,c(hmet, bmet)])
  names(mydat)<-c( "hydro","bio")
  
  mydat <- mydat[which(mydat$hydro>=0),]
  
  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]

  glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
  
 
})

# save
save(pos.glm, file="models/01_CSCI_positive_GLM_all_delta_mets_April2021.RData")

## extract datapoints from model
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
  
  mydat<-na.omit(new_csci[,c(hmet, bmet)])
  names(mydat)<-c( "hydro","bio")
  
  mydat <- mydat[which(mydat$hydro>=0  ),]
  
  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]
  mydat$site_num <- rownames(mydat)
 
  mydat$hydro_code <- hmet
  mydat$bio <- bmet
  mydat$threshold <- bmet.thresh

  data <- bind_rows(data, mydat)
  
  
}

data_pos <- data
data_pos

##### extract glm coeficients

coefs <- data.frame(matrix(ncol=11, nrow=144))
colnames(coefs) <- c("InterceptCoef", "VariableCoef", "Deviance", "AIC", "NullDeviance",
                     "InterceptPvalue", "VariablePvalue", "Delta", "McFadden", "Nagelkerke", "n")

for(i in 1:length(neg.glm)) {
  
  mod <- neg.glm[[i]]
  rsq <- nagelkerke(mod)

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
cscinegcoefs <- cbind(bio_h_summary, coefs)
cscinegcoefs

## positive
coefs <- data.frame(matrix(ncol=11, nrow=144))
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
csciposcoefs <- cbind(bio_h_summary, coefs)
csciposcoefs

### join pos and neg 

csci_coefs <- rbind(csciposcoefs, cscinegcoefs)

## save coefs
write.csv(csci_coefs, "output_data/manuscript/01_csci_glm_coefs.csv")

## combine ffm names and delta h
hydro.m<-na.omit(unique(melt(new_csci[,hydro.endpoints])))
hydro.m<-hydro.m[order(hydro.m$variable,hydro.m$value),]
names(hydro.m)<-c("hydro.endpoints","hydro.threshold")
head(hydro.m)

## hydro threshold here - just used to see whether delta h is negative or positive
bio_h_summary2<-merge(bio_h_summary, hydro.m)
head(bio_h_summary2)

## extract predicted probability
bio_h_summary2$PredictedProbability<-
  sapply(1:nrow(bio_h_summary2), function(i)
  {
    hmet<-bio_h_summary2[i,"hydro.endpoints"]
    bmet<-bio_h_summary2[i,"biol.endpoints"]
    hthresh <- bio_h_summary2[i,"thresholds"]
    thresh<-bio_h_summary2[i,"hydro.threshold"]
    thresh

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

## ad neg or pos
bio_h_summary2$Type<-ifelse(bio_h_summary2$hydro.threshold<0,"Negative","Positive")

## Combine all data together - input and output of model
## subset only negative numbers
neg_pred <- subset(bio_h_summary2, Type!="Positive")

## merge dataframes
head(data_neg) ## response 1/0
head(neg_pred) ## predicted probabiloty

## create merge code
data_neg$comb_code <- paste(data_neg$bio, "_", data_neg$hydro_code,"_", data_neg$threshold, sep="")
dim(data_neg)
all_data <- merge(neg_pred, data_neg, by="comb_code", all=T)
head(all_data)

all_data$thresholds <- as.character(all_data$thresholds)

# save - df for BRTs
write.csv(all_data, "output_data/01_all_data_neg_logR_metrics_figures_April2021.csv")

### CSCI endpoint only
all_data_csci <- subset(all_data,biol.endpoints=="CSCI")
head(all_data_csci)
write.csv(all_data_csci, "output_data/01_csci_neg_logR_metrics_figures_April2021.csv")


ggplot(all_data, aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)
  
## CSCI
ggplot(all_data_csci, aes(x=hydro.threshold, y=PredictedProbability, color=thresholds))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)


## positive

## subset only negative numbers
pos_pred <- subset(bio_h_summary2, Type!="Negative")

## merge dataframes
head(data_pos) ## condition 1/0s
head(pos_pred) ## negative pred probs 

data_pos$comb_code <- paste(data_pos$bio, "_", data_pos$hydro_code, "_", data_pos$threshold, sep="")

all_data <- merge(pos_pred, data_pos, by="comb_code", all=T)
head(all_data)

## save
write.csv(all_data, "output_data/01_all_data_pos_logR_metrics_figures_April2021.csv")

### CSCI endpoint
all_data_csci <- subset(all_data,biol.endpoints=="CSCI")
head(all_data_csci)
write.csv(all_data_csci, "output_data/01_csci_pos_logR_metrics_figures_April2021.csv")


# all_data[which(all_data$hydro==-194),]
## all endpoints - each endpoint has different number of 1s and 0s
ggplot(all_data, aes(x=hydro.threshold, y=PredictedProbability, color=biol.endpoints))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)

## CSCI
ggplot(all_data_csci, aes(x=hydro.threshold, y=PredictedProbability, color=threshold))+
  # geom_point(size=1)+
  geom_path()+#stat_summary(fun.y="mean", geom="line")+
  facet_wrap(~hydro.endpoints, scales="free_x", nrow=4)+scale_y_continuous(limits=c(0,1))+
  geom_vline(xintercept=0, linetype="dashed")+
  geom_point(aes(y=Condition, x=hydro), colour = 'black', size = 1)


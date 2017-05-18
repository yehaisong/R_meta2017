##Haisong Ye, 4/26/2017
library(readr)
library(metaSEM)
library(metafor)
library(MVN)
library(nortest)

##Read data from csv
SRSTd <- read_csv("yeh_ad.csv")
SRSTd.TotalN<-SRSTd$CGn+SRSTd$EGn
mean(SRSTd.TotalN)
sd(SRSTd.TotalN)
max(SRSTd.TotalN)
min(SRSTd.TotalN)
sum(SRSTd.TotalN)
###Retention
#overall analysis
resSR<-rma(measure="SMD",SRd,SRv,data=SRSTd)
resSR

#Subgroup analysis
# Presentation Pace
#Approach 1: Q statistics
#Seperate analysis for Presentation Pace (PP)
resSR_SelfPace<-rma(measure="SMD",SRd,SRv,data=SRSTd,subset=PP==0)
resSR_SelfPace
resSR_SystemPace<-rma(measure="SMD",SRd,SRv,data=SRSTd,subset=PP==1)
resSR_SystemPace
#Create data frame for the two subgroup estimates
SRSTd.compPP<-data.frame(
  estimate=c(coef(resSR_SelfPace),
  coef(resSR_SystemPace)),
  stderror=c(resSR_SelfPace$se,resSR_SystemPace$se),
  PP=c("Self-paced","System-paced"),
  tau2=round(c(resSR_SelfPace$tau2,resSR_SystemPace$tau2),3)
  )
SRSTd.compPP
#Compare the two estimates
resSR_PP<-rma(estimate,sei=stderror,mods= ~ PP, method="FE", data=SRSTd.compPP,digits=3)
resSR_PP
#Approach 2: meta regression
resSR_PP_reg<-rma(measure="SMD",SRd,SRv,mods=~PP,data=SRSTd,digits = 3)
resSR_PP_reg
plot(resSR_PP_reg)

#Length of VS
#Appraoch 1: Q statistics
#Seperate analysis for Length of Verbal Segment (LVS)
#Approach 2: meta regression
resSR_LVS_reg<-rma(measure="SMD",SRd,SRv,mods=~LVS,data=SRSTd, digits=3)
resSR_LVS_reg
plot(resSR_LVS_reg)

#Type of visualization
#Approach 1: Q statistics
#Approach 2: meta regression
resSR_ToV_reg<-rma(measure="SMD",SRd,SRv,mods=~ToV,data=SRSTd, digits=3)
resSR_ToV_reg
plot(resSR_ToV_reg)

#two moderators
resSR_PP_LVS_reg<-rma(measure="SMD",SRd,SRv,mods=cbind(PP,LVS),data=SRSTd, digits=3)
resSR_PP_LVS_reg

resSR_PP_ToV_reg<-rma(measure="SMD",SRd,SRv,mods=cbind(PP,ToV),data=SRSTd, digits=3)
resSR_PP_ToV_reg

resSR_ToV_LVS_reg<-rma(measure="SMD",SRd,SRv,mods=cbind(LVS,ToV),data=SRSTd, digits=3)
resSR_ToV_LVS_reg

#All moderators
resSR_All_reg<-rma(measure="SMD",SRd,SRv,mods=cbind(PP,LVS,ToV),data=SRSTd, digits=3)
resSR_All_reg



###Transfer
#overall analysis
resST<-rma(measure="SMD",STd,STv,data=SRSTd)
resST

#Subgroup analysis
# Presentation Pace
#Approach 1: Q statistics
#Seperate analysis for Presentation Pace (PP)
#Create data frame for the two subgroup estimates
#Approach 2: meta regression
resST_PP_reg<-rma(measure="SMD",STd,STv,mods=~PP,data=SRSTd,digits = 3)
resST_PP_reg
plot(resST_PP_reg)

#Length of VS
#Appraoch 1: Q statistics
#Seperate analysis for Length of Verbal Segment (LVS)
#Approach 2: meta regression
resST_LVS_reg<-rma(measure="SMD",STd,STv,mods=~LVS,data=SRSTd, digits=3)
resST_LVS_reg
plot(resST_LVS_reg)

#Type of visualization
#Approach 1: Q statistics
#Approach 2: meta regression
resST_ToV_reg<-rma(measure="SMD",STd,STv,mods=~ToV,data=SRSTd, digits=3)
resST_ToV_reg
plot(resST_ToV_reg)

#two moderators
resST_PP_LVS_reg<-rma(measure="SMD",STd,STv,mods=cbind(PP,LVS),data=SRSTd, digits=3)
resST_PP_LVS_reg

resST_PP_ToV_reg<-rma(measure="SMD",STd,STv,mods=cbind(PP,ToV),data=SRSTd, digits=3)
resST_PP_ToV_reg

resST_ToV_LVS_reg<-rma(measure="SMD",STd,STv,mods=cbind(LVS,ToV),data=SRSTd, digits=3)
resST_ToV_LVS_reg

#All moderators
resST_All_reg<-rma(measure="SMD",STd,STv,mods=cbind(PP,LVS,ToV),data=SRSTd, digits=3)
resST_All_reg
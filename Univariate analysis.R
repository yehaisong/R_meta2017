##Haisong Ye, 4/26/2017
library(readr)
library(metaSEM)
library(metafor)
library(MVN)
library(nortest)

##Read data from csv
SRSTd <- read_csv("yeh_ad_final_05162017.csv")

SRSTd.TotalN<-SRSTd$CGn+SRSTd$EGn
cat("N mean: ", mean(SRSTd.TotalN))
cat("N SD: ",sd(SRSTd.TotalN))
cat("N MAX: ", max(SRSTd.TotalN))
cat("N MIN: ", min(SRSTd.TotalN))
cat("N Total: ", sum(SRSTd.TotalN))



###Retention
#overall analysis
resSR<-rma(measure="SMD",SRd,SRv,data=SRSTd)
resSR
##forest plot for retention
rmaSR<-rma(yi=SRd,vi=SRv,data=SRSTd)
forest(rmaSR,slab=SRSTd$ExperimentID)
title("Forest plot for LPKR")
forest(SRSTd$SRd, SRSTd$SRv,
       xlim=c(-2.5,3.5),
       subset=order(SRSTd$SRd),
       slab=NA, annotate=FALSE,
       efac=0,
       pch=19,
       col="gray40",
       psize=2,
       cex.lab=1,cex.axis = 1,
       lty=c("solid","blank"))
title("Caterpillar plot for LPKR")
points(sort(SRSTd$SRd), 68:1, pch=19, cex=0.5)
addpoly(rmaSR, row=0, mlab = "", annotate = FALSE, cex=1)
text(-2,0,"RE Model", pos=4, offset=0, cex=1)
#funnel and trimfill adjustment
par(mar=c(5,4,1,2))
rmaSR.taf<-trimfill(rmaSR)
funnel(rmaSR.taf)
rmaSR.taf
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
resSR.PP<-rma(measure="SMD",SRd,SRv,mods=~PP,data=SRSTd,digits = 3)
resSR.PP
plot(resSR.PP)

#Length of VS
#Appraoch 1: Q statistics
#Seperate analysis for Length of Verbal Segment (LVS)
#Approach 2: meta regression
resSR.LVS<-rma(measure="SMD",SRd,SRv,mods=~LVS,data=SRSTd, digits=3)
resSR.LVS
plot(resSR.LVS)

#Type of visualization
#Approach 1: Q statistics
#Approach 2: meta regression
resSR.ToV<-rma(measure="SMD",SRd,SRv,mods=~ToV,data=SRSTd, digits=3)
resSR.ToV
plot(resSR.ToV)

#two moderators
resSR.PPxLVS<-rma(measure="SMD",SRd,SRv,mods=cbind(PP,LVS,PoPxLoVS),data=SRSTd, digits=3)
resSR.PPxLVS
cov2cor(vcov.rma(resSR.PPxLVS))

resSR.PPxToV<-rma(measure="SMD",SRd,SRv,mods=cbind(PP,ToV,PoPxToV),data=SRSTd, digits=3)
resSR.PPxToV
cov2cor(vcov.rma(resSR.PPxToV))

resSR.ToVxLVS<-rma(measure="SMD",SRd,SRv,mods=cbind(LVS,ToV, LoVSxToV),data=SRSTd, digits=3)
resSR.ToVxLVS
cov2cor(vcov.rma(resSR.ToVxLVS))

#All moderators
resSR.PPxLVSxToV<-rma(measure="SMD",SRd,SRv,mods=cbind(PP,LVS,ToV,PoPxLoVS,PoPxToV,LoVSxToV,PoPxLoVSxToV),data=SRSTd, digits=3)
resSR.PPxLVSxToV
cov2cor(vcov.rma(resSR.PPxLVSxToV))
resSR.PP_LVS_ToV<-rma(measure="SMD",SRd,SRv,mods=cbind(PP,LVS,ToV),data=SRSTd, digits=3)
resSR.PP_LVS_ToV
cov2cor(vcov.rma(resSR.PP_LVS_ToV))

###Transfer
#overall analysis
resST<-rma(measure="SMD",STd,STv,data=SRSTd)
resST

##forest plot for transfer
rmaST<-rma(yi=STd,vi=STv,data=SRSTd)
forest(rmaST,slab=SRSTd$ExperimentID)
title("Forest plot for LPKT")
forest(SRSTd$STd, SRSTd$STv,
       xlim=c(-2.5,3.5),
       subset=order(SRSTd$STd),
       slab=NA, annotate=FALSE,
       efac=0,
       pch=19,
       col="gray40",
       psize=2,
       cex.lab=1,cex.axis = 1,
       lty=c("solid","blank"))
title("Caterpillar plot for LPKT")
points(sort(SRSTd$STd), 58:1, pch=19, cex=0.5)
addpoly(rmaST, row=0, mlab = "", annotate = FALSE, cex=1)
text(-2,0,"RE Model", pos=4, offset=0, cex=1)
#funnel and trimfill adjustment
par(mar=c(5,4,1,2))
rmaST.taf<-trimfill(rmaST)
funnel(rmaST.taf)
rmaST.taf

#Subgroup analysis
# Presentation Pace
#Approach 1: Q statistics
#Seperate analysis for Presentation Pace (PP)
#Create data frame for the two subgroup estimates
#Approach 2: meta regression
resST.PP<-rma(measure="SMD",STd,STv,mods=~PP,data=SRSTd,digits = 3)
resST.PP
plot(resST.PP)

#Length of VS
#Appraoch 1: Q statistics
#Seperate analysis for Length of Verbal Segment (LVS)
#Approach 2: meta regression
resST.LVS<-rma(measure="SMD",STd,STv,mods=~LVS,data=SRSTd, digits=3)
resST.LVS
plot(resST.LVS)

#Type of visualization
#Approach 1: Q statistics
#Approach 2: meta regression
resST.ToV<-rma(measure="SMD",STd,STv,mods=~ToV,data=SRSTd, digits=3)
resST.ToV
plot(resST.ToV)

#two moderators
resST.PPxLVS<-rma(measure="SMD",STd,STv,mods=cbind(PP,LVS,PoPxLoVS),data=SRSTd, digits=3)
resST.PPxLVS
cov2cor(vcov.rma(resST.PPxLVS))

resST.PPxToV<-rma(measure="SMD",STd,STv,mods=cbind(PP,ToV,PoPxToV),data=SRSTd, digits=3)
resST.PPxToV
cov2cor(vcov.rma(resST.PPxToV))

resST.ToVxLVS<-rma(measure="SMD",STd,STv,mods=cbind(LVS,ToV, LoVSxToV),data=SRSTd, digits=3)
resST.ToVxLVS
cov2cor(vcov.rma(resST.ToVxLVS))

#All moderators
resST.PPxLVSxToV<-rma(measure="SMD",STd,STv,mods=cbind(PP,LVS,ToV,PoPxLoVS,PoPxToV,LoVSxToV,PoPxLoVSxToV),data=SRSTd, digits=3)
resST.PPxLVSxToV
cov2cor(vcov.rma(resST.PPxLVSxToV))

resST.PP_LVS_ToV<-rma(measure="SMD",STd,STv,mods=cbind(PP,LVS,ToV),data=SRSTd, digits=3)
resST.PP_LVS_ToV
cov2cor(vcov.rma(resST.PP_LVS_ToV))

#CL
#overall analysis
resCL<-rma(measure="SMD",CLd,CLv,data=SRSTd)
resCL

##forest plot for CL
rmaCL<-rma(yi=CLd,vi=CLv,data=SRSTd)
forest(rmaCL,slab=SRSTd$ExperimentID)
title("Forest plot for LCL")
forest(SRSTd$CLd, SRSTd$CLv,
       xlim=c(-2.5,3.5),
       subset=order(SRSTd$CLd),
       slab=NA, annotate=FALSE,
       efac=0,
       pch=19,
       col="gray40",
       psize=2,
       cex.lab=1,cex.axis = 1,
       lty=c("solid","blank"))
title("Caterpillar plot for LCL")
points(sort(SRSTd$CLd), 23:1, pch=19, cex=0.5)
addpoly(rmaCL, row=0, mlab = "", annotate = FALSE, cex=1)
text(-2,0,"RE Model", pos=4, offset=0, cex=1)
par(mar=c(5,4,1,2))
rmaCL.taf<-trimfill(rmaCL)
funnel(rmaCL.taf)
rmaCL.taf

#two moderators
resCL.PPxLVS<-rma(measure="SMD",CLd,CLv,mods=cbind(PP,LVS,PoPxLoVS),data=SRSTd, digits=3)
resCL.PPxLVS
cov2cor(vcov.rma(resCL.PPxLVS))

resCL.PPxToV<-rma(measure="SMD",CLd,CLv,mods=cbind(PP,ToV,PoPxToV),data=SRSTd, digits=3)
resCL.PPxToV
cov2cor(vcov.rma(resCL.PPxToV))

resCL.ToVxLVS<-rma(measure="SMD",CLd,CLv,mods=cbind(LVS,ToV, LoVSxToV),data=SRSTd, digits=3)
resCL.ToVxLVS
cov2cor(vcov.rma(resCL.ToVxLVS))

#All moderators
resCL.PPxLVSxToV<-rma(measure="SMD",CLd,CLv,mods=cbind(PP,LVS,ToV,PoPxLoVS,PoPxToV,LoVSxToV,PoPxLoVSxToV),data=SRSTd, digits=3)
resCL.PPxLVSxToV
cov2cor(vcov.rma(resCL.PPxLVSxToV))
resCL.PP_LVS_ToV<-rma(measure="SMD",CLd,CLv,mods=cbind(PP,LVS,ToV),data=SRSTd, digits=3)
resCL.PP_LVS_ToV
cov2cor(vcov.rma(resCL.PP_LVS_ToV))

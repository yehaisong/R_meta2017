#Shortterm Retention and shortterm Transfer effect sizes
library(readr)
library(metaSEM)
library(metafor)
library(MVN)

##Read data from csv
SRSTd <- read_csv("SRd_STd_r0.csv")

##QQPlot test using metaFor
resSR<-rma(measure="SMD",SRd,SRv,data=SRSTd)
qqnorm(resSR,label="out", main="Normal Q-Q Plot of Retention")
resSR1M<-rma(measure="SMD",SRd,SRv,mods=cbind(PP,LVS,ToV),data=SRSTd)
qqnorm(resSR1M,label="out", main="Normal Q-Q Plot of Retention with Moderators")

resST<-rma(measure="SMD",STd,STv,data=SRSTd)
qqnorm(resST,label="out",main="Normal Q-Q Plot of Transfer")
resST1M<-rma(measure="SMD",STd,STv,mods=cbind(PP,LVS,ToV),data=SRSTd)
qqnorm(resST1M,label="out",main="Normal Q-Q Plot of Trasnfer with Moderators")

resCL<-rma(measure="SMD",CLd,CLv,data=SRSTd)
qqnorm(resCL)

##Normality test using Maridia's MVN
uniPlot(SRSTd[9:10],type="qqplot")

##Multivariance meta analysis with metaSEM
##SRd-short term retention effect size, STd-short tem transfer effect size, v-variance,cov-covariance
result<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov,STv),data=SRSTd, model.name="Random effects model")
summary(result)

##plot the effect sizes and their confidence ellipses
plot(result,diag.panel = FALSE)

##Plot the effect sizes with the forest plots
##create extra panels for the forest plots
plot(result,diag.panel = TRUE, 
     main="Learning Performance Analysis", 
     axis.labels = c("Retention","Transfer"))
##forest plot for retention
forest(rma(yi=SRd,vi=SRv,data=SRSTd))
title("Forest plot of retention")
##forest plot for transfer
forest(rma(yi=STd,vi=STv,data=SRSTd))
title("Forest plot of transfer")

##three moderators
result1<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov,STv),data=SRSTd, model.name="Random effects model",x=cbind(PP,LVS,ToV))
summary(result1)
plot(result1)

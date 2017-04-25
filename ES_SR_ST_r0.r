#Shortterm Retention and shortterm Transfer effect sizes
library(readr)
library(metaSEM)
library(metafor)
library(MVN)
library(nortest)

##Read data from csv
SRSTd <- read_csv("SRd_STd_CLd_r0.csv")

##QQPlot test using metaFor
resSR<-rma(measure="SMD",SRd,SRv,data=SRSTd)
srd<-SRSTd[["SRd"]]
lillie.test(srd)#Kolmogorov-Smirnov normality test
sf.test(srd)#Shapiro-Francia normality test
qqnorm(resSR,label="out", main="Normal Q-Q Plot of Retention")
resSR1M<-rma(measure="SMD",SRd,SRv,mods=cbind(PP,LVS,ToV),data=SRSTd)
qqnorm(resSR1M,label="out", main="Normal Q-Q Plot of Retention with Moderators")

resST<-rma(measure="SMD",STd,STv,data=SRSTd)
std<-SRSTd[["STd"]]
lillie.test(std)#Kolmogorov-Smirnov normality test
sf.test(std)#Shapiro-Francia normality test
qqnorm(resST,label="out",main="Normal Q-Q Plot of Transfer")
resST1M<-rma(measure="SMD",STd,STv,mods=cbind(PP,LVS,ToV),data=SRSTd)
qqnorm(resST1M,label="out",main="Normal Q-Q Plot of Trasnfer with Moderators")

resCL<-rma(measure="SMD",CLd,CLv,data=SRSTd)
qqnorm(resCL)

##Normality test using Maridia's MVN
nmtres<-mardiaTest(SRSTd[,c(9:10)],qqplot=TRUE)
unires<-uniPlot(SRSTd[9:10],type="qqplot")

##r=0
##Multivariance meta analysis with metaSEM
##SRd-short term retention effect size, STd-short tem transfer effect size, v-variance,cov-covariance
result<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov,STv),data=SRSTd, model.name="Random effects model")
summary(result)

##extract the variance component of the random effects
T2<-vec2symMat(coef(result,select="random"))
T2
##Convert the covariance matrix to a correlation matrix
cov2cor(T2)


##plot the effect sizes and their confidence ellipses
plot(result, axis.labels = c("Retention","Transfer"))

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


##r=1
result1<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov1,STv),data=SRSTd, model.name="Random effects model")
summary(result1)

##three moderators
resultpp<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov,STv),data=SRSTd, model.name="Random effects model",x=cbind(PP))
summary(resultpp)
resultlvs<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov,STv),data=SRSTd, model.name="Random effects model",x=cbind(LVS))
summary(resultlvs)
resulttov<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov,STv),data=SRSTd, model.name="Random effects model",x=cbind(ToV))
summary(resulttov)
resultall<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov,STv),data=SRSTd, model.name="Random effects model",x=cbind(PP,LVS,ToV))
summary(resultall)
plot(resultall)

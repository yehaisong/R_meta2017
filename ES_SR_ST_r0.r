#Shortterm Retention and shortterm Transfer effect sizes
library(readr)
library(metaSEM)
library(metafor)
library(MVN)
library(nortest)

##Read data from csv
SRSTd <- read_csv("yeh_ad_final_05162017.csv")

##QQPlot test using metaFor
#Normality test for short term retention
srd<-SRSTd[["SRd"]]
lillie.test(srd)#Kolmogorov-Smirnov normality test
sf.test(srd)#Shapiro-Francia normality test
#Generate QQ plot for retention
resSR<-rma(measure="SMD",SRd,SRv,data=SRSTd)
resSR
qqnorm(resSR,label="out", main="Normal Q-Q Plot of Retention")
#generate QQ plot for retention with moderators
#resSR1M<-rma(measure="SMD",SRd,SRv,mods=cbind(PP,LVS,ToV),data=SRSTd)
#qqnorm(resSR1M,label="out", main="Normal Q-Q Plot of Retention with Moderators")

##Normality test for short term transfer
std<-SRSTd[["STd"]]
lillie.test(std)#Kolmogorov-Smirnov normality test
sf.test(std)#Shapiro-Francia normality test
#generate QQ plot for retention
resST<-rma(measure="SMD",STd,STv,data=SRSTd)
resST
qqnorm(resST,label="out",main="Normal Q-Q Plot of Transfer")
#generate QQ plot for transfer
#resST1M<-rma(measure="SMD",STd,STv,mods=cbind(PP,LVS,ToV),data=SRSTd)
#qqnorm(resST1M,label="out",main="Normal Q-Q Plot of Trasnfer with Moderators")

##Normality test for cognitive load
cld<-SRSTd[["CLd"]]
lillie.test(cld)#Kolmogorov-Smirnov normality test
sf.test(cld)#Shapiro-Francia normality test
#generate QQ plot for cognitive load
resCL<-rma(measure="SMD",CLd,CLv,data=SRSTd)
resCL
qqnorm(resCL, label="out", main="Normal Q-Q Plot of Cognitive Load")
#generate QQ plot for cognitive load with moderatoers
#resSR1M<-rma(measure="SMD",CLd,CLv,mods=cbind(PP,LVS,ToV),data=SRSTd)
#qqnorm(resCL, label="out", main="Normal Q-Q Plot of Cognitive Load with Moderators")

##Normality test using Maridia's MVN for multivariate
vmndata<-SRSTd[,c("SRd","STd")]
nmtres<-mardiaTest(vmndata,qqplot=TRUE)
nmtres

##r=0
##Multivariance meta analysis with metaSEM
##SRd-short term retention effect size, STd-short tem transfer effect size, v-variance,cov-covariance
result.SRST0.main<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov,STv),data=SRSTd, model.name="Random Effects Model Analysis for Retention and Transfer")
summary(result.SRST0.main)

##extract the variance component of the random effects
result.SRST0.T2<-vec2symMat(coef(result.SRST0.main,select="random"))
##T2
##Convert the covariance matrix to a correlation matrix
result.SRST0.cor<-cov2cor(result.SRST0.T2)
result.SRST0.cor

##plot the effect sizes and their confidence ellipses
plot(result.SRST0.main, axis.labels = c("Retention","Transfer"),study.ellipse.plot = FALSE)

##Plot the effect sizes with the forest plots
##create extra panels for the forest plots
#plot(result.SRST0.main,diag.panel = TRUE, 
#     main="Learning Performance Analysis", 
#     axis.labels = c("Retention","Transfer"))




##r=1
result.SRST1.main<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov1,STv),data=SRSTd, model.name="Random effects model")
summary(result.SRST1.main)
plot(result.SRST1.main, axis.labels = c("Retention","Transfer"),study.ellipse.plot = FALSE)

##extract the variance component of the random effects
result.SRST1.T2<-vec2symMat(coef(result.SRST1.main,select="random"))
##Convert the covariance matrix to a correlation matrix
result.SRST1.cor<-cov2cor(result.SRST1.T2)
result.SRST1.cor

##three moderators
resultpp<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov,STv),data=SRSTd, model.name="Random effects model",x=cbind(PP))
summary(resultpp)
resultlvs<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov,STv),data=SRSTd, model.name="Random effects model",x=cbind(LVS))
summary(resultlvs)
resulttov<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov,STv),data=SRSTd, model.name="Random effects model",x=cbind(ToV))
summary(resulttov)
resultall<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov,STv),data=SRSTd, model.name="Random effects model",x=cbind(PP,LVS,ToV))
summary(resultall)
T2all<-vec2symMat(coef(resultall,select="random"))
T2all
cov2cor(T2all)
plot(resultall)

#CL
result.CL.main<-meta(y=cbind(CLd),v=cbind(CLv),data=SRSTd, model.name="Random effects model")
summary(result.CL.main)
#plot(resultCL,study.ellipse.plot = FALSE)


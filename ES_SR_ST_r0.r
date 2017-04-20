#Shortterm Retention and shortterm Transfer effect sizes
library(readr)
library(metaSEM)
library(metafor)

##Read data from csv
SRSTd <- read_csv("SRd_STd_r1.csv")

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

##r=1
result1<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov1,STv),data=SRSTd, model.name="Random effects model")
summary(result1)

##three moderators
resultall<-meta(y=cbind(SRd,STd),v=cbind(SRv,SRSTcov,STv),data=SRSTd, model.name="Random effects model",x=cbind(PP,LVS,ToV))
summary(resultall)
plot(resultall)

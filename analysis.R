mirc=read.csv("RandomMIRSamplingC.csv")
mirc$ID=paste(mirc$Year,mirc$Week,mirc$County,sep="_")
mirc$ID=as.factor(mirc$ID)

library(lme4)
library(lmerTest)
fit0=lmer(log(Relative_Error_Absolute+0.0001)~1+(1|ID),data = mirc)

fitfull=update(fit0,~MIR0+County+TrapDensity+(1|ID))
pf=predict(fitfull)
mirc$epf=exp(pf)-0.0001

fit2=update(fitfull,~MIR0*TrapDensity+County+(1|ID))
pf2=predict(fit2)
mirc$epf2=exp(pf2)-0.0001

ggplot(mirc,aes(TrapDensity,epf,fill=County))+
  geom_jitter()



# fit0=lmer(log(Relative_Error_Absolute+0.0001)~1+(Week+Year|County),data = mirc)
# fitfull=update(fit0,~MIR0+TrapDensity+(Week+Year|County))
# fit2=update(fitfull,~MIR0*TrapDensity+(Week+Year|County))

preddata=data.frame(County=rep("COOK",2500),ID=rep("2010_31_COOK",2500),MIR0=rep(seq(from=min(mirc$MIR0),to=max(mirc$MIR0),length.out = 50),50),
                    TrapDensity=rep(seq(from=min(mirc$TrapDensity),to=max(mirc$TrapDensity),length.out = 50),each=50))
prednew=predict(fit2,preddata)
preddata$epf2=exp(prednew)-0.0001

library(RColorBrewer)
ggplot(preddata,aes(TrapDensity,MIR0))+
  geom_raster(aes(fill=epf2))+
  scale_fill_gradient(low="blue",high="red")

f2ci=confint(fit2)
fullout=cbind(summary(fit2)$coefficients,f2ci[3:7,])
save(fullout,file="coefficients.Rdata")

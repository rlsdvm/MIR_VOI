load("RandomMIRSamplingCountywithVariance.Rdata")
ggplot(resultsC,aes(MIR0,Relative_Error_Absolute,color=TrapDensity))+
  geom_jitter(alpha=0.1)+
  scale_color_gradientn(colours=rainbow(10))

ggplot(resultsC,aes(MIR0,rangeDiff,color=TrapDensity))+
  geom_jitter(alpha=0.1)+
  scale_color_gradientn(colours=rainbow(10))

ggplot(resultsC,aes(Relative_Error_Absolute,rangeDiff,color=TrapDensity))+
  geom_jitter(alpha=0.1)+
  scale_color_gradientn(colours=rainbow(10))
ggplot(resultsC,aes(Relative_Error_Absolute,MIRrange,color=TrapDensity))+
  geom_jitter(alpha=0.1)+
  scale_color_gradientn(colours=rainbow(10))
ggplot(resultsC,aes(MIR0range,MIRrange,color=TrapDensity))+
  geom_jitter(alpha=0.1)+
  scale_color_gradientn(colours=rainbow(10))
ggplot(resultsC,aes(MIR0range,rangeDiff,color=TrapDensity))+
  geom_jitter(alpha=0.1)+
  scale_color_gradientn(colours=rainbow(10))
ggplot(resultsC,aes(MIR0,MIR0range,color=TrapDensity))+
  geom_jitter(alpha=0.1)+
  scale_color_gradientn(colours=rainbow(10))
ggplot(resultsC,aes(MIR0,rangeDiff,color=TrapDensity))+
  geom_jitter(alpha=0.1)+
  scale_color_gradientn(colours=rainbow(10))

ggplot(resultsC,aes(MIR0,MIR,color=TrapDensity,size=MIRrange*100))+
  geom_jitter(alpha=0.1)+
  scale_color_gradientn(colours=rainbow(10))
ggplot(resultsC,aes(MIR0,MIR,color=TrapDensity))+
  geom_pointrange(alpha=0.1,aes(ymin=MIRL,ymax=MIRU))+
  scale_color_gradientn(colours=rainbow(10))

library(lme4)
library(lmerTest)
mirc=resultsC[which(resultsC$MIR0!=0),]
fit0=lmer(log(Relative_Error_Absolute+0.00001)~1+(1|ID),data = mirc)

fitfull=update(fit0,~MIR0+TrapDensity+(1|ID))
pf=predict(fitfull)
mirc$epf=exp(pf)-0.00001
mirc$predict=pf

fit2=update(fitfull,~MIR0*TrapDensity+(1|ID))
pf2=predict(fit2)
mirc$epf2=exp(pf2)-0.00001

fitsim=glm(log(Relative_Error_Absolute+0.00001)~MIR0*TrapDensity+Year+Week+County,data=mirc,family = "gaussian",subset = which(MIR0!=0))
pfsim=predict(fitsim)
mirc$`Fixed Effects`=exp(pfsim)-0.00001

library(tidyr)
library(ggplot2)
names(mirc)[c(28,29)]=c("Random Effects","Fixed Effects")
rC_long=gather(mirc,"Model","Prediction",c(28,29))
tiff("Model_fit.tif")
ggplot(rC_long,aes(x=Relative_Error_Absolute,y=Prediction,color=MIR0))+
  geom_jitter()+
  geom_abline(slope=1,intercept = 0)+
  facet_grid(County~Model)
dev.off()

tiff("figure_2_rev_viridis.tif",height = 800,width = 800)
ggplot(resultsC,aes(MIR0,Relative_Error,color=TrapDensity))+
  geom_jitter()+
  scale_color_viridis_c()+
  theme_classic()+
  theme(text=element_text(size = 20))+
  labs(y="Relative Error in Simulated MIR",x=expression(MIR[100]),color="Density\nof Traps")
dev.off()

# resultsC$p=factor(resultsC$p)
# tiff("figure_2_rev_p.tif")
# ggplot(resultsC,aes(MIR0,Relative_Error,color=p))+
#   geom_jitter()+
#   scale_color_viridis_d()+
#   labs(y="Relative Error",x=expression(MIR[0]),color="Proportion of\nTraps Sampled")
# dev.off()

ggplot(resultsC,aes(MIR0,TrapDensity,color=log(Relative_Error_Absolute)))+
  geom_jitter()+
  scale_color_gradientn(colours = terrain.colors(20))+
  labs(color="Log Absolute\nRelative Error",x=expression(MIR[0]),y="Density of Traps")



f2ci=confint(fit2)
fullout=cbind(summary(fit2)$coefficients,f2ci[3:6,])
fullout=signif(fullout,2)
save(fullout,file="coefficients.Rdata")
write.csv(fullout,file="coefficients.csv")

###########analysis of range
ggplot(resultsC,aes(MIR0,rangeDiff,color=TrapDensity))+
  geom_jitter(alpha=0.1)+
  scale_color_viridis_c()+
  theme_classic()+
  labs(y="Change in Range of Simulated MIR",x=expression(MIR[100]),color="Density\nof Traps")

tiff("FigureNew3.tif",height = 800,width = 800)
ggplot(resultsC,aes(TrapDensity,rangeDiff,color=MIR0))+
  geom_jitter(alpha=0.1)+
  scale_color_viridis_c(option = "A")+
  theme_classic()+
  theme(text=element_text(size = 20))+
  labs(y="Change in Range of 95% CI around Simulated MIR",color=expression(MIR[100]),x="Density of Traps")
dev.off()

fitr=lmer(rangeDiff~1+(1|ID),data=mirc)
fitr2=update(fitr,~MIR0*TrapDensity+(1|ID))
rangetab=summary(fitr2)$coefficients
fr2ci=confint(fitr2)
rangetab=cbind(rangetab,fr2ci[3:6,])
write.csv(signif(rangetab,2),file="range_model_output.csv")
save(rangetab,file = "range_model_output.Rdata")


#don't use - no negative values!
# fitrt=lmer(MIRrange~1+(1|ID),data=mirc)
# fitrt2=update(fitr,~MIR0*TrapDensity+MIR0range+(1|ID))

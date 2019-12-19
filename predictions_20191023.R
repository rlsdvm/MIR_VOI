load("coefficients.Rdata")
write.csv(fullout,file="full_model_output.csv")

load("RandomMIRSamplingCountywithVariance.Rdata")
resultsC$cumweek=resultsC$Week+52*(resultsC$Year-2005)
resultsC$ID=paste(resultsC$Year,resultsC$Week,resultsC$County,sep="_")
resultsC$ID=as.factor(resultsC$ID)

library(ggplot2)
library(MASS)

uID=unique(resultsC$ID)
uniq=match(uID,resultsC$ID)
basedata=resultsC[uniq,]


cookdata=basedata[which(basedata$County=="COOK"),]
basedata=basedata[order(basedata$cumweek),]
basedata$Density0=basedata$TrapDensity/(basedata$p/100)
logpred=fullout[1,1]+basedata$MIR0*fullout[2,1]+basedata$Density0*fullout[3,1]+basedata$MIR0*basedata$Density0*fullout[4,1]
basedata$pred=exp(logpred)-0.00001
basedata$upred=basedata$MIR0/(1-basedata$pred)
basedata$lpred=basedata$MIR0/(1+basedata$pred)

summary(basedata$upred-basedata$lpred)*1000
summary(basedata$MIR0range)*1000


load("range_model_output.Rdata")
rangepred=rangetab[1,1]+basedata$MIR0*rangetab[2,1]+basedata$Density0*rangetab[3,1]+basedata$MIR0*basedata$Density0*rangetab[4,1]
basedata$urange=basedata$MIR0U-basedata$MIR0+basedata$upred+rangepred/2
basedata$lrange=basedata$MIR0L-basedata$MIR0+basedata$lpred-rangepred/2
basedata$lrange[which(basedata$lrange<0)]=0


tiff("figure3_rev.tif",width = 800,height = 800)
ggplot(basedata[which(basedata$Year%in%c(2014,2015)),])+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week),alpha=0.3,fill="red")+
  geom_errorbar(aes(x=Week,ymin=MIR0L,ymax=MIR0U))+
  geom_line(aes(y=urange,x=Week),color="red",linetype=2)+
  geom_line(aes(y=lrange,x=Week),color="red",linetype=2)+
  theme_classic()+
  scale_x_continuous(breaks = seq(from=20,to=41),limits = c(20,41))+
  theme(text=element_text(size = 20),axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 14))+
  labs(x="Week",y=expression(MIR[100]))+
  facet_grid(Year~County,scales = "free_y")
dev.off()



###
load("SmallCountyObservations.Rdata")
mirlw$cumweek=mirlw$Week+52*(mirlw$Year-2005)
mirlw$ID=paste(mirlw$Year,mirlw$Week,mirlw$County,sep="_")
mirlw$ID=as.factor(mirlw$ID)
uID=unique(mirlw$ID)
uniq=match(uID,mirlw$ID)
smdata=mirlw[uniq,]
smdata$MIR0=smdata$MIR0/1000

logpred=fullout[1,1]+smdata$MIR0*fullout[2,1]+smdata$TrapDensity*fullout[3,1]+smdata$MIR0*smdata$TrapDensity*fullout[4,1]
smdata$pred=exp(logpred)-0.00001
smdata$upred=smdata$MIR0/(1-smdata$pred)
smdata$lpred=smdata$MIR0/(1+smdata$pred)
rangepred=rangetab[1,1]+smdata$MIR0*rangetab[2,1]+smdata$TrapDensity*rangetab[3,1]+smdata$MIR0*smdata$TrapDensity*rangetab[4,1]
smdata$urange=smdata$MIR0U-smdata$MIR0+smdata$upred+rangepred/2
smdata$lrange=smdata$MIR0L-smdata$MIR0+smdata$lpred-rangepred/2
smdata$lrange[which(smdata$lrange<0)]=0

summary(smdata$upred-smdata$lpred)*1000
summary(smdata$MIR0U-smdata$MIR0L)*1000


usethese=c(which(smdata$County=="LAKE"&smdata$Year==2007),which(smdata$County=="MCHENRY"&smdata$Year==2013),which(smdata$County=="WILL"&smdata$Year==2005))
tiff("figure4_rev.tif",width = 400,height = 800)
ggplot(smdata[usethese,])+
  geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week),alpha=0.3,fill="red")+
  geom_line(aes(x=Week,y=urange),color="red",linetype=2)+
  geom_line(aes(x=Week,y=lrange),color="red",linetype=2)+
  geom_errorbar(aes(x=Week,ymin=MIR0L,ymax=MIR0U))+
  theme_classic()+
  facet_grid(County~.)+
  scale_x_continuous(breaks = seq(from=23,to=36),limits = c(23,36))+
  theme(text=element_text(size = 20),axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  labs(x="Week",y=expression(MIR[100]))
dev.off()

smdata$predrange=smdata$upred-smdata$lpred
smdata$obsrange=smdata$MIR0U-smdata$MIR0L
ggplot(smdata,aes(x=obsrange,y=predrange,color=MIR0))+geom_jitter()+
  geom_abline(slope=1,intercept = 0)

###############################
#probability of MIR=0 when not
resultsC$falseneg=0
resultsC$falseneg[which(resultsC$MIR==0)]=1
resultsC$falseneg=factor(resultsC$falseneg)
fn=resultsC[which(resultsC$MIR0>0),]

fnmod=glm(falseneg~MIR0*TrapDensity,data=fn,family = "binomial")
fn$predprob=predict(fnmod,type = "response")


fnplot=ggplot(fn,aes(x=MIR0,y=TrapDensity,color=falseneg))+
  geom_jitter(alpha=0.1)+
  theme_classic()+theme(legend.position="none")+
  scale_color_viridis_d()+
  labs(y="Trap Density",x=expression(MIR[100]))
library(ggExtra)
tiff("FalseNegativeProbability.tif",height = 800,width = 800)
ggMarginal(fnplot,type="violin",groupColour = T)
dev.off()

fnt=table(fn$falseneg,fn$p)
fnt[2,]/apply(fnt,2,sum)

summary(fn$MIR0[which(fn$falseneg==1)])
summary(fn$MIR0[which(fn$falseneg==0)])

fnci=confint(fnmod)
fncoef=summary(fnmod)$coefficients
fncoef=cbind(fncoef,fnci)
fncoef=cbind(fncoef,exp(fncoef[,c(1,5:6)]))
colnames(fncoef)[7:9]=c("OR","2.5% OR","97.5% OR")
write.csv(fncoef,file="falseNegativeModel.csv")
###############################

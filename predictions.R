load("coefficients.Rdata")
write.csv(fullout,file="full_model_output.csv")

load("RandomMIRSamplingCountywithVariance.Rdata")
resultsC$cumweek=resultsC$Week+52*(resultsC$Year-2005)
resultsC$ID=paste(resultsC$Year,resultsC$Week,resultsC$County,sep="_")
resultsC$ID=as.factor(resultsC$ID)

library(ggplot2)

uID=unique(resultsC$ID)
uniq=match(uID,resultsC$ID)
basedata=resultsC[uniq,]


cookdata=basedata[which(basedata$County=="COOK"),]
cookdata=cookdata[order(cookdata$cumweek),]
cookdata$Density0=cookdata$TrapDensity/(cookdata$p/100)
logpred=fullout[1,1]+cookdata$MIR0*fullout[2,1]+cookdata$Density0*fullout[3,1]+cookdata$MIR0*cookdata$Density0*fullout[4,1]
cookdata$pred=exp(logpred)-0.00001
cookdata$upred=cookdata$MIR0/(1-cookdata$pred)
cookdata$lpred=cookdata$MIR0/(1+cookdata$pred)

summary(cookdata$upred-cookdata$lpred)*1000
summary(cookdata$MIR0range)*1000


load("range_model_output.Rdata")
rangepred=rangetab[1,1]+cookdata$MIR0*rangetab[2,1]+cookdata$Density0*rangetab[3,1]+cookdata$MIR0*cookdata$Density0*rangetab[4,1]
cookdata$urange=cookdata$MIR0U-cookdata$MIR0+cookdata$upred+rangepred/2
cookdata$lrange=cookdata$MIR0L-cookdata$MIR0+cookdata$lpred-rangepred/2
cookdata$lrange[which(cookdata$lrange<0)]=0

# plot(cookdata$MIR0~cookdata$cumweek,type="l")
# lines(cookdata$upred~cookdata$cumweek,col=2,lty=2)
# lines(cookdata$lpred~cookdata$cumweek,col=2,lty=2)

ggplot(cookdata,aes(x=Week,y=MIR0))+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week,fill="band"),alpha=0.3)+
  geom_pointrange(aes(ymin=MIR0L,ymax=MIR0U))+
  scale_color_manual("",values="red")+
  scale_fill_manual("",values="grey12")+
  theme_classic()+
#  geom_line(aes(x=Week,y=Density0,color="red"))+
  facet_grid(Year~.)

# logpred5=fullout[1,1]+cookdata$MIR0*fullout[2,1]+cookdata$TrapDensity*fullout[3,1]+cookdata$MIR0*cookdata$TrapDensity*fullout[4,1]
# cookdata$pred5=exp(logpred5)+0.0001
# cookdata$upred5=cookdata$MIR0/(1-cookdata$pred5)
# cookdata$lpred5=cookdata$MIR0/(1+cookdata$pred5)
# logpred5=fullout[1,1]+cookdata$MIR0U*fullout[2,1]+cookdata$TrapDensity*fullout[3,1]+cookdata$MIR0U*cookdata$TrapDensity*fullout[4,1]
# cookdata$pred5U=exp(logpred5)+0.0001
# cookdata$upred5U=cookdata$MIR0U/(1-cookdata$pred5)
# cookdata$lpred5U=cookdata$MIR0U/(1+cookdata$pred5)
# logpred5=fullout[1,1]+cookdata$MIR0L*fullout[2,1]+cookdata$TrapDensity*fullout[3,1]+cookdata$MIR0L*cookdata$TrapDensity*fullout[4,1]
# cookdata$pred5L=exp(logpred5)+0.0001
# cookdata$upred5L=cookdata$MIR0L/(1-cookdata$pred5)
# cookdata$lpred5L=cookdata$MIR0L/(1+cookdata$pred5)

# cookdata$Density10=cookdata$Density0*.1
# logpred10=fullout[1,1]+cookdata$MIR0*fullout[2,1]+cookdata$Density10*fullout[3,1]+cookdata$MIR0*cookdata$Density10*fullout[4,1]
# cookdata$pred10=exp(logpred10)+0.00001
# cookdata$upred10=cookdata$MIR0/(1-cookdata$pred10)
# cookdata$lpred10=cookdata$MIR0/(1+cookdata$pred10)
# 
# tiff("figure3.tif",width = 800,height = 800)
# ggplot(cookdata)+geom_line(aes(x=Week,y=MIR0))+
#   geom_ribbon(aes(ymin=lpred10,ymax=upred10,x=Week,fill="10%"),alpha=0.3)+
#   geom_ribbon(aes(ymin=lpred5,ymax=upred5,x=Week,fill="50%"),alpha=0.3)+
#   geom_errorbar(aes(x=Week,ymin=MIR0L,ymax=MIR0U))+
#   guides(fill=guide_legend(title="Trap\nDensity"))+
#   theme_classic()+
#   scale_x_continuous(breaks = seq(from=20,to=41),limits = c(20,41))+
#   theme(text=element_text(size = 12),axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(x="Week",y=expression(MIR[0]))+
#   facet_wrap(~Year)
# dev.off()

tiff("figure3_rev.tif",width = 800,height = 800)
ggplot(cookdata[which(cookdata$Year%in%c(2015,2016)),])+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week),alpha=0.3,fill="green")+
  geom_errorbar(aes(x=Week,ymin=MIR0L,ymax=MIR0U))+
  geom_line(aes(y=urange,x=Week),color="green",linetype=2)+
  geom_line(aes(y=lrange,x=Week),color="green",linetype=2)+
  theme_classic()+
  scale_x_continuous(breaks = seq(from=20,to=41),limits = c(20,41))+
  theme(text=element_text(size = 14),axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 14))+
  labs(x="Week",y=expression(MIR[0]))+
  facet_grid(Year~.,scales = "free_y")
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
# logpred=fullout[1,1]+smdata$MIR0U*fullout[2,1]+smdata$TrapDensity*fullout[3,1]+smdata$MIR0U*smdata$TrapDensity*fullout[4,1]
# smdata$predU=exp(logpred)+0.00001
# smdata$upredU=smdata$MIR0U/(1-smdata$pred)
# smdata$lpredU=smdata$MIR0U/(1+smdata$pred)
# logpred=fullout[1,1]+smdata$MIR0L*fullout[2,1]+smdata$TrapDensity*fullout[3,1]+smdata$MIR0L*smdata$TrapDensity*fullout[4,1]
# smdata$predL=exp(logpred)+0.00001
# smdata$upredL=smdata$MIR0L/(1-smdata$pred)
# smdata$lpredL=smdata$MIR0L/(1+smdata$pred)
rangepred=rangetab[1,1]+smdata$MIR0*rangetab[2,1]+smdata$TrapDensity*rangetab[3,1]+smdata$MIR0*smdata$TrapDensity*rangetab[4,1]
smdata$urange=smdata$MIR0U-smdata$MIR0+smdata$upred+rangepred/2
smdata$lrange=smdata$MIR0L-smdata$MIR0+smdata$lpred-rangepred/2
smdata$lrange[which(smdata$lrange<0)]=0

summary(smdata$upred-smdata$lpred)*1000
summary(smdata$MIR0U-smdata$MIR0L)*1000

# tiff("figure4.tif",width = 400,height = 400)
# ggplot(smdata)+geom_line(aes(x=Week,y=MIR0))+
#   geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week),alpha=0.3,fill="green")+
#   geom_errorbar(aes(x=Week,ymin=MIR0L,ymax=MIR0U))+
#   theme_classic()+
#   scale_x_continuous(breaks = seq(from=23,to=36),limits = c(23,36))+
#   theme(text=element_text(size = 12),axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   labs(x="Week",y=expression(MIR[0]))+
#   facet_grid(County~Year)
# dev.off()

usethese=c(which(smdata$County=="LAKE"&smdata$Year==2007),which(smdata$County=="MCHENRY"&smdata$Year==2013),which(smdata$County=="WILL"&smdata$Year==2005))
tiff("figure4_rev.tif",width = 400,height = 400)
ggplot(smdata[usethese,])+
  geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week),alpha=0.3,fill="green")+
#  geom_ribbon(aes(ymin=lpredU,ymax=upredU,x=Week),alpha=0.2,fill="red")+
#  geom_ribbon(aes(ymin=lpredL,ymax=upredL,x=Week),alpha=0.2,fill="blue")+
  geom_line(aes(x=Week,y=urange),color="green",linetype=2)+
  geom_line(aes(x=Week,y=lrange),color="green",linetype=2)+
  geom_errorbar(aes(x=Week,ymin=MIR0L,ymax=MIR0U))+
  theme_classic()+
  facet_grid(County~.)+
  scale_x_continuous(breaks = seq(from=23,to=36),limits = c(23,36))+
  theme(text=element_text(size = 12),axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  labs(x="Week",y=expression(MIR[0]))
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
tiff("FalseNegativeProbability.tif")
ggMarginal(fnplot,type="violin",groupColour = T)
dev.off()



summary(fn$MIR0[which(fn$falseneg==1)])
summary(fn$MIR0[which(fn$falseneg==0)])

fnci=confint(fnmod)
fncoef=summary(fnmod)$coefficients
fncoef=cbind(fncoef,fnci)
fncoef=cbind(fncoef,exp(fncoef[,c(1,5:6)]))
colnames(fncoef)[7:9]=c("OR","2.5% OR","97.5% OR")
write.csv(fncoef,file="falseNegativeModel.csv")
###############################

ggplot(cookdata)+geom_line(aes(x=Week,y=MIR))+
  geom_ribbon(aes(ymin=lpred5,ymax=upred5,x=Week,fill="band"),alpha=0.3)+
  scale_color_manual("",values="red")+
  scale_fill_manual("",values="grey12")+
  theme_classic()+
  #  geom_line(aes(x=Week,y=Density0,color="red"))+
  facet_grid(Year~.)

cook2012=resultsC[which(resultsC$County=="COOK"&resultsC$Year==2012),]
cook2013=resultsC[which(resultsC$County=="COOK"&resultsC$Year==2013),]
cookdata2012=cookdata[which(cookdata$Year==2012),]
cookdata2013=cookdata[which(cookdata$Year==2013),]

cooklim=rbind(cook2012,cook2013)
cookdatalim=rbind(cookdata2012,cookdata2013)

ggplot(cookdata2012)+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred10,ymax=upred10,x=Week,fill="10%"),alpha=0.3)+
  geom_ribbon(aes(ymin=lpred5,ymax=upred5,x=Week,fill="50%"),alpha=0.3)+
  geom_violin(data=cook2012,aes(Week,MIR,group=Week))+
  guides(fill=guide_legend(title="Trap\nDensity"))+
  ylim(0,25)+
  theme_classic()

ggplot(cookdata2013)+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred10,ymax=upred10,x=Week,fill="10%"),alpha=0.3)+
  geom_ribbon(aes(ymin=lpred5,ymax=upred5,x=Week,fill="50%"),alpha=0.3)+
  geom_boxplot(data=cook2013,aes(Week,MIR,group=Week))+
  guides(fill=guide_legend(title="Trap\nDensity"))+
  theme_classic()

ggplot(cookdatalim)+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred10,ymax=upred10,x=Week,fill="10%"),alpha=0.3)+
  geom_ribbon(aes(ymin=lpred5,ymax=upred5,x=Week,fill="50%"),alpha=0.3)+
  geom_violin(data=cooklim,aes(Week,MIR,group=Week))+
  guides(fill=guide_legend(title="Trap\nDensity"))+
  #ylim(0,22)+
  facet_grid(Year~.)+
  theme_classic()




will2005=resultsC[which(resultsC$County=="WILL"&resultsC$Year==2005),]
will2006=resultsC[which(resultsC$County=="WILL"&resultsC$Year==2006),]
willdata2005=willdata[which(willdata$Year==2005),]
willdata2006=willdata[which(willdata$Year==2006),]

willlim=rbind(will2005,will2006)
willdatalim=rbind(willdata2005,willdata2006)

ggplot(willdata2005)+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred5,ymax=upred5,x=Week,fill="50%"),alpha=1)+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week,fill="100%"),alpha=1)+
  geom_boxplot(data=will2005,aes(Week,MIR,group=Week))+
  guides(fill=guide_legend(title="Trap\nDensity"))+
  scale_fill_manual(name="Trap\nDensity",labels=c("50%","100%"),values=c("#56B4E9","#E69F00"))+
  ylim(0,25)+
  theme_classic()

ggplot(willdata2006)+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week),alpha=0.3)+
  geom_boxplot(data=will2006,aes(Week,MIR,group=Week))+
  guides(fill=guide_legend(title="Trap\nDensity"))+
  theme_classic()

ggplot(willdatalim)+
  geom_ribbon(aes(ymin=lpred5,ymax=upred5,x=Week,fill="50%"),alpha=1)+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week,fill="100%"),alpha=1)+
  geom_line(aes(x=Week,y=MIR0))+
  geom_violin(data=willlim,aes(Week,MIR,group=Week))+
  guides(fill=guide_legend(title="Trap\nDensity"))+
  scale_fill_manual(name="Trap\nDensity",labels=c("100%","50%"),values=c("#56B4E9","#E69F00"))+
  ylim(0,15)+
  facet_grid(Year~.)+
  theme_classic()

###
lakedata=basedata[which(basedata$County=="LAKE"),]
lakedata=lakedata[order(lakedata$cumweek),]
lakedata$Density0=lakedata$TrapDensity/(lakedata$p/100)
logpred=fullout[1,1]+lakedata$MIR0*fullout[2,1]+lakedata$Density0*fullout[3,1]+lakedata$MIR0*lakedata$Density0*fullout[5,1]
lakedata$pred=exp(logpred)+0.0001
lakedata$upred=lakedata$MIR0/(1-lakedata$pred)
lakedata$lpred=lakedata$MIR0/(1+lakedata$pred)
logpred5=fullout[1,1]+lakedata$MIR0*fullout[2,1]+lakedata$TrapDensity*fullout[3,1]+lakedata$MIR0*lakedata$TrapDensity*fullout[5,1]
lakedata$pred5=exp(logpred5)+0.0001
lakedata$upred5=lakedata$MIR0/(1-lakedata$pred5)
lakedata$lpred5=lakedata$MIR0/(1+lakedata$pred5)
lake2007=resultsC[which(resultsC$County=="LAKE"&resultsC$Year==2007),]
lake2015=resultsC[which(resultsC$County=="LAKE"&resultsC$Year==2015),]
lakedata2007=lakedata[which(lakedata$Year==2007),]
lakedata2015=lakedata[which(lakedata$Year==2015),]

lakelim=rbind(lake2007,lake2015)
lakedatalim=rbind(lakedata2007,lakedata2015)

ggplot(lakedata2007)+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week),alpha=0.3)+
  geom_boxplot(data=lake2007,aes(Week,MIR,group=Week))+
  guides(fill=guide_legend(title="Trap\nDensity"))+
  ylim(0,25)+
  theme_classic()

ggplot(lakedata2015)+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week),alpha=0.3)+
  geom_boxplot(data=lake2015,aes(Week,MIR,group=Week))+
  guides(fill=guide_legend(title="Trap\nDensity"))+
  theme_classic()

ggplot(lakedatalim)+
  geom_ribbon(aes(ymin=lpred5,ymax=upred5,x=Week,fill="50%"),alpha=1)+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week,fill="100%"),alpha=1)+
  geom_line(aes(x=Week,y=MIR0))+
  geom_violin(data=lakelim,aes(Week,MIR,group=Week))+
  guides(fill=guide_legend(title="Trap\nDensity"))+
  scale_fill_manual(name="Trap\nDensity",labels=c("100%","50%"),values=c("#56B4E9","#E69F00"))+
  ylim(0,15)+
  facet_grid(Year~.)+
  theme_classic()

###
dupagedata=basedata[which(basedata$County=="DUPAGE"),]
dupagedata=dupagedata[order(dupagedata$cumweek),]
dupagedata$Density0=dupagedata$TrapDensity/(dupagedata$p/100)
logpred=fullout[1,1]+dupagedata$MIR0*fullout[2,1]+dupagedata$Density0*fullout[3,1]+dupagedata$MIR0*dupagedata$Density0*fullout[5,1]
dupagedata$pred=exp(logpred)+0.0001
dupagedata$upred=dupagedata$MIR0/(1-dupagedata$pred)
dupagedata$lpred=dupagedata$MIR0/(1+dupagedata$pred)
dupage2005=resultsC[which(resultsC$County=="DUPAGE"&resultsC$Year==2005),]
dupage2006=resultsC[which(resultsC$County=="DUPAGE"&resultsC$Year==2006),]
dupagedata2005=dupagedata[which(dupagedata$Year==2005),]
dupagedata2006=dupagedata[which(dupagedata$Year==2006),]

dupagelim=rbind(dupage2005,dupage2006)
dupagedatalim=rbind(dupagedata2005,dupagedata2006)

ggplot(dupagedata2005)+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week),alpha=0.3)+
  geom_boxplot(data=dupage2005,aes(Week,MIR,group=Week))+
  guides(fill=guide_legend(title="Trap\nDensity"))+
  ylim(0,25)+
  theme_classic()

ggplot(dupagedata2006)+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week),alpha=0.3)+
  geom_boxplot(data=dupage2006,aes(Week,MIR,group=Week))+
  guides(fill=guide_legend(title="Trap\nDensity"))+
  theme_classic()

ggplot(dupagedatalim)+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week),alpha=0.3)+
  geom_boxplot(data=dupagelim,aes(Week,MIR,group=Week))+
  guides(fill=guide_legend(title="Trap\nDensity"))+
  ylim(0,28)+
  facet_grid(Year~.)+
  theme_classic()

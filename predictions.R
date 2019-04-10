load("coefficients.Rdata")
write.csv(fullout,file="full_model_output.csv")

mirc=read.csv("RandomMIRSamplingC.csv")
mirc$cumweek=mirc$Week+52*(mirc$Year-2005)
mirc$ID=paste(mirc$Year,mirc$Week,mirc$County,sep="_")
mirc$ID=as.factor(mirc$ID)

library(ggplot2)

uID=unique(mirc$ID)
uniq=match(uID,mirc$ID)
basedata=mirc[uniq,]


cookdata=basedata[which(basedata$County=="COOK"),]
cookdata=cookdata[order(cookdata$cumweek),]
cookdata$Density0=cookdata$TrapDensity/(cookdata$p/100)
logpred=fullout[1,1]+cookdata$MIR0*fullout[2,1]+cookdata$Density0*fullout[3,1]+cookdata$MIR0*cookdata$Density0*fullout[5,1]
cookdata$pred=exp(logpred)+0.0001
cookdata$upred=cookdata$MIR0/(1-cookdata$pred)
cookdata$lpred=cookdata$MIR0/(1+cookdata$pred)

# plot(cookdata$MIR0~cookdata$cumweek,type="l")
# lines(cookdata$upred~cookdata$cumweek,col=2,lty=2)
# lines(cookdata$lpred~cookdata$cumweek,col=2,lty=2)

ggplot(cookdata)+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week,fill="band"),alpha=0.3)+
  scale_color_manual("",values="red")+
  scale_fill_manual("",values="grey12")+
  theme_classic()+
#  geom_line(aes(x=Week,y=Density0,color="red"))+
  facet_grid(Year~.)

logpred5=fullout[1,1]+cookdata$MIR0*fullout[2,1]+cookdata$TrapDensity*fullout[3,1]+cookdata$MIR0*cookdata$TrapDensity*fullout[5,1]
cookdata$pred5=exp(logpred5)+0.0001
cookdata$upred5=cookdata$MIR0/(1-cookdata$pred5)
cookdata$lpred5=cookdata$MIR0/(1+cookdata$pred5)
cookdata$Density10=cookdata$Density0*.1
logpred10=fullout[1,1]+cookdata$MIR0*fullout[2,1]+cookdata$Density10*fullout[3,1]+cookdata$MIR0*cookdata$Density10*fullout[5,1]
cookdata$pred10=exp(logpred10)+0.0001
cookdata$upred10=cookdata$MIR0/(1-cookdata$pred10)
cookdata$lpred10=cookdata$MIR0/(1+cookdata$pred10)

tiff("figure3.tif",width = 800,height = 800)
ggplot(cookdata)+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred10,ymax=upred10,x=Week,fill="10%"),alpha=0.3)+
  geom_ribbon(aes(ymin=lpred5,ymax=upred5,x=Week,fill="50%"),alpha=0.3)+
  guides(fill=guide_legend(title="Trap\nDensity"))+
  theme_classic()+
  scale_x_continuous(breaks = seq(from=20,to=41),limits = c(20,41))+
  theme(text=element_text(size = 12),axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  labs(x="Week",y=expression(MIR[0]))+
  facet_wrap(~Year)
dev.off()

cookdata$predrange5=cookdata$upred5-cookdata$lpred5

###
willdata=basedata[which(basedata$County=="WILL"),]
willdata=willdata[order(willdata$cumweek),]
willdata$Density0=willdata$TrapDensity/(willdata$p/100)
logpred=fullout[1,1]+willdata$MIR0*fullout[2,1]+willdata$Density0*fullout[3,1]+willdata$MIR0*willdata$Density0*fullout[5,1]
willdata$pred=exp(logpred)+0.0001
willdata$upred=willdata$MIR0/(1-willdata$pred)
willdata$lpred=willdata$MIR0/(1+willdata$pred)
logpred5=fullout[1,1]+willdata$MIR0*fullout[2,1]+willdata$TrapDensity*fullout[3,1]+willdata$MIR0*willdata$TrapDensity*fullout[5,1]
willdata$pred5=exp(logpred5)+0.0001
willdata$upred5=willdata$MIR0/(1-willdata$pred5)
willdata$lpred5=willdata$MIR0/(1+willdata$pred5)
willuse=which(willdata$Year<=2006)

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
lakeuse=which(lakedata$Year>2006&lakedata$Year<2016)

fig4dat=data.frame(County=c(rep("Will",length(willuse)),rep("Lake",length(lakeuse))),
                   Week=c(willdata$Week[willuse],lakedata$Week[lakeuse]),
                   Year=c(rep(c("Year 1","Year 2"),each=4),rep("Year 1",6),rep("Year 2",4)),
                   MIR0=c(willdata$MIR0[willuse],lakedata$MIR0[lakeuse]),
                   lpred=c(willdata$lpred[willuse],lakedata$lpred[lakeuse]),
                   upred=c(willdata$upred[willuse],lakedata$upred[lakeuse])
)

tiff("figure4.tif",width = 400,height = 400)
ggplot(fig4dat)+geom_line(aes(x=Week,y=MIR0))+
  geom_ribbon(aes(ymin=lpred,ymax=upred,x=Week),alpha=0.3,fill="green")+
  theme_classic()+
  scale_x_continuous(breaks = seq(from=23,to=36),limits = c(23,36))+
  theme(text=element_text(size = 12),axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  labs(x="Week",y=expression(MIR[0]))+
  facet_grid(County~Year)
dev.off()

fig4dat$predrange=fig4dat$upred-fig4dat$lpred

###############################

ggplot(cookdata)+geom_line(aes(x=Week,y=MIR))+
  geom_ribbon(aes(ymin=lpred5,ymax=upred5,x=Week,fill="band"),alpha=0.3)+
  scale_color_manual("",values="red")+
  scale_fill_manual("",values="grey12")+
  theme_classic()+
  #  geom_line(aes(x=Week,y=Density0,color="red"))+
  facet_grid(Year~.)

cook2012=mirc[which(mirc$County=="COOK"&mirc$Year==2012),]
cook2013=mirc[which(mirc$County=="COOK"&mirc$Year==2013),]
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




will2005=mirc[which(mirc$County=="WILL"&mirc$Year==2005),]
will2006=mirc[which(mirc$County=="WILL"&mirc$Year==2006),]
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
lake2007=mirc[which(mirc$County=="LAKE"&mirc$Year==2007),]
lake2015=mirc[which(mirc$County=="LAKE"&mirc$Year==2015),]
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
dupage2005=mirc[which(mirc$County=="DUPAGE"&mirc$Year==2005),]
dupage2006=mirc[which(mirc$County=="DUPAGE"&mirc$Year==2006),]
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

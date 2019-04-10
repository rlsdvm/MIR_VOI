mirc=read.csv("RandomMIRSamplingC.csv")

library(ggplot2)

ggplot(mirc,aes(x=Relative_Error_Absolute,group=p))+
  geom_histogram(aes(x=Relative_Error_Absolute,fill=p))

ggplot(mirc[which(mirc$County=="COOK"|mirc$County=="DUPAGE"),],aes(y=Relative_Error_Absolute,group=p,x=p))+
  geom_violin(fill="blue")+
  xlab("Proportion of Traps Sampled")+ylab(expression(E[p]))+
  theme_classic()

ggplot(mirc[which(mirc$County=="COOK"|mirc$County=="DUPAGE"),],aes(y=TrapDensity,colour=Relative_Error_Absolute,x=MIR0))+
  geom_jitter(shape=1)+
  xlab(expression(MIR[0]))+ylab("Trap Density")+
  labs(colour = expression(E[p]))+
  facet_wrap(~p)+
  theme_classic()+scale_color_gradient(low = "gray",high = "red")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00")
ggplot(mirc[which(mirc$County=="COOK"|mirc$County=="DUPAGE"),],aes(colour=factor(p),y=Relative_Error,x=MIR0))+
  geom_point()+labs(colour="Proportion of\nTraps Sampled")+
  xlab(expression(MIR[0]))+ylab("Relative Error")+
  theme_classic()+scale_color_manual(values = cbbPalette)

ggplot(mirc[which(mirc$County=="COOK"|mirc$County=="DUPAGE"),],aes(colour=MIR0,y=Relative_Error,x=TrapDensity))+
  geom_point()+labs(colour=expression(MIR[0]))+
  xlab("Trap Density")+ylab("Relative Error")+
  theme_classic()+scale_color_gradientn(colors = cbbPalette)

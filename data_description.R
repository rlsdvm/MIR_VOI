load("RandomMIRSamplingCountywithVariance.Rdata")

uID=unique(resultsC$ID)
baseData=matrix(NA,nrow=length(uID),ncol=ncol(resultsC));colnames(baseData)=colnames(resultsC)
for(i in 1:length(uID)){
  baseData[i,]=as.matrix(resultsC[which(resultsC$ID==uID[i])[1],],nrow=1)
}
baseData=as.data.frame(baseData)
baseData$Density0=as.numeric(as.character(baseData$TrapDensity))*2


baseData = setNames(data.table(matrix(ncol = 10, nrow = 0)), c("Year", "Week", "County","Positive Pools","Total Mosquitos","MIR","MIRL","MIRU","Traps","TrapDensity"))
#Initializes empty result tables

for (y in 2005:2016) {
  print(y)
  name=paste("mir",y,sep="")
  TableYear = get(name)
  weeks = unique(as.vector(TableYear$WEEK_NEW))
  weeks = weeks[!is.na(weeks)]
  for (w in weeks) {
    print(w)
    TableWeek = TableYear[which(TableYear$WEEK_NEW==w),]
    counties = unique(as.vector(TableWeek$FIPSCOUNTY))
    counties = counties[!is.na(counties)]
    for (cty in counties) {
      print(cty)
      table = TableWeek[which(TableWeek$FIPSCOUNTY==cty),]
      #table ends up being the data of county cty, week w and year y
      n = nrow(table)
      npos0 = sum(table[,"ISPOS"])
      pop0 = sum(table[,"POOLSIZE"])
      pb = pooledBin(table[,"ISPOS"],table[,"POOLSIZE"],pt.method = "mir",scale = 1000)
      MIR = pb$p 
      MIRL = pb$lcl 
      MIRU = pb$ucl 
      if (cty == "COOK                                              ") {cty = "COOK"}
      if (cty == "DUPAGE                                            ") {cty = "DUPAGE"}
      if (nrow(table)>=50 & (cty == "COOK" | cty == "DUPAGE")) { #Use only the weeks for which there are at least 50 obs
        area = 945
        if (cty == "DUPAGE") {area = 327}
        baseData = rbindlist(list(baseData, list(y,w,cty,npos0,pop0,MIR,MIRL,MIRU,n,n/area)))
      }
    }
  }
} # treats the data county-wise

save(baseData,file = "baseline_data.Rdata")

baseData$County=factor(baseData$County)
baseData$Date=baseData$Year+baseData$Week/100


library(ggplot2)
library(cowplot)

ggplot(baseData,aes(x=Week,y=MIR,ymin=MIRL,ymax=MIRU))+
  geom_line()+
  geom_ribbon(alpha=0.5)+
  facet_grid(County~Year)

mirplot=ggplot(baseData,aes(x=Week,y=MIR,ymin=MIRL,ymax=MIRU,color=County))+
  geom_pointrange(position = position_dodge2(preserve = "single",width = 1))+
  facet_grid(.~Year)+
#  geom_line(aes(y=TrapDensity/20))+
#  scale_y_continuous(sec.axis = sec_axis(~.*20, name = "Trap Density (/sq mi)"))+
  theme(legend.position = "bottom")

densityplot=ggplot(baseData,aes(x=Week,y=TrapDensity,color=County))+
  geom_point()+
  facet_grid(.~Year)+
  theme(legend.position = "none")

tiff("data_description.tif")
plot_grid(mirplot,densityplot,align = "h",nrow = 2)
dev.off()

library(tableone)
desctable=CreateTableOne(data = baseData[,c(3:6,9:10)],strata = c("County"))
dtable=print(desctable,nonnormal=c("Positive Pools","Total Mosquitos","MIR","Traps","TrapDensity"),test=F)
write.csv(dtable,file="descriptive_table.csv")



###########
simtable=matrix(NA,nrow = 5,ncol=6);colnames(simtable)=ps=seq(from=50,to=100,by=10)
rownames(simtable)=c("Positive Pools","Total Mosquitos","MIR","Traps","TrapDensity")
for(i in 1:5){
  idata=resultsC[which(resultsC$p==ps[i]),]
  simtable[1,i]=paste(median(idata$Npos)," [",quantile(idata$Npos,probs = 0.25),", ",quantile(idata$Npos,probs = 0.75),"]",sep="")
  simtable[2,i]=paste(median(idata$Pop)," [",quantile(idata$Pop,probs = 0.25),", ",quantile(idata$Pop,probs = 0.75),"]",sep="")
  simtable[3,i]=paste(signif(median(idata$MIR),2)," [",signif(quantile(idata$MIR,probs = 0.25),2),", ",signif(quantile(idata$MIR,probs = 0.75),2),"]",sep="")
  simtable[4,i]=paste(median(idata$Traps)," [",quantile(idata$Traps,probs = 0.25),", ",quantile(idata$Traps,probs = 0.75),"]",sep="")
  simtable[5,i]=paste(round(median(idata$TrapDensity),2)," [",round(quantile(idata$TrapDensity,probs = 0.25),2),", ",round(quantile(idata$TrapDensity,probs = 0.75),2),"]",sep="")
}
simtable[1,6]=paste(median(baseData$"Positive Pools")," [",quantile(baseData$"Positive Pools",probs = 0.25),", ",quantile(baseData$"Positive Pools",probs = 0.75),"]",sep="")
simtable[2,6]=paste(median(baseData$"Total Mosquitos")," [",quantile(baseData$"Total Mosquitos",probs = 0.25),", ",quantile(baseData$"Total Mosquitos",probs = 0.75),"]",sep="")
simtable[3,6]=paste(signif(median(baseData$MIR),2)," [",signif(quantile(baseData$MIR,probs = 0.25),2),", ",signif(quantile(baseData$MIR,probs = 0.75),2),"]",sep="")
simtable[4,6]=paste(median(baseData$Traps)," [",quantile(baseData$Traps,probs = 0.25),", ",quantile(baseData$Traps,probs = 0.75),"]",sep="")
simtable[5,6]=paste(round(median(baseData$TrapDensity),2)," [",round(quantile(baseData$TrapDensity,probs = 0.25),2),", ",round(quantile(baseData$TrapDensity,probs = 0.75),2),"]",sep="")
write.csv(simtable,file="data_by_proportion.csv")

summary(baseData$MIR[which(baseData$County=="COOK")])
summary(baseData$MIR[which(baseData$County=="DUPAGE")])

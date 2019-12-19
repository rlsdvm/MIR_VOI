#Lake, McHenry, and Will
mirlw=read.csv("RandomMIRSamplingC.csv")
library(binGroup)
smcty=levels(mirlw$County)[3:5]

mirlw=mirlw[which(mirlw$County%in%smcty),]
mirlw$County=droplevels(mirlw$County)

MIR0=MIR0L=MIR0U=rep(NA,nrow(mirlw))
for(i in 1:nrow(mirlw)){
pb = pooledBin(mirlw$Npos0[i],mirlw$Pop0[i],pt.method = "mir",scale = 1000)
MIR0[i] = pb$p 
MIR0L[i] = pb$lcl 
MIR0U[i] = pb$ucl 
}
mirlw$MIR0=MIR0
mirlw$MIR0U=MIR0U
mirlw$MIR0L=MIR0L
save(mirlw,file="SmallCountyObservations.Rdata")
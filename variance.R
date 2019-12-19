library(binGroup)

require(data.table)

for (y in 2005:2016) {
  name = paste("mir",y,sep = "")
  temp = read.csv(paste("MIR",y,".csv",sep = ""))
  temp = temp[!is.na(temp$WEEK_NEW),]
  assign(name, temp)
}
#Loads all data files into dataframes

resultsC = setNames(data.table(matrix(ncol = 20, nrow = 0)), c("p","Year", "Week", "County","Npos0","Dpos0","Pop0","MIR0","MIR0L","MIR0U","Npos","Dpos","Pop","MIR","MIRL","MIRU","Standard_Deviation","Relative_Error","Traps","TrapDensity"))
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
      npos0 = sum(table[,"ISPOS"])
      pop0 = sum(table[,"POOLSIZE"])
      pb = pooledBin(table[,"ISPOS"],table[,"POOLSIZE"],pt.method = "mir",scale = 1000)
      MIR0 = pb$p 
      MIR0L = pb$lcl 
      MIR0U = pb$ucl 
      if (cty == "COOK                                              ") {cty = "COOK"}
      if (cty == "DUPAGE                                            ") {cty = "DUPAGE"}
      if (nrow(table)>=50 & (cty == "COOK" | cty == "DUPAGE")) { #Use only the weeks for which there are at least 50 obs
        for (p in ((5:9)*10)) {
          n = nrow(table)
          m = round(p/100*nrow(table))
          for (k in 1:100) {
            extraction = sample(1:n, m)
            #Computes a random vector (extraction) of p% of the indices of the table
            npos = sum(table[extraction,"ISPOS"])
            pop = sum(table[extraction,"POOLSIZE"])
            pb = pooledBin(table[extraction,"ISPOS"],table[extraction,"POOLSIZE"],pt.method = "mir",scale = 1000)
            MIR = pb$p 
            MIRL = pb$lcl 
            MIRU = pb$ucl             
            error = (MIR-MIR0)/MIR0
            if (MIR0 == 0) {
              error = 0
            } #Discard the relative error if MIR0 = 0
            std = 1;
            if (npos > 1) {
              std = (1+1/pop) * 1/(npos-1) * (1+1/pop - npos/pop);
            }
            std = sqrt(std)
            area = 945
            if (cty == "DUPAGE") {area = 327}
            resultsC = rbindlist(list(resultsC, list(p,y,w,cty,npos0,npos0/n,pop0,MIR0,MIR0L,MIR0U,npos,npos/m,pop,MIR,MIRL,MIRU,std,error,m,m/area)))
            #Prints the result in a new row of the dataframe results
          }
        }
      }
    }
  }
} # treats the data county-wise
resultsC$MIR0range=resultsC$MIR0U-resultsC$MIR0L
resultsC$MIRrange=resultsC$MIRU-resultsC$MIRL
resultsC$rangeDiff=resultsC$MIRrange-resultsC$MIR0range
resultsC$ID=paste(resultsC$Year,resultsC$Week,resultsC$County,sep="_")
resultsC$ID=as.factor(resultsC$ID)
resultsC$Relative_Error_Absolute=abs(resultsC$Relative_Error)
resultsC$County=factor(resultsC$County)

write.csv(resultsC, file = "RandomMIRSamplingCountywithVariance.csv", row.names=FALSE)
save(resultsC, file = "RandomMIRSamplingCountywithVariance.Rdata")

library(ggplot2)
ggplot(resultsC,aes(x=TrapDensity,y=rangeDiff))+
  geom_point()

ggplot(resultsC,aes(x=MIR0,y=rangeDiff,color=TrapDensity))+
  geom_point(alpha=0.5)


require(data.table)

for (y in 2005:2016) {
  name = paste("mir",y,sep = "")
  temp = read.csv(paste("MIR",y,".csv",sep = ""))
  temp = temp[!is.na(temp$WEEK_NEW),]
  #temp[temp=="DUPAGE                                            "] <- "DUPAGE"
  #temp[temp=="COOK                                              "] <- "COOK"
  #temp[temp=="WILL                                              "] <- "WILL"
  #temp[temp=="LAKE                                              "] <- "LAKE"
  assign(name, temp)
}
#Loads all data files into dataframes

# for (y in 2005:2016) {
#   name = paste("mir",y,sep = "")
#   temp = get(name)
#   print(name)
#   print(nrow(temp))
#   print(unique(as.vector(temp$FIPSCOUNTY)))
# }
  

resultsW = setNames(data.table(matrix(ncol = 14, nrow = 0)), c("p","Year", "Week", "County","Npos0","Dpos0","Pop0","MIR0","Npos","Dpos","Pop","MIR","Standard_Deviation","Relative_Error"))
resultsC = setNames(data.table(matrix(ncol = 16, nrow = 0)), c("p","Year", "Week", "County","Npos0","Dpos0","Pop0","MIR0","Npos","Dpos","Pop","MIR","Standard_Deviation","Relative_Error","Traps","TrapDensity"))
#Initializes empty result tables

for (y in 2005:2016) {
  print(y)
  name=paste("mir",y,sep="")
  TableYear = get(name)
  weeks = unique(as.vector(TableYear$WEEK_NEW))
  weeks = weeks[!is.na(weeks)]
  for (w in weeks) {
    print(w)
    table = TableYear[which(TableYear$WEEK_NEW==w),]
    #table ends up being the data of week w and year y
    npos0 = sum(table[,"ISPOS"])
    pop0 = sum(table[,"POOLSIZE"])
    MIR0 = npos0 / pop0 *1000
    if (nrow(table)>=50) { #Use only the weeks for which there are at least 50 obs
      for (p in ((5:9)*10)) {
        n = nrow(table)
        m = round(p/100*nrow(table))
        for (k in 1:100) {
          extraction = sample(1:n, m)
          #Computes a random vector (extraction) of p% of the indices of the table
          npos = sum(table[extraction,"ISPOS"])
          pop = sum(table[extraction,"POOLSIZE"])
          MIR = npos / pop *1000
          error = (MIR-MIR0)/MIR0
          if (MIR0 == 0) {
            error = 0
          } #Discard the relative error if MIR0 = 0
          std = 1
          if (npos > 1) {
            std = (1+1/pop) * 1/(npos-1) * (1+1/pop - npos/pop)
          }
          std = sqrt(std)
          resultsW = rbindlist(list(resultsW, list(p,y,w,"COUNTY",npos0,npos0/n,pop0,MIR0,npos,npos/m,pop,MIR,std,error)))
          #Prints the result in a new row of the dataframe results
        }
      }
    }
  }
} # treats the data week-wise

plot(Relative_Error~Npos,type="p",data=resultsW[which(resultsW$p==50 & resultsW$Npos >= 50),],col=alpha(50,0.2))     #first set, p=0.5
for(i in (6:9)*10) {points(Relative_Error~Npos,col=alpha(i,0.2),data=resultsW[which(resultsW$p==i & resultsW$Npos >= 50),])}    #remaining sets
#Plots scatter plot, not county-wise

plot(Relative_Error~Npos,type="p",data=resultsC[which(resultsC$p==50),])     #first set, p=0.5
for(i in (6:9)*10) {points(Relative_Error~Npos,col=i,data=resultsC[which(resultsC$p==i),])}    #remaining sets
#Plots scatter plot, not county-wise

plot(Relative_Error~Standard_Deviation,type="p",data=resultsW[which(resultsW$p==50 & resultsW$Standard_Deviation<=0.23),],col=alpha(50,0.2))     #first set, p=0.5
for(i in (8:9)*10) {points(Relative_Error~Standard_Deviation,col=alpha(i,0.2),data=resultsW[which(resultsW$p==i & resultsW$Standard_Deviation<=0.23),])}    #remaining sets
#Plots scatter plot, not county-wise

plot(Relative_Error~Standard_Deviation,type="p",data=resultsC[which(resultsC$p==50),])     #first set, p=0.5
for(i in (6:9)*10) {points(Relative_Error~Standard_Deviation,col=i,data=resultsC[which(resultsC$p==i),])}    #remaining sets
#Plots scatter plot, not county-wise


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
      MIR0 = npos0 / pop0 *1000
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
            MIR = npos / pop *1000
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
            resultsC = rbindlist(list(resultsC, list(p,y,w,"COUNTY",npos0,npos0/n,pop0,MIR0,npos,npos/m,pop,MIR,std,error,m,m/area)))
            #Prints the result in a new row of the dataframe results
          }
        }
      }
    }
  }
} # treats the data county-wise

plot(Relative_Error~TrapDensity,type="p",data=resultsC[which(resultsC$p==50),],col=alpha(50,0.2))     #first set, p=0.5
for(i in (6:9)*10) {points(Relative_Error~TrapDensity,col=alpha(i,0.2),data=resultsC[which(resultsC$p==i),])}    #remaining sets
  #Plots scatter plot, not county-wise

# for (p in (5:9 * 10)) {
#   #for p in c(10,20,...,90)
#   for (y in 2005:2016) {
#     name=paste("mir",y,sep="")
#     print(p)
#     print(y)
#     TableYear = get(name)
#     weeks = unique(as.vector(TableYear$WEEK_NEW))
#     weeks = weeks[!is.na(weeks)]
#     for (w in weeks) {
#       TableWeek = TableYear[which(TableYear$WEEK_NEW==w),]
#       #TableWeek ends up being the data of week w and year y
#       counties = unique(as.vector(TableWeek$FIPSCOUNTY))
#       counties = counties[!is.na(counties)]
#       print(w)
#       for (cty in counties) {
#         table = TableWeek[which(TableWeek$FIPSCOUNTY==cty),]
#         MIR0 = sum(table[,"ISPOS"])/sum(table[,"POOLSIZE"])*1000
#         #Doesn't use ALL weeks, only the one for which there are at least 50 obs
#         if (nrow(table)>=50) {
#           for (k in 1:100) {
#             extraction = sample(1:nrow(table), round(p/100*nrow(table)))
#             #Computes a random vector (extraction) of p% of the indices of the table
#             MIR = sum(table[extraction,"ISPOS"])/sum(table[extraction,"POOLSIZE"])*1000
#             error = (MIR-MIR0)/MIR0
#             if (MIR0 == 0) {
#               error = 0
#             }
#             #Discard the relative error if MIR0 = 0
#             resultsC = rbindlist(list(resultsC, list(p,y,w,cty,MIR,MIR0,error)))
#             #results[nrow(results) + 1,] = list(p,y,w,cty,MIR,MIR0,error)
#             #Prints the result in a new row of the dataframe results
#           }
#         }
#       }
#     }
#   }
# } #treats the data county-wise

resultsB = resultsC
resultsC[resultsC=="DUPAGE                                            "] <- "DUPAGE"
resultsC[resultsC=="COOK                                              "] <- "COOK"
resultsC[resultsC=="WILL                                              "] <- "WILL"
resultsC[resultsC=="LAKE                                              "] <- "LAKE"
#To correct some county names

write.csv(resultsW, file = "RandomMIRSampling.csv", row.names=FALSE)
write.csv(resultsC, file = "RandomMIRSamplingCounty.csv", row.names=FALSE)
#Prints the results into a csv file

boxplot(Relative_Error~p,data=resultsW[which(resultsW$p>=50),],main="Relative error per sample size",xlab="p, percentage of sampled data",ylab="Relative error", range = 0)
#boxplot((MIR-MIR0)/MIR0~p,data=resultsW[which(resultsW$p>=50),],main="Relative error per sample size",xlab="p, percentage of sampled data",ylab="Relative error")

boxplot(Relative_Error~p,data=resultsC[which(resultsC$p>=50),],main="Relative error per sample size, county-wise",xlab="p, percentage of sampled data",ylab="Relative error", range = 0)
#boxplot((MIR-MIR0)/MIR0~p,data=resultsC[which(resultsC$p>=50),],main="Relative error per sample size, county-wise",xlab="p, percentage of sampled data",ylab="Relative error")

library(ggplot2)
#ggplot(results[which(results$p>=50),], aes(factor(p), (MIR-MIR0)/MIR0)) + geom_violin(aes(fill = factor(p)))

ggplot(resultsW[which(resultsW$p>=50),], aes(factor(p), Relative_Error)) + geom_violin(aes(fill = factor(p)))
ggplot(resultsC[which(resultsC$p>=50),], aes(factor(p), Relative_Error)) + geom_violin(aes(fill = factor(p)))

plot(Relative_Error~MIR0,type="p",data=resultsW[which(resultsW$p==50),])     #first set, p=0.1
for(i in (6:9)*10) {points(Relative_Error~MIR0,col=i,data=resultsW[which(resultsW$p==i),])}    #remaining sets

plot(Relative_Error~MIR0,type="p",data=resultsC[which(resultsC$p==50),])     #first set, p=0.1
for(i in (6:9)*10) {points(Relative_Error~MIR0,col=i,data=resultsC[which(resultsC$p==i),])}    #remaining sets
#Plots scatter plot, not county-wise

for (i in (5:9)*10) {
  print(ggplot(resultsC[which(resultsC$p==i),], aes(factor(County), Relative_Error)) + geom_violin(aes(fill = factor(County))) + ggtitle(paste("Sampling per county per week, at ",i,"% sampled", sep="")))
}
#Prints violin plot, at each p fixed

for (cty in unique(resultsC$County)) {
  print(ggplot(resultsC[which(resultsC$County==cty),], aes(factor(p), Relative_Error)) + geom_violin(aes(fill = factor(p))) + ggtitle(paste("Sampling per week of county ", cty, sep="")))
}
#Prints violin plot, at each county fixed

for (cty in unique(resultsC$County)) {
  plot(Relative_Error~MIR0,type="p",data=resultsC[which(resultsC$p==50 & resultsC$County==cty),],main=paste("Sampling per week of county ", cty, sep=""))     #first set, p=0.1
  for(i in (6:9)*10) {points(Relative_Error~MIR0,col=i,data=resultsC[which(resultsC$p==i & resultsC$County==cty),])}    #remaining sets
}
#Prints scatter plot, for each county fixed
#ggplot(results[which(results$p>=50),], aes(factor(p), (MIR-MIR0)/MIR0)) + geom_violin(aes(fill = p))

#mch = resultsC[resultsC$County=="MCHENRY"]
#unique(as.vector(mch$))

#results2706 = results

#colnames(resultsW)[7] <- "Relative_Error"

plot(Relative_Error~npos,type="p",data=resultsC[which(resultsC$p==50),])     #first set, p=0.1
for(i in (6:9)*10) {points(Relative_Error~npos,col=i,data=resultsC[which(resultsC$p==i),])}    #remaining sets
#Plots scatter plot, not county-wise


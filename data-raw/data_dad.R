



baseDir    <- "u:/RhineFlow/Elevation/"
setwd(baseDir)


load(paste0(baseDir,"/Data/tempData.RData"))
load(paste0(baseDir,"/Data/snowData.RData"))
load(paste0(baseDir,"/Data/clouData.RData"))
load(paste0(baseDir,"/Data/radiData.RData"))
load(paste0(baseDir,"/Data/sunsData.RData"))
load(paste0(baseDir,"/Data/presData.RData"))
load(paste0(baseDir,"/Data/rainData.RData"))
load(paste0(baseDir,"/Data/rainData2.RData"))
stationMeta <- read.table(paste0(baseDir,"/Data/rawData/stationsMeta.txt"), sep=";", header=T)

statIDs <- c("SMA", "STG", "SAE", "DAV")

f_statSelect <- function(dataIn, statIDs){
  
  for(i in 1:length(statIDs)){
    if(i==1){cSelect=1}#date column
    if(length(which(grepl(statIDs[i], colnames(dataIn))) > 0)){
      cSelect <- c(cSelect, which(grepl(statIDs[i], colnames(dataIn))))
    }
  }
  
  dataIn <- dataIn[,cSelect]  
  
} 

tempData <- f_statSelect(tempData, statIDs)
snowData <- f_statSelect(snowData, statIDs)
radiData <- f_statSelect(radiData, statIDs)
sunsData <- f_statSelect(sunsData, statIDs)
clouData <- f_statSelect(clouData, statIDs)
presData <- f_statSelect(presData, statIDs)
rainData <- f_statSelect(rainData, statIDs)
rainData <- f_statSelect(rainData2, statIDs)
stationMeta <- stationMeta[c(3,8,21,25),]

ahumData <- presData
for(i in 1:length(statIDs)){
  
  ahumData[,which(grepl(paste0(statIDs[i]), colnames(presData)))] <- (100*presData[,which(grepl(paste0(statIDs[i]), colnames(presData)))]) / 
    (461.5* (tempData[, which(grepl(paste0(statIDs[i],"mea"), colnames(tempData)))] + 273.15) )*1000
}


write.csv(tempData, file="tempData.csv",row.names=F)
write.csv(snowData, file="snowData.csv",row.names=F)
write.csv(radiData, file="radiData.csv",row.names=F)
write.csv(sunsData, file="sunsData.csv",row.names=F)
write.csv(clouData, file="clouData.csv",row.names=F)
write.csv(ahumData, file="ahumData.csv",row.names=F)
write.csv(rainData, file="rainData.csv",row.names=F)
write.csv(rainData, file="rainData2.csv",row.names=F)
write.csv(stationMeta, file="stationMeta.csv",row.names=F)



#Discharge data

load("u:/RhineFlow/Elevation/Data/rawData/dismeta.Rdata")

statNames <- c("Rekingen", "Neuhausen", "Diepoldsau")

cols <- c(1, which(colnames(dis) == statNames[1]), which(colnames(dis) == statNames[2]), which(colnames(dis) == statNames[3]))

discData <- dis[,cols]

rows <- c(which(meta$name == statNames[1]), which(meta$name == statNames[2]), which(meta$name == statNames[3]))

discMeta <- meta[rows,]

write.csv(discMeta, file="discMeta.csv",row.names=F)

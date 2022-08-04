logregFPO<-read.csv("C:/Users/17163/Documents/Disc golf analytics/Elo ratings/logregFPO.csv",header=T)

ey <- unique(logregFPO[c("Ev","year")])
eyy <-ey[ey$year==ey[nrow(ey),]$year,]
leyy<-nrow(eyy)

logregFPO_sub1<-subset(logregFPO,logregFPO$Ev==eyy$Ev[1] & logregFPO$year==eyy$year[1])
logregFPO_sub1$Etot<-rowSums(cbind(logregFPO_sub1$E,logregFPO_sub1$Et),na.rm=T)
logregFPO_sub1<-logregFPO_sub1[,c(5,9,4)]
logregFPO_sub1<-logregFPO_sub1[order(logregFPO_sub1$Etot,decreasing=T),][1:25,]

logregFPO_sub2<-subset(logregFPO,logregFPO$Ev==eyy$Ev[2] & logregFPO$year==eyy$year[2])
logregFPO_sub2$Etot<-rowSums(cbind(logregFPO_sub2$E,logregFPO_sub2$Et),na.rm=T)
logregFPO_sub2<-logregFPO_sub2[,c(5,9,4)]
logregFPO_sub2<-logregFPO_sub2[order(logregFPO_sub2$Etot,decreasing=T),][1:25,]

for (j in 1:nrow(logregFPO_sub1)) {
  if (logregFPO_sub1$name[j] %in% logregFPO_sub2$name) {
    logregFPO_sub1$Etot[j] <- logregFPO_sub2$Etot[match(logregFPO_sub1$name[j],logregFPO_sub2$name)]
    logregFPO_sub1$Et[j] <- logregFPO_sub2$Et[match(logregFPO_sub1$name[j],logregFPO_sub2$name)]
  }
}

logregFPO_sub2 <- logregFPO_sub2[!logregFPO_sub2$name %in% logregFPO_sub1$name,]
logregFPO_rank <- rbind(logregFPO_sub1,logregFPO_sub2)
logregFPO_rank <- logregFPO_rank[order(logregFPO_rank$Etot,decreasing=T),][1:25,]
for (i in 3:leyy) {
  logregFPO_sub1<-subset(logregFPO,logregFPO$Ev==eyy$Ev[i] & logregFPO$year==eyy$year[i])
  logregFPO_sub1$Etot<-rowSums(cbind(logregFPO_sub1$E,logregFPO_sub1$Et),na.rm=T)
  logregFPO_sub1<-logregFPO_sub1[,c(5,9,4)]
  logregFPO_sub1<-logregFPO_sub1[order(logregFPO_sub1$Etot,decreasing=T),][1:25,]
  
  for (j in 1:nrow(logregFPO_rank)) {
    if (logregFPO_rank$name[j] %in% logregFPO_sub1$name) {
      logregFPO_rank$Etot[j] <- logregFPO_sub1$Etot[match(logregFPO_rank$name[j],logregFPO_sub1$name)]
      logregFPO_rank$Et[j] <- logregFPO_sub1$Et[match(logregFPO_rank$name[j],logregFPO_sub1$name)]
    }
  }
  
  logregFPO_sub1 <- logregFPO_sub1[!logregFPO_sub1$name %in% logregFPO_rank$name,]
  
  logregFPO_rank <- rbind(logregFPO_rank,logregFPO_sub1)
  logregFPO_rank <- logregFPO_rank[order(logregFPO_rank$Etot,decreasing=T),][1:25,]
}

logregFPO_data_25 <- logregFPO[logregFPO$name %in% logregFPO_rank$name,]
logregFPO_data_25$Etot <- rowSums(cbind(logregFPO_data_25$E,logregFPO_data_25$Et),na.rm=T)
logregFPO_data_25 <- logregFPO_data_25[,c(5,6,7,8,9,4)]
logregFPO_data_25 <- subset(logregFPO_data_25,logregFPO_data_25$year>2017)
logregFPO_data_25 <- logregFPO_data_25[logregFPO_data_25$Etot!=0,]
logregFPO_data_25$Etot <- round(logregFPO_data_25$Etot,0)
logregFPO_data_25$Et <- round(logregFPO_data_25$Et,0)
logregFPO_data_25$date<-as.Date(logregFPO_data_25$date,format="%d-%m-%Y")
names(logregFPO_data_25) <- c("Name","Event","Year","Date","Elo","dElo")

logregFPO_rank$Etot <- round(logregFPO_rank$Etot,0)
logregFPO_rank$Et <- round(logregFPO_rank$Et,0)
names(logregFPO_rank) <- c("Name","Elo","dElo")

write.csv(logregFPO_data_25,"EloDashDataFPO.csv")
write.csv(logregFPO_rank,"EloRank25FPO.csv")

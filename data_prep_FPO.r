logregFPO<-read.csv("C:/Users/17163/Documents/Disc golf analytics/Elo ratings/logregFPO.csv",header=T)

ey <- unique(logregFPO[c("Ev","year")])
eyy <-ey[ey$year==ey[nrow(ey),]$year,]
leyy<-nrow(eyy)

logregFPO_sub1<-subset(logregFPO,logregFPO$Ev==eyy$Ev[1] & logregFPO$year==eyy$year[1] & logregFPO$E!="NA")
logregFPO_sub1$Etot<-rowSums(cbind(logregFPO_sub1$E,logregFPO_sub1$Et),na.rm=T)
logregFPO_sub1<-logregFPO_sub1[,c(5,9,4)]
logregFPO_sub1<-logregFPO_sub1[order(logregFPO_sub1$Etot,decreasing=T),][1:25,]

logregFPO_sub2<-subset(logregFPO,logregFPO$Ev==eyy$Ev[2] & logregFPO$year==eyy$year[2] & logregFPO$E!="NA")
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
  logregFPO_sub1<-subset(logregFPO,logregFPO$Ev==eyy$Ev[i] & logregFPO$year==eyy$year[i] & logregFPO$E!="NA")
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
logregFPO_data_25$Etot <- round(logregFPO_data_25$Etot,1)
logregFPO_data_25$Et <- round(logregFPO_data_25$Et,1)
logregFPO_data_25$date<-as.Date(logregFPO_data_25$date,format="%d-%m-%Y")
names(logregFPO_data_25) <- c("Name","Event","Year","Date","Elo","dElo")

logregFPO_rank$Etot <- round(logregFPO_rank$Etot,1)
logregFPO_rank$Et <- round(logregFPO_rank$Et,1)
names(logregFPO_rank) <- c("Name","Elo","dElo")

write.csv(logregFPO_data_25,"EloDashDataFPO.csv")
write.csv(logregFPO_rank,"EloRank25FPO.csv")


### Win probabilities###

## win probs for ratings and players
logregFPO_prob<-subset(logregFPO,logregFPO$year>=2020 & logregFPO$E>1680)
logregFPO_prob$P[logregFPO_prob$P!=1]<-0
outf<-glm(P~E,data=logregFPO_prob,family=binomial)
summary(outf)

## extract coefs for players
#not using these for now (could be framework for code needed later)
mod_p_coefs<-coef(outf)[-c(1:2)]
mod_p_names<-substr(names(coef(outf)[-c(1:2)]),5,length(names(coef(outf)[-c(1:2)])))
names(mod_p_coefs)<-mod_p_names
mod_p_coefs<-mod_p_coefs[mod_p_names %in% logregFPO_rank$Name]
rank_p_coefs<-mod_p_coefs[match(logregFPO_rank$Name,names(mod_p_coefs))]

## determine which players in top-25 are playing next event
require(rvest)

##### PDGA event number MUST CHANGE EACH TIME #####
eventnum<-65206
###################################################


event <- read_html(paste("https://www.pdga.com/tour/event/",eventnum,sep=""))#
name<-event %>%
  html_nodes(".player") %>%
  html_text()

name[which(name=="Eagle Wynne McMahon")]<-"Eagle McMahon"
name[which(name=="Eagle Mcmahon")]<-"Eagle McMahon"
name[which(name=="Paul Mcbeth")]<-"Paul McBeth"
name[which(name=="Nathan Sexton")]<-"Nate Sexton"
name[which(name=="Nathan Doss")]<-"Nate Doss"
name[which(name=="Karl johan Nybo")]<-"Karl Johan Nybo"
name[which(name=="Joshua Anthon")]<-"Josh Anthon"
name[which(name=="Benjamin Callaway")]<-"Ben Callaway"

#subset coefs for players playing next event
logregFPO_rank_playing<-logregFPO_rank[logregFPO_rank$Name %in% name,]
#rank_p_playing_coefs<-rank_p_coefs[logregFPO_rank$Name %in% name]

#calculate win probs
prob1<-exp(coef(outf)[1] + logregFPO_rank_playing$Elo*coef(outf)[2])/(1+exp(coef(outf)[1] + logregFPO_rank_playing$Elo*coef(outf)[2]))

prob_win<-prob1/sum(prob1)
prob_win<-round(prob_win,3)*100
prob_win[prob_win==0.0]<-"<0.1"

#Win prob table
win_dataFPO<-data.frame(logregFPO_rank_playing$Name,prob_win,logregFPO_rank_playing$Elo)
names(win_dataFPO)<-c("Players","Win_Prob","Elo")

write.csv(win_dataFPO,"WinProbFPO.csv")

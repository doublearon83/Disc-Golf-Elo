
logreg<-read.csv("C:/Users/17163/Documents/Disc golf analytics/Elo ratings/logreg.csv",header=T)

ey <- unique(logreg[c("Ev","year")])
eyy <-ey[ey$year==ey[nrow(ey),]$year,]
leyy<-nrow(eyy)

logreg_sub1<-subset(logreg,logreg$Ev==eyy$Ev[1] & logreg$year==eyy$year[1] & logreg$E!="NA")
logreg_sub1$Etot<-rowSums(cbind(logreg_sub1$E,logreg_sub1$Et),na.rm=T)
logreg_sub1<-logreg_sub1[,c(5,9,4)]
logreg_sub1<-logreg_sub1[order(logreg_sub1$Etot,decreasing=T),][1:25,]

logreg_sub2<-subset(logreg,logreg$Ev==eyy$Ev[2] & logreg$year==eyy$year[2] & logreg$E!="NA")
logreg_sub2$Etot<-rowSums(cbind(logreg_sub2$E,logreg_sub2$Et),na.rm=T)
logreg_sub2<-logreg_sub2[,c(5,9,4)]
logreg_sub2<-logreg_sub2[order(logreg_sub2$Etot,decreasing=T),][1:25,]

for (j in 1:nrow(logreg_sub1)) {
  if (logreg_sub1$name[j] %in% logreg_sub2$name) {
    logreg_sub1$Etot[j] <- logreg_sub2$Etot[match(logreg_sub1$name[j],logreg_sub2$name)]
    logreg_sub1$Et[j] <- logreg_sub2$Et[match(logreg_sub1$name[j],logreg_sub2$name)]
  }
}

logreg_sub2 <- logreg_sub2[!logreg_sub2$name %in% logreg_sub1$name,]
logreg_rank <- rbind(logreg_sub1,logreg_sub2)
logreg_rank <- logreg_rank[order(logreg_rank$Etot,decreasing=T),][1:25,]
for (i in 3:leyy) {
  logreg_sub1<-subset(logreg,logreg$Ev==eyy$Ev[i] & logreg$year==eyy$year[i] & logreg$E!="NA")
  logreg_sub1$Etot<-rowSums(cbind(logreg_sub1$E,logreg_sub1$Et),na.rm=T)
  logreg_sub1<-logreg_sub1[,c(5,9,4)]
  logreg_sub1<-logreg_sub1[order(logreg_sub1$Etot,decreasing=T),][1:25,]
  
  for (j in 1:nrow(logreg_rank)) {
    if (logreg_rank$name[j] %in% logreg_sub1$name) {
      logreg_rank$Etot[j] <- logreg_sub1$Etot[match(logreg_rank$name[j],logreg_sub1$name)]
      logreg_rank$Et[j] <- logreg_sub1$Et[match(logreg_rank$name[j],logreg_sub1$name)]
    }
  }
  
  logreg_sub1 <- logreg_sub1[!logreg_sub1$name %in% logreg_rank$name,]
  
  logreg_rank <- rbind(logreg_rank,logreg_sub1)
  logreg_rank <- logreg_rank[order(logreg_rank$Etot,decreasing=T),][1:25,]
}
  
logreg_data_25 <- logreg[logreg$name %in% logreg_rank$name,]
logreg_data_25$Etot <- rowSums(cbind(logreg_data_25$E,logreg_data_25$Et),na.rm=T)
logreg_data_25 <- logreg_data_25[,c(5,6,7,8,9,4)]
logreg_data_25 <- subset(logreg_data_25,logreg_data_25$year>2016)
logreg_data_25 <- logreg_data_25[logreg_data_25$Etot!=0,]
logreg_data_25$Etot <- round(logreg_data_25$Etot,1)
logreg_data_25$Et <- round(logreg_data_25$Et,1)
logreg_data_25$date<-as.Date(logreg_data_25$date,format="%d-%m-%Y")
names(logreg_data_25) <- c("Name","Event","Year","Date","Elo","dElo")

logreg_rank$Etot <- round(logreg_rank$Etot,1)
logreg_rank$Et <- round(logreg_rank$Et,1)
names(logreg_rank) <- c("Name","Elo","dElo")

write.csv(logreg_data_25,"EloDashData.csv")
write.csv(logreg_rank,"EloRank25.csv")


### Win probabilities###

## win probs for ratings and players
logreg_prob<-subset(logreg,logreg$year>=2020 & logreg$E>1700)
logreg_prob$P[logreg_prob$P!=1]<-0
outf<-glm(P~E+name,data=logreg_prob,family=binomial)
summary(outf)

## extract coefs for players
mod_p_coefs<-coef(outf)[-c(1:2)]
mod_p_names<-substr(names(coef(outf)[-c(1:2)]),5,length(names(coef(outf)[-c(1:2)])))
names(mod_p_coefs)<-mod_p_names
mod_p_coefs<-mod_p_coefs[mod_p_names %in% logreg_rank$Name]
rank_p_coefs<-mod_p_coefs[match(logreg_rank$Name,names(mod_p_coefs))]

## determine which players in top-25 are playing next event
require(rvest)

##### PDGA event number MUST CHANGE EACH TIME #####
eventnum<-55592
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
logreg_rank_playing<-logreg_rank[logreg_rank$Name %in% name,]
rank_p_playing_coefs<-rank_p_coefs[logreg_rank$Name %in% name]

#calculate win probs
prob1<-exp(coef(outf)[1] + logreg_rank_playing$Elo*coef(outf)[2] + rank_p_playing_coefs)/(1+exp(coef(outf)[1] + logreg_rank_playing$Elo*coef(outf)[2] + rank_p_playing_coefs))

prob_win<-prob1/sum(prob1)
prob_win<-round(prob_win,3)*100

#Win prob table
win_data<-data.frame(names(prob_win),as.numeric(prob_win),logreg_rank_playing$Elo)
names(win_data)<-c("Players","Win_Prob","Elo")

write.csv(win_data,"WinProb.csv")

datatable(win_data,
          class = "compact cell-border",
          rownames = FALSE,
          options = list(order = list(1, "desc"),
                         autoWidth = FALSE,
                         pageLength = 25,
                         columnDefs = list(list(className = 'dt-center', targets = 1:2))))

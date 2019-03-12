
rm(list=ls())
setwd("~documents/Sparrows/Code")
library(igraph)
library(dplyr)
library(plyr)
library(rptR)

#Read in collective data from every aviary:
total_interaction <- read.csv("Sparrows/total_interaction.csv")
data<-total_interaction
str(data)

#create null dataframe 
Result_Dataset <- data.frame(NULL)
RData<-data.frame(NULL)


#Create for loop to calculate measures of centrality (degree, closeness, betweenness) for each occasion (i) and every individual. 
#el creates a matrix that extracts the two columns of individuals. 
#newGraph pairs individuals from the same row to show they interact. 
#directed=FALSE means neither individual initiated the interaction.
#f creates another data frame containing functions that will calculate measures of centrality
#for each individual. rownames<-NULL is used to remove the additional individual name 
#attached to degree. rbind will add this new data frame (f) on the bottom of my initial table.

library(lme4)
kk<-seq(1:25)
for (k in kk){
  Result_Dataset <- data.frame(NULL)  
  
  for (j in unique(data$occasion)){
    a <- subset(data, occasion == j)
    av <- unique(a$aviary)
    occ <- j
    for (i in av){
      b<-data.frame(NULL)
      b <- subset(a, a$aviary == i)
      len<-length(b$id1)
      Posid1<-unique(b$id1)
      Posid2<-unique(b$id2)
      rid1<-sample(Posid1, size=len, replace=TRUE)
      rid2<-sample(Posid2, size=len, replace=TRUE)
      df<-data.frame(rid1,rid2)
      el<-as.matrix(df)
      # the above is the permutation
      net <- graph.data.frame(el, directed=FALSE)
      E(net)$weight <- 1
      net2 <- simplify(net, edge.attr.comb = list(weight="sum"))
      nG <- simplify(net2, edge.attr.comb = list(weight="sum"))
      NewDat <- NULL
      NewDat <- as.data.frame(degree(nG))
      NewDat$ID <- row.names(NewDat)  
      NewDat$ocassion <- paste(j,  sep = " ")
      NewDat$Av <- paste(i, sep="")
      NewDat$Degree <- degree(nG)
      NewDat$Closeness <- closeness(nG)
      NewDat$Betweenness <- betweenness(nG)
      # the next two ones are variables on network - that means they generate the same value for each entry for that network. 
      NewDat$Rdensity<-c(rep(edge_density(nG),length(strength(nG))))
      NewDat$RNVertices<-c(rep(gorder(nG),length(strength(nG))))
      NewDat <- NewDat[,-1]
      #Saving each file under a different name. 
      assign(paste("Event",i, "Dataset", sep = ""), NewDat )
      #Compiling all the data.
      Result_Dataset <- rbind(Result_Dataset,NewDat)
    }
  }
  
  #Calculating repeatability
  
  RDegree<-rptPoisson(Degree~(1|ID), grname="ID", data=Result_Dataset, nboot=1000)
  rD<-data.frame(NULL)
  rD<-as.numeric(RDegree$R)
  
  RClose<-rptPoisson(Closeness~(1|ID), grname="ID", data=Result_Dataset, nboot=1000)
  rC<-data.frame(NULL)
  rC<-as.numeric(RClose$R)
  
  RBet<-rptPoisson(Betweenness~(1|ID), grname="ID", data=Result_Dataset, nboot=1000)
  rB<-data.frame(NULL)
  rB<-as.numeric(RBet$R)
 
  TempR <- NULL
  TempR <- as.data.frame(rD[[1]])
  TempR$RDeg<-rD
  TempR$RBet<-rB
  TempR$RClose<-rC
  
  TempR<-TempR[,-1]
  #bind data from each permutation to create a dataset of repeatability values
  RData <-rbind(RData, TempR)
}
#save data 
write.csv(RData, "NetworkSimulationData1.csv")
head(RData)
#plot null model distribution with observed values and confidence intevals 
plot(density(RData$RBet), xlim=c(0,0.8), main="", xlab="Repeatability Betweeness")
lines(x=c(0.287,0.287), y=c(0,27), col="red")
lines(x=c(0.000,0.000), y=c(0,27), col="red", lty=2)
lines(x=c(0.536,0.536), y=c(0,27), col="red", lty=2)

plot(density(RData$RClose), xlim=c(0,0.8), main="", xlab="Repeatability Closeness")
lines(x=c(0.232,0.232), y=c(0,135), col="red")
lines(x=c(0.00,0.00), y=c(0,135), col="red", lty=2)
lines(x=c(0.459,0.459), y=c(0,135), col="red", lty=2)

plot(density(RData$RDeg), xlim=c(0,0.8), main="", xlab="Repeatability Degree")
lines(x=c(0.422,0.422), y=c(0,200), col="red")
lines(x=c(0.0566,0.0566), y=c(0,200), col="red", lty=2)
lines(x=c(0.626,0.626), y=c(0,200), col="red", lty=2)

#find significance values by comparing observed repeatability to 95% threshold 
#find exact significance by sorting null distribution and comparing observed value to the sorted distribution
B<-quantile(RData$RBet, probs=0.95)
sort(RData$RBet)
C<-quantile(RData$RClose, probs=0.95)
sort(RData$RClose)
D<-quantile(RData$RDeg, probs=0.95)
sort(RData$RDeg)

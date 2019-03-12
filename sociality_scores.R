#!/usr/bin/env Rscript
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "26 Mar 2018"

require(igraph)
require(dplyr)
require(plyr)
require(tnet)

rm(list=ls())
graphics.off()




# format and save data

###############################################################################
# Read in Data
###############################################################################

tran_code <- read.csv('file:///C:/Users/georg/Documents/Sparrows/Data/tblAllCodes.csv', header=T)

f_name <- 'total'

sparrows <- read.csv('file:///C:/Users/georg/Documents/Sparrows/Data/Aviary.csv', header=T)
coh_sex <- read.csv('file:///C:/Users/georg/Documents/Sparrows/Data/coh_sex.csv', header=T)

data <- read.csv(paste('file:///C:/Users/georg/Documents/Sparrows/Results/',f_name,'.csv', sep=''), header=T)
# interaction data file 

id <- read.csv('file:///C:/Users/georg/Documents/Sparrows/Data/birdsex.1_validcolourcodes_AST.csv', header=T)
# colour ring to ID file


###############################################################################
# Format Data
###############################################################################


#################### Change Ring code to ID code

data$individual1 <- tran_code$BirdID[match(data$id1, tran_code$Transponder)]
# change bird id1 to ID code from ring data

data$individual2 <- tran_code$BirdID[match(data$id2, tran_code$Transponder)]
# change bird id2 to ID code from ring data


###############################################################################
# loop testing
###############################################################################

soc_trip <- function(ev){ 
  
  df <- subset(data, trip == trips[ev,])
  
  indv <- data.frame(df$individual1, df$individual2)
  # make individual data into df
  
  net <- graph.data.frame(indv, directed=F)
  # network plot format for data
  
  E(net)$weight <- 1
  net2 <- simplify(net, edge.attr.comb = list(weight="sum"))
  # collapses multiple edges to weighted edges
  
  plot(net2, vertex.size=2, vertex.label=NA, vertex.color="grey50", layout=layout_nicely(net2)) 
  dev.off()
  
  #plot(net2, vertex.size=5, vertex.label=NA, vertex.color="grey50") 
  
  den <- edge_density(net2, loops=FALSE)
  
  betw <- as.data.frame(betweenness(net2, directed=F, weights=NA))
  names(betw)[1] <- "betweenness"
  betw$individual <- rownames(betw)
  # creates and formats betweenness data
  
  
  close <- as.data.frame(closeness(net2, mode="all", weights=NA))
  names(close)[1] <- "closeness"
  close$individual <- rownames(close)
  # creates and formats closeness data
  
  
  deg <- as.data.frame(degree(net2, mode="all"))
  names(deg)[1] <- "degree"
  deg$individual <- rownames(deg)
  # creates and formats degree data
  
  
  soc <- merge(close, deg, by="individual", all=TRUE)
  soc <- merge(soc, betw, by="individual", all=TRUE)
  # merges the data created above
  
  
  soc$cohort <- coh_sex$Cohort[match(soc$individual, coh_sex$BirdID)]
  # adds cohort to the measured individuals
  
  soc$year<- 2018
  #adds year to dataframe 
  
  
  soc$age <- soc$year - soc$cohort
  
  soc$sex <- coh_sex$SexEstimate[match(soc$individual, coh_sex$BirdID)]
  # 0 = F
  
  
  soc$trip <- trips[ev,]
  # adds trip column 
  soc$type <- 2
  soc$density <- den
  return(soc)
}


###############################################################################
# loop to run functions for days
###############################################################################



trips <- data.matrix(unique(data$trip, incomparables = FALSE, MARGIN = 1, fromLast = FALSE))
# makes a df of list of unique recording days

dlen_d <- nrow(trips)
# number of days


soc_t1 <-SetNames(data.frame(matrix(ncol = 10, nrow = 9)), c("individual", "closeness", "degree", "betweenness", "weighted", "cohort", "year", "age", "sex", "trip"))
# sets up df to populate later for average modularity


for (i in 1:dlen_d){

  assign(paste("soc_t",i, sep=""), soc_trip(i))
}

soc_total <- rbind(soc_t1)
#creates datframe 

write.csv(soc_total, 'C:/Users/georg/Documents/Sparrows/Results/soc.csv')
#saves dataframe to results 

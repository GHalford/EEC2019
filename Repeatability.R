rm(list=ls())

#Set wd
setwd("Documents/Sparrows")


library(rptR)
library(ggplot2)
library(dplyr)
library(readr)

#import data
interactions <- read_csv("Sparrows/InteractionResults.csv")
str(interactions)
##Ring_ID = Ring number for each individual bird. 
##Sex = male (1) and female (0)
##Age was determined from year bird was born (Cohort)
##Aviary_Out is the aviary that individuals came from at the time of measuring during first social network analysis (SNA)
##Aviary_In is the aviary that individuals came from at the time of measurement during second SNA
##Colour rings are the colour ring combinations that help identify each bird. Birds with just 'M' are birds born in 2018 with just a metal number ring.
##occasion is the round of data collection in which the scores were taken from
##Nodes is the number of nodes within each network 
##betweenness, closeness and degree are all sociality scores.
##Socialty scores were measured as follows during the first SNA:
##In Aviary 1, from 17th December to 5th Jan 2019
##In Aviay 4, from 13th November to 19th November 2018
##In Aviary 7, from 13th November to 25th November 2018
##In Aviary 10, from 4th December to 14th January 2019
##In Aviary 13, from 25th November to 3rd December 2018
##In Aviary 16, from 29th November to 3rd December 2018
##In Aviary 19, from 6th December to 16th December 2018
##In Aviary 22, from 4th January to 14th January 2018
##Socialty scores were measured as follows during the second SNA:
##In Aviary 1, from 28th January to 4th December 2019
##In Aviay 4, from 11th February to 18th February 2019
##In Aviary 7, from 18th February to 25th February 2019
##In Aviary 10, from 4th February to 11th February 2019
##In Aviary 13, from 28th January to 4th February 2019
##In Aviary 16, from 4th Febryary to 11th February 2019
##In Aviary 19, from 13th February to 20th February 2019
##Overall, social interactions were measured from 13th November to 14th January (2 months) during the first SNA and from 28th January to 25th February (1 month) during the second SNA
##Transponder is the unique pit tag that each bird has, so when they entered the social network cage, their number would be scanned and recorded. This can change over time as birds can lose their transponders.


i<-interactions

##What about age and sociality?
boxplot(i$age~i$degree) 
boxplot(i$age~i$betweenness)
boxplot(i$age~i$closeness)
##Doesn't seem to be an affect of age and personality

##subset results based upon occasion of SNA
i1<-subset(i, occasion==1, select=c("Ring_ID", "Transponder", "colour rings", "Aviary_out", "Aviary_in", "degree", "closeness", "betweenness", "sex", "age"))
i2<-subset(i, occasion==2, select=c("Ring_ID", "Transponder", "colour rings", "Aviary_out", "Aviary_in", "degree", "closeness", "betweenness", "sex", "age"))

##What about aviary?
table(i1$Aviary_out)
##13 bird in aviary 1, 13 in 4, 9 in 7, 15 in 10, 11 in 13, 18 in 16, 12 in 19 and 11 in 22 used in analysis
##birds with no recordngs were not used in analysis
table(i2$Aviary_in)
##14 bird in aviary 1, 15 in 4, 15 in 7, 16 in 10, 14 in 13, 15 in 16, 13 in 19 used in analysis
##Aviary 1 and 19 contains extroverts, 4, 7, 13 and 16 contains introverts and 10 contains a mixed population (control)
##aviary22 contained all birds with no recordings during the first occasion, these were not used in the analysis 
hist(i1$Aviary_out)
hist(i2$Aviary_in)
boxplot(i1$Aviary_out~i1$Ring_ID)
boxplot(i2$Aviary_in~i2$Ring_ID)
##Here can see which bird is in which aviary.
##Consider Aviary as a fixed effect in model?


##Look at sociality for the first occasion of data collection 
mean(i1$degree)
#mean=1.147059
mean(i1$betweenness)
#mean=0.1388889
mean(i1$closeness)
#mean=0.0642553
##all quite low scores. What about range - do we see a big range of between individuals?
range(i1$degree)
#range 0-8
range(i1$betweenness)
#range 0-0.2783333
range(i1$closeness)
#range 0-0.5
##Range within all 3 seems quite low 
var(i1$degree)
#var=4.008
var(i1$betweenness)
#var=0.1828841
var(i1$closeness)
#var=0.01207875
##Not much variation between individuals with closeness score.  
##Note these personality scores are ranked continious data. Zero inflated. Poisson distribution for analysing
hist(i1$degree)
hist(i1$betweenness)
hist(i1$closeness)
##All left skewed, zero inflated. Poisson. Non normal.

##Look at sociality for the second occasion of data collection 
##data not fully collected for this occasion of collection yet so cannot fully comment on it 
mean(i2$degree)
#mean=2.715686
mean(i2$betweenness)
#mean=0.3294118
mean(i2$closeness)
#mean=0.1541543
##means so far for degree and betweeness are much higher, slightly lower for closeness- could be skewed by high values in extrovert aviary  
range(i2$degree)
#range 0-12
range(i2$betweenness)
#range=0-7.5
range(i2$closeness)
#range 0-0.83333
##range is higher for all 3 then forst occasion
var(i2$degree)
#var=16.3
var(i2$betweenness)
#var=1.2918
var(i2$closeness)
#0.07071487
## variance between individuals again higher across the board
##higher range and variance could be due to higher scores from extroverts interacting with more extroverts? 
##
hist(i2$degree)
hist(i2$betweenness)
hist(i2$closeness)
##once again all left skewed. Poisson distributed. However there are a higher number of non-zero scores 

#separate look at introverted and extroverted aviary's 
Ex<-subset(i2, subset=(Aviary_in==1|Aviary_in==19), select=c("Ring_ID", "Transponder", "colour rings", "Aviary_out", "Aviary_in", "degree", "closeness", "betweenness", "sex", "age"))
In<-subset(i2, subset=(Aviary_in==4|Aviary_in==7|Aviary_in==13|Aviary_in==16), select=c("Ring_ID", "Transponder", "colour rings", "Aviary_out", "Aviary_in", "degree", "closeness", "betweenness", "sex", "age"))
mean(Ex$degree)
#mean=2.481481
mean(Ex$betweenness)
#mean=0.6518519
mean(Ex$closeness)
#mean=0.05861132
mean(In$degree)
#mean=1.118644
mean(In$betweenness)
#mean=0.2711864
mean(In$closeness)
#mean=0.07019101
t.test(Ex$degree, In$degree, data=i)
t.test(Ex$betweenness, In$betweenness, data=i)
t.test(Ex$closeness, In$closeness, data=i)

##Difference in means across sampling periods 
t.test(i1$degree,i2$degree, data=interactions)
#significant difference in means between data sets in degree, p=0.00058- means that changing aviary compositions has had an effect on sociality? 
t.test(i1$betweenness,i2$betweenness, data=interactions)
#is a difference between data sets, however not significant, p=0.1155- different to degree due to how its measured? 
t.test(i1$closeness,i2$closeness, data=interactions)
#significant difference between data sets again, p=0.0019
#results suggest that personalities may not be repeatable as they are affected by the changes? 

##Does personality differ between individuals
model1d<-lm(i1$degree~i1$Ring_ID)
summary(model1d)
#Significant difference between individuals and sociality score of degree
model1b<-lm(i1$betweenness~i1$Ring_ID)
summary(model1b)
model1c<-lm(i1$closeness~i1$Ring_ID)
summary(model1c)
#Signifcant difference between sociality of individals

##sociality per aviary for first occasion of data collection 
boxplot(i1$degree~i1$Aviary_out)
##seems sociality does change with aviary number.
boxplot(i1$betweenness~i1$Aviary_out)
##Again,potentially differences between aviaries here (more zeros though)
boxplot(i1$closeness~i1$Aviary_out)
##Potentially differences again
model2d<-lm(i1$degree~i1$Aviary_out)
summary(model2d)
anova(model2d)
##no significant difference between aviarys, p=0.2764
model2b<-lm(i1$betweenness~i1$Aviary_out)
summary(model2b)
anova(model2b)
##no significant difference between aviarys, p=0.3077 
model2c<-lm(i1$closeness~i1$Aviary_out)
summary(model2c)
anova(model2c)
##again no significant difference between aviarys, p=0.423
##shows that no significant differences between aviarys in sociality scores during the first occasion of data collection as expected 

##sociality per aviary during second occasion of data collection
boxplot(i2$degree~i2$Aviary_in)
##seems sociality does change, with aviary 1 and 10 looking much more social- mixed and extrovert population, unexpectedly 19 very low
boxplot(i2$betweenness~i2$Aviary_in)
##seems not much diffenece across aviarys, with the exeption of Aviary1 
boxplot(i2$closeness~i2$Aviary_in)
##difficult to interpret any differences 
model3d<-lm(i2$degree~i2$Aviary_in)
summary(model3d)
anova(model3d)
##no significant difference between degree, p=0.1617 
model3b<-lm(i2$betweenness~i1$Aviary_in)
summary(model3b)
anova(model3b)
##no significant difference, p=0.1753
model3c<-lm(i2$closeness~i2$Aviary_in)
summary(model3c)
anova(model3c)
##no significant difference, p=0.8285
###suggest that the make-up of the social population does not effect sociality scores, however p value for bet and deg lower than first round 

##Repeatability across the whole population 

#Find how many repeated measures
table(table(interactions$Ring_ID))
#2 repeated measures, 102 individuals 

#repeatability across social contexts for degree
rep1<-rptPoisson(degree~Aviary_in+Aviary_out+Nodes+sex+(1|Ring_ID), grname="Ring_ID", data=interactions, nboot=1000, npermut=1000)
##added aviarys and sex as fixed effects, unsure if this was the correct thing to do, but also ran the model without these and it made very little difference 
print(rep1)
summary(rep1)
plot(rep1)


#repeatability for betweenness
rep2<-rptPoisson(betweenness~Aviary_in+Aviary_out+Nodes+(1|Ring_ID), grname="Ring_ID", data=interactions, nboot=1000, npermut=1000)
print(rep2)
summary(rep2)
plot(rep2)


#repeatability for closeness 
rep3<-rptPoisson(closeness~Aviary_in+Aviary_out+Nodes+sex+(1|Ring_ID),grname="Ring_ID", data=interactions, nboot=1000, npermut=1000)
print(rep3)
summary(rep3)
plot(rep3)




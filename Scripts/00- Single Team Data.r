library(tidyverse)
library(rethinking) #Web page, YouTube, Book
library(car) #regression diagnostics

#Data
#2016 overview: Inherent unpredictability: CHC lost 1/3 of games
#Objective is to predict who wins a game. Build model at level of a particular team.
#CHC schedule overview.
urlfile<-'https://raw.githubusercontent.com/blakeobeans/Bayesian-Baseball/master/Cubs/Season/cubs.csv'
data<-read.csv(urlfile)
#Pitcher Stats
urlfile<-'https://raw.githubusercontent.com/blakeobeans/Bayesian-Baseball/master/Cubs/Pitching/pitching.csv'
pitcher<-read.csv(urlfile)
data<- left_join(data, pitcher,by="Pit")
#WL Record
urlfile<-'https://raw.githubusercontent.com/blakeobeans/Bayesian-Baseball/master/Cubs/Team%20Rankings/rankings.csv'
WL<-read.csv(urlfile)
data<- left_join(data, WL,by="Opp")
#Transform Data
data$score <- data$R-data$RA #Positive score indicates Giants win. Beats logistic. No ties.
data$opp_team <- coerce_index(data$Opp) #ID for team (function from rethinking)
data$pit_id <- coerce_index(data$Pit) #Home pitcher
names(data) <- c("tm", "opp", "R", "RA", "pit", "pitera", "wl", "score",  "opp_team", "pit_id")
data$pitera_norm <-  (data$pitera - mean(data$pitera))/sd(data$pitera) #normalize ERA
data$wl_norm <-  (data$wl - mean(data$wl))/sd(data$wl) #normalize WL
data <- as.data.frame(data)
#Summary Stats
length(unique(data$opp_team)) # number of opposing teams; note 29 other in MLB
length(unique(data$pit)) # number of cubs pitchers
table(data$pit)# interesting margins, assume 1 per game
hist(data$score, prob=TRUE) #dist of scores
lines(density(data$score))
shapiro.test(data$score) #passes normality
#sort data
data <- data[order(data$opp_team),] #sort by team, note imbalance


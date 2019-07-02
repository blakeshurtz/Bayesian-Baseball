library(tidyverse)
library(rethinking)

###import batting data manually: bal_bat###
###import pitching data manually: bal_pit###

team <- 'BAL' #team name variable

###bal_homeaway for home/away###
bal_homeaway <- bal_bat %>% select(Date, Opp, X)
###create home variable
bal_homeaway$X <- as.character(bal_homeaway$X)
bal_homeaway$Opp <- as.character(bal_homeaway$Opp)
bal_homeaway$X <- ifelse(bal_homeaway$X =='@', bal_homeaway$Opp, bal_homeaway$X)
bal_homeaway$X <- ifelse(bal_homeaway$X =='', team, bal_homeaway$X) #use of team variable name here
names(bal_homeaway)[names(bal_homeaway) == 'X'] <- 'home'
###create away variable
bal_homeaway$Opp <- ifelse(bal_homeaway$Opp == bal_homeaway$home, team, bal_homeaway$Opp) #use of team variable name here
names(bal_homeaway)[names(bal_homeaway) == 'Opp'] <- 'away'
###change class of Date
bal_homeaway$Date <- as.character(bal_homeaway$Date)
###select key vars
bal_homeaway <- bal_homeaway %>% select(home, away, Date) 

###bal_bat_a for batting vars (away)###
bal_bat_a <- bal_bat %>% select(Date, Opp, Rslt, X, BA)
bal_bat_a <- bal_bat_a %>% filter(X == '@') #away games
###runs for away games###
bal_bat_a$Rslt <- as.character(bal_bat_a$Rslt) #character vector
TEMP<- strsplit(x=bal_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
bal_bat_a$Raway <- substring(TEMP$V1, 2); bal_bat_a$Raway <- as.numeric(bal_bat_a$Raway)
###BA for away games###
bal_bat_a$BAa <- bal_bat_a$BA
###create away variable###
bal_bat_a$X <- ifelse(bal_bat_a$X =='@', team, bal_bat_a$X) #use of team variable name here
names(bal_bat_a)[names(bal_bat_a) == 'X'] <- 'away'
###change class of Date
bal_bat_a$Date <- as.character(bal_bat_a$Date)
###select key vars
bal_bat_a <- bal_bat_a %>% select(Date, away, Raway, BAa)

###bal_bat_h for batting vars (home)###
bal_bat_h <- bal_bat %>% select(Date, Opp, Rslt, X, BA)
bal_bat_h <- bal_bat_h %>% filter(X == '') #home games
###runs for home games###
bal_bat_h$Rslt <- as.character(bal_bat_h$Rslt) #character vector
TEMP<- strsplit(x=bal_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
bal_bat_h$Rhome <- substring(TEMP$V1, 2); bal_bat_h$Rhome <- as.numeric(bal_bat_h$Rhome)
###BA for home games###
bal_bat_h$BAh <- bal_bat_h$BA
###create home variable###
bal_bat_h$X <- ifelse(bal_bat_h$X =='', team, bal_bat_h$X) #use of team variable name here
names(bal_bat_h)[names(bal_bat_h) == 'X'] <- 'home'
###change class of Date
bal_bat_h$Date <- as.character(bal_bat_h$Date)
###select key vars
bal_bat_h <- bal_bat_h %>% select(Date, home, Rhome, BAh)

###bal_pit_a for pitching vbalables (away)###
bal_pit_a <- bal_pit %>% select(Date, Opp, X, H, R)
bal_pit_a <- bal_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(bal_pit_a)[names(bal_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(bal_pit_a)[names(bal_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
bal_pit_a$X <- ifelse(bal_pit_a$X =='@', team, bal_pit_a$X) #use of team variable name here
names(bal_pit_a)[names(bal_pit_a) == 'X'] <- 'away'
###change class of Date
bal_pit_a$Date <- as.character(bal_pit_a$Date)
###select key vars
bal_pit_a <- bal_pit_a %>% select(Date, away, HHAa, RSAa)

###bal_pit_h for pitching variables (home)###
bal_pit_h <- bal_pit %>% select(Date, Opp, X, H, R)
bal_pit_h <- bal_pit_h %>% filter(X == '') #home games
###splitting bal_pit_h vector###
names(bal_pit_h)[names(bal_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(bal_pit_h)[names(bal_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
bal_pit_h$X <- ifelse(bal_pit_h$X =='', team, bal_pit_h$X) #use of team variable name here
names(bal_pit_h)[names(bal_pit_h) == 'X'] <- 'home'
###change class of Date
bal_pit_h$Date <- as.character(bal_pit_h$Date)
###filter down data
bal_pit_h <- bal_pit_h %>% select(Date, home, HHAh, RSAh)

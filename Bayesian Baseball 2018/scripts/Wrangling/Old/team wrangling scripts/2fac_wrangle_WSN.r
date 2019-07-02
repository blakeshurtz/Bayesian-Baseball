library(tidyverse)
library(rethinking)

###import batting data manually: wsn_bat###
###import pitching data manually: wsn_pit###

team <- 'WSN' #team name variable

###wsn_homeaway for home/away###
wsn_homeaway <- wsn_bat %>% select(Date, Opp, X)
###create home variable
wsn_homeaway$X <- as.character(wsn_homeaway$X)
wsn_homeaway$Opp <- as.character(wsn_homeaway$Opp)
wsn_homeaway$X <- ifelse(wsn_homeaway$X =='@', wsn_homeaway$Opp, wsn_homeaway$X)
wsn_homeaway$X <- ifelse(wsn_homeaway$X =='', team, wsn_homeaway$X) #use of team variable name here
names(wsn_homeaway)[names(wsn_homeaway) == 'X'] <- 'home'
###create away variable
wsn_homeaway$Opp <- ifelse(wsn_homeaway$Opp == wsn_homeaway$home, team, wsn_homeaway$Opp) #use of team variable name here
names(wsn_homeaway)[names(wsn_homeaway) == 'Opp'] <- 'away'
###change class of Date
wsn_homeaway$Date <- as.character(wsn_homeaway$Date)
###select key vars
wsn_homeaway <- wsn_homeaway %>% select(home, away, Date) 

###wsn_bat_a for batting vars (away)###
wsn_bat_a <- wsn_bat %>% select(Date, Opp, Rslt, X, BA)
wsn_bat_a <- wsn_bat_a %>% filter(X == '@') #away games
###runs for away games###
wsn_bat_a$Rslt <- as.character(wsn_bat_a$Rslt) #character vecwsn
TEMP<- strsplit(x=wsn_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
wsn_bat_a$Raway <- substring(TEMP$V1, 2); wsn_bat_a$Raway <- as.numeric(wsn_bat_a$Raway)
###BA for away games###
wsn_bat_a$BAa <- wsn_bat_a$BA
###create away variable###
wsn_bat_a$X <- ifelse(wsn_bat_a$X =='@', team, wsn_bat_a$X) #use of team variable name here
names(wsn_bat_a)[names(wsn_bat_a) == 'X'] <- 'away'
###change class of Date
wsn_bat_a$Date <- as.character(wsn_bat_a$Date)
###select key vars
wsn_bat_a <- wsn_bat_a %>% select(Date, away, Raway, BAa)

###wsn_bat_h for batting vars (home)###
wsn_bat_h <- wsn_bat %>% select(Date, Opp, Rslt, X, BA)
wsn_bat_h <- wsn_bat_h %>% filter(X == '') #home games
###runs for home games###
wsn_bat_h$Rslt <- as.character(wsn_bat_h$Rslt) #character vecwsn
TEMP<- strsplit(x=wsn_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
wsn_bat_h$Rhome <- substring(TEMP$V1, 2); wsn_bat_h$Rhome <- as.numeric(wsn_bat_h$Rhome)
###BA for home games###
wsn_bat_h$BAh <- wsn_bat_h$BA
###create home variable###
wsn_bat_h$X <- ifelse(wsn_bat_h$X =='', team, wsn_bat_h$X) #use of team variable name here
names(wsn_bat_h)[names(wsn_bat_h) == 'X'] <- 'home'
###change class of Date
wsn_bat_h$Date <- as.character(wsn_bat_h$Date)
###select key vars
wsn_bat_h <- wsn_bat_h %>% select(Date, home, Rhome, BAh)

###wsn_pit_a for pitching vwsnables (away)###
wsn_pit_a <- wsn_pit %>% select(Date, Opp, X, H, R)
wsn_pit_a <- wsn_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(wsn_pit_a)[names(wsn_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(wsn_pit_a)[names(wsn_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
wsn_pit_a$X <- ifelse(wsn_pit_a$X =='@', team, wsn_pit_a$X) #use of team variable name here
names(wsn_pit_a)[names(wsn_pit_a) == 'X'] <- 'away'
###change class of Date
wsn_pit_a$Date <- as.character(wsn_pit_a$Date)
###select key vars
wsn_pit_a <- wsn_pit_a %>% select(Date, away, HHAa, RSAa)

###wsn_pit_h for pitching variables (home)###
wsn_pit_h <- wsn_pit %>% select(Date, Opp, X, H, R)
wsn_pit_h <- wsn_pit_h %>% filter(X == '') #home games
###splitting wsn_pit_h vecwsn###
names(wsn_pit_h)[names(wsn_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(wsn_pit_h)[names(wsn_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
wsn_pit_h$X <- ifelse(wsn_pit_h$X =='', team, wsn_pit_h$X) #use of team variable name here
names(wsn_pit_h)[names(wsn_pit_h) == 'X'] <- 'home'
###change class of Date
wsn_pit_h$Date <- as.character(wsn_pit_h$Date)
###filter down data
wsn_pit_h <- wsn_pit_h %>% select(Date, home, HHAh, RSAh)

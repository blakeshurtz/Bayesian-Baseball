library(tidyverse)
library(rethinking)

###import batting data manually: chc_bat###
###import pitching data manually: chc_pit###

team <- 'CHC' #team name variable

###chc_homeaway for home/away###
chc_homeaway <- chc_bat %>% select(Date, Opp, X)
###create home variable
chc_homeaway$X <- as.character(chc_homeaway$X)
chc_homeaway$Opp <- as.character(chc_homeaway$Opp)
chc_homeaway$X <- ifelse(chc_homeaway$X =='@', chc_homeaway$Opp, chc_homeaway$X)
chc_homeaway$X <- ifelse(chc_homeaway$X =='', team, chc_homeaway$X) #use of team variable name here
names(chc_homeaway)[names(chc_homeaway) == 'X'] <- 'home'
###create away variable
chc_homeaway$Opp <- ifelse(chc_homeaway$Opp == chc_homeaway$home, team, chc_homeaway$Opp) #use of team variable name here
names(chc_homeaway)[names(chc_homeaway) == 'Opp'] <- 'away'
###change class of Date
chc_homeaway$Date <- as.character(chc_homeaway$Date)
###select key vars
chc_homeaway <- chc_homeaway %>% select(home, away, Date) 

###chc_bat_a for batting vars (away)###
chc_bat_a <- chc_bat %>% select(Date, Opp, Rslt, X, BA)
chc_bat_a <- chc_bat_a %>% filter(X == '@') #away games
###runs for away games###
chc_bat_a$Rslt <- as.character(chc_bat_a$Rslt) #character vector
TEMP<- strsplit(x=chc_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
chc_bat_a$Raway <- substring(TEMP$V1, 2); chc_bat_a$Raway <- as.numeric(chc_bat_a$Raway)
###BA for away games###
chc_bat_a$BAa <- chc_bat_a$BA
###create away variable###
chc_bat_a$X <- ifelse(chc_bat_a$X =='@', team, chc_bat_a$X) #use of team variable name here
names(chc_bat_a)[names(chc_bat_a) == 'X'] <- 'away'
###change class of Date
chc_bat_a$Date <- as.character(chc_bat_a$Date)
###select key vars
chc_bat_a <- chc_bat_a %>% select(Date, away, Raway, BAa)

###chc_bat_h for batting vars (home)###
chc_bat_h <- chc_bat %>% select(Date, Opp, Rslt, X, BA)
chc_bat_h <- chc_bat_h %>% filter(X == '') #home games
###runs for home games###
chc_bat_h$Rslt <- as.character(chc_bat_h$Rslt) #character vector
TEMP<- strsplit(x=chc_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
chc_bat_h$Rhome <- substring(TEMP$V1, 2); chc_bat_h$Rhome <- as.numeric(chc_bat_h$Rhome)
###BA for home games###
chc_bat_h$BAh <- chc_bat_h$BA
###create home variable###
chc_bat_h$X <- ifelse(chc_bat_h$X =='', team, chc_bat_h$X) #use of team variable name here
names(chc_bat_h)[names(chc_bat_h) == 'X'] <- 'home'
###change class of Date
chc_bat_h$Date <- as.character(chc_bat_h$Date)
###select key vars
chc_bat_h <- chc_bat_h %>% select(Date, home, Rhome, BAh)

###chc_pit_a for pitching vchcables (away)###
chc_pit_a <- chc_pit %>% select(Date, Opp, X, H, R)
chc_pit_a <- chc_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(chc_pit_a)[names(chc_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(chc_pit_a)[names(chc_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
chc_pit_a$X <- ifelse(chc_pit_a$X =='@', team, chc_pit_a$X) #use of team variable name here
names(chc_pit_a)[names(chc_pit_a) == 'X'] <- 'away'
###change class of Date
chc_pit_a$Date <- as.character(chc_pit_a$Date)
###select key vars
chc_pit_a <- chc_pit_a %>% select(Date, away, HHAa, RSAa)

###chc_pit_h for pitching variables (home)###
chc_pit_h <- chc_pit %>% select(Date, Opp, X, H, R)
chc_pit_h <- chc_pit_h %>% filter(X == '') #home games
###splitting chc_pit_h vector###
names(chc_pit_h)[names(chc_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(chc_pit_h)[names(chc_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
chc_pit_h$X <- ifelse(chc_pit_h$X =='', team, chc_pit_h$X) #use of team variable name here
names(chc_pit_h)[names(chc_pit_h) == 'X'] <- 'home'
###change class of Date
chc_pit_h$Date <- as.character(chc_pit_h$Date)
###filter down data
chc_pit_h <- chc_pit_h %>% select(Date, home, HHAh, RSAh)

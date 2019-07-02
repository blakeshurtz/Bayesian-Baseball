library(tidyverse)
library(rethinking)

###import batting data manually: chw_bat###
###import pitching data manually: chw_pit###

team <- 'CHW' #team name variable

###chw_homeaway for home/away###
chw_homeaway <- chw_bat %>% select(Date, Opp, X)
###create home variable
chw_homeaway$X <- as.character(chw_homeaway$X)
chw_homeaway$Opp <- as.character(chw_homeaway$Opp)
chw_homeaway$X <- ifelse(chw_homeaway$X =='@', chw_homeaway$Opp, chw_homeaway$X)
chw_homeaway$X <- ifelse(chw_homeaway$X =='', team, chw_homeaway$X) #use of team variable name here
names(chw_homeaway)[names(chw_homeaway) == 'X'] <- 'home'
###create away variable
chw_homeaway$Opp <- ifelse(chw_homeaway$Opp == chw_homeaway$home, team, chw_homeaway$Opp) #use of team variable name here
names(chw_homeaway)[names(chw_homeaway) == 'Opp'] <- 'away'
###change class of Date
chw_homeaway$Date <- as.character(chw_homeaway$Date)
###select key vars
chw_homeaway <- chw_homeaway %>% select(home, away, Date) 

###chw_bat_a for batting vars (away)###
chw_bat_a <- chw_bat %>% select(Date, Opp, Rslt, X, BA)
chw_bat_a <- chw_bat_a %>% filter(X == '@') #away games
###runs for away games###
chw_bat_a$Rslt <- as.character(chw_bat_a$Rslt) #character vector
TEMP<- strsplit(x=chw_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
chw_bat_a$Raway <- substring(TEMP$V1, 2); chw_bat_a$Raway <- as.numeric(chw_bat_a$Raway)
###BA for away games###
chw_bat_a$BAa <- chw_bat_a$BA
###create away variable###
chw_bat_a$X <- ifelse(chw_bat_a$X =='@', team, chw_bat_a$X) #use of team variable name here
names(chw_bat_a)[names(chw_bat_a) == 'X'] <- 'away'
###change class of Date
chw_bat_a$Date <- as.character(chw_bat_a$Date)
###select key vars
chw_bat_a <- chw_bat_a %>% select(Date, away, Raway, BAa)

###chw_bat_h for batting vars (home)###
chw_bat_h <- chw_bat %>% select(Date, Opp, Rslt, X, BA)
chw_bat_h <- chw_bat_h %>% filter(X == '') #home games
###runs for home games###
chw_bat_h$Rslt <- as.character(chw_bat_h$Rslt) #character vector
TEMP<- strsplit(x=chw_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
chw_bat_h$Rhome <- substring(TEMP$V1, 2); chw_bat_h$Rhome <- as.numeric(chw_bat_h$Rhome)
###BA for home games###
chw_bat_h$BAh <- chw_bat_h$BA
###create home variable###
chw_bat_h$X <- ifelse(chw_bat_h$X =='', team, chw_bat_h$X) #use of team variable name here
names(chw_bat_h)[names(chw_bat_h) == 'X'] <- 'home'
###change class of Date
chw_bat_h$Date <- as.character(chw_bat_h$Date)
###select key vars
chw_bat_h <- chw_bat_h %>% select(Date, home, Rhome, BAh)

###chw_pit_a for pitching vchwables (away)###
chw_pit_a <- chw_pit %>% select(Date, Opp, X, H, R)
chw_pit_a <- chw_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(chw_pit_a)[names(chw_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(chw_pit_a)[names(chw_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
chw_pit_a$X <- ifelse(chw_pit_a$X =='@', team, chw_pit_a$X) #use of team variable name here
names(chw_pit_a)[names(chw_pit_a) == 'X'] <- 'away'
###change class of Date
chw_pit_a$Date <- as.character(chw_pit_a$Date)
###select key vars
chw_pit_a <- chw_pit_a %>% select(Date, away, HHAa, RSAa)

###chw_pit_h for pitching variables (home)###
chw_pit_h <- chw_pit %>% select(Date, Opp, X, H, R)
chw_pit_h <- chw_pit_h %>% filter(X == '') #home games
###splitting chw_pit_h vector###
names(chw_pit_h)[names(chw_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(chw_pit_h)[names(chw_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
chw_pit_h$X <- ifelse(chw_pit_h$X =='', team, chw_pit_h$X) #use of team variable name here
names(chw_pit_h)[names(chw_pit_h) == 'X'] <- 'home'
###change class of Date
chw_pit_h$Date <- as.character(chw_pit_h$Date)
###filter down data
chw_pit_h <- chw_pit_h %>% select(Date, home, HHAh, RSAh)

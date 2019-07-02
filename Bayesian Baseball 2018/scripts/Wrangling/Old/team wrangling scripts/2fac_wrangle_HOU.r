library(tidyverse)
library(rethinking)

###import batting data manually: hou_bat###
###import pitching data manually: hou_pit###

team <- 'HOU' #team name variable

###hou_homeaway for home/away###
hou_homeaway <- hou_bat %>% select(Date, Opp, X)
###create home variable
hou_homeaway$X <- as.character(hou_homeaway$X)
hou_homeaway$Opp <- as.character(hou_homeaway$Opp)
hou_homeaway$X <- ifelse(hou_homeaway$X =='@', hou_homeaway$Opp, hou_homeaway$X)
hou_homeaway$X <- ifelse(hou_homeaway$X =='', team, hou_homeaway$X) #use of team variable name here
names(hou_homeaway)[names(hou_homeaway) == 'X'] <- 'home'
###create away variable
hou_homeaway$Opp <- ifelse(hou_homeaway$Opp == hou_homeaway$home, team, hou_homeaway$Opp) #use of team variable name here
names(hou_homeaway)[names(hou_homeaway) == 'Opp'] <- 'away'
###change class of Date
hou_homeaway$Date <- as.character(hou_homeaway$Date)
###select key vars
hou_homeaway <- hou_homeaway %>% select(home, away, Date) 

###hou_bat_a for batting vars (away)###
hou_bat_a <- hou_bat %>% select(Date, Opp, Rslt, X, BA)
hou_bat_a <- hou_bat_a %>% filter(X == '@') #away games
###runs for away games###
hou_bat_a$Rslt <- as.character(hou_bat_a$Rslt) #character vector
TEMP<- strsplit(x=hou_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
hou_bat_a$Raway <- substring(TEMP$V1, 2); hou_bat_a$Raway <- as.numeric(hou_bat_a$Raway)
###BA for away games###
hou_bat_a$BAa <- hou_bat_a$BA
###create away variable###
hou_bat_a$X <- ifelse(hou_bat_a$X =='@', team, hou_bat_a$X) #use of team variable name here
names(hou_bat_a)[names(hou_bat_a) == 'X'] <- 'away'
###change class of Date
hou_bat_a$Date <- as.character(hou_bat_a$Date)
###select key vars
hou_bat_a <- hou_bat_a %>% select(Date, away, Raway, BAa)

###hou_bat_h for batting vars (home)###
hou_bat_h <- hou_bat %>% select(Date, Opp, Rslt, X, BA)
hou_bat_h <- hou_bat_h %>% filter(X == '') #home games
###runs for home games###
hou_bat_h$Rslt <- as.character(hou_bat_h$Rslt) #character vector
TEMP<- strsplit(x=hou_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
hou_bat_h$Rhome <- substring(TEMP$V1, 2); hou_bat_h$Rhome <- as.numeric(hou_bat_h$Rhome)
###BA for home games###
hou_bat_h$BAh <- hou_bat_h$BA
###create home variable###
hou_bat_h$X <- ifelse(hou_bat_h$X =='', team, hou_bat_h$X) #use of team variable name here
names(hou_bat_h)[names(hou_bat_h) == 'X'] <- 'home'
###change class of Date
hou_bat_h$Date <- as.character(hou_bat_h$Date)
###select key vars
hou_bat_h <- hou_bat_h %>% select(Date, home, Rhome, BAh)

###hou_pit_a for pitching vhouables (away)###
hou_pit_a <- hou_pit %>% select(Date, Opp, X, H, R)
hou_pit_a <- hou_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(hou_pit_a)[names(hou_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(hou_pit_a)[names(hou_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
hou_pit_a$X <- ifelse(hou_pit_a$X =='@', team, hou_pit_a$X) #use of team variable name here
names(hou_pit_a)[names(hou_pit_a) == 'X'] <- 'away'
###change class of Date
hou_pit_a$Date <- as.character(hou_pit_a$Date)
###select key vars
hou_pit_a <- hou_pit_a %>% select(Date, away, HHAa, RSAa)

###hou_pit_h for pitching variables (home)###
hou_pit_h <- hou_pit %>% select(Date, Opp, X, H, R)
hou_pit_h <- hou_pit_h %>% filter(X == '') #home games
###splitting hou_pit_h vector###
names(hou_pit_h)[names(hou_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(hou_pit_h)[names(hou_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
hou_pit_h$X <- ifelse(hou_pit_h$X =='', team, hou_pit_h$X) #use of team variable name here
names(hou_pit_h)[names(hou_pit_h) == 'X'] <- 'home'
###change class of Date
hou_pit_h$Date <- as.character(hou_pit_h$Date)
###filter down data
hou_pit_h <- hou_pit_h %>% select(Date, home, HHAh, RSAh)

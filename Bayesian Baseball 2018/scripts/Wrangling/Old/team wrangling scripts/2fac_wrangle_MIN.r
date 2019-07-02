library(tidyverse)
library(rethinking)

###import batting data manually: min_bat###
###import pitching data manually: min_pit###

team <- 'MIN' #team name variable

###min_homeaway for home/away###
min_homeaway <- min_bat %>% select(Date, Opp, X)
###create home variable
min_homeaway$X <- as.character(min_homeaway$X)
min_homeaway$Opp <- as.character(min_homeaway$Opp)
min_homeaway$X <- ifelse(min_homeaway$X =='@', min_homeaway$Opp, min_homeaway$X)
min_homeaway$X <- ifelse(min_homeaway$X =='', team, min_homeaway$X) #use of team variable name here
names(min_homeaway)[names(min_homeaway) == 'X'] <- 'home'
###create away variable
min_homeaway$Opp <- ifelse(min_homeaway$Opp == min_homeaway$home, team, min_homeaway$Opp) #use of team variable name here
names(min_homeaway)[names(min_homeaway) == 'Opp'] <- 'away'
###change class of Date
min_homeaway$Date <- as.character(min_homeaway$Date)
###select key vars
min_homeaway <- min_homeaway %>% select(home, away, Date) 

###min_bat_a for batting vars (away)###
min_bat_a <- min_bat %>% select(Date, Opp, Rslt, X, BA)
min_bat_a <- min_bat_a %>% filter(X == '@') #away games
###runs for away games###
min_bat_a$Rslt <- as.character(min_bat_a$Rslt) #character vector
TEMP<- strsplit(x=min_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
min_bat_a$Raway <- substring(TEMP$V1, 2); min_bat_a$Raway <- as.numeric(min_bat_a$Raway)
###BA for away games###
min_bat_a$BAa <- min_bat_a$BA
###create away variable###
min_bat_a$X <- ifelse(min_bat_a$X =='@', team, min_bat_a$X) #use of team variable name here
names(min_bat_a)[names(min_bat_a) == 'X'] <- 'away'
###change class of Date
min_bat_a$Date <- as.character(min_bat_a$Date)
###select key vars
min_bat_a <- min_bat_a %>% select(Date, away, Raway, BAa)

###min_bat_h for batting vars (home)###
min_bat_h <- min_bat %>% select(Date, Opp, Rslt, X, BA)
min_bat_h <- min_bat_h %>% filter(X == '') #home games
###runs for home games###
min_bat_h$Rslt <- as.character(min_bat_h$Rslt) #character vector
TEMP<- strsplit(x=min_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
min_bat_h$Rhome <- substring(TEMP$V1, 2); min_bat_h$Rhome <- as.numeric(min_bat_h$Rhome)
###BA for home games###
min_bat_h$BAh <- min_bat_h$BA
###create home variable###
min_bat_h$X <- ifelse(min_bat_h$X =='', team, min_bat_h$X) #use of team variable name here
names(min_bat_h)[names(min_bat_h) == 'X'] <- 'home'
###change class of Date
min_bat_h$Date <- as.character(min_bat_h$Date)
###select key vars
min_bat_h <- min_bat_h %>% select(Date, home, Rhome, BAh)

###min_pit_a for pitching vminables (away)###
min_pit_a <- min_pit %>% select(Date, Opp, X, H, R)
min_pit_a <- min_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(min_pit_a)[names(min_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(min_pit_a)[names(min_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
min_pit_a$X <- ifelse(min_pit_a$X =='@', team, min_pit_a$X) #use of team variable name here
names(min_pit_a)[names(min_pit_a) == 'X'] <- 'away'
###change class of Date
min_pit_a$Date <- as.character(min_pit_a$Date)
###select key vars
min_pit_a <- min_pit_a %>% select(Date, away, HHAa, RSAa)

###min_pit_h for pitching variables (home)###
min_pit_h <- min_pit %>% select(Date, Opp, X, H, R)
min_pit_h <- min_pit_h %>% filter(X == '') #home games
###splitting min_pit_h vector###
names(min_pit_h)[names(min_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(min_pit_h)[names(min_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
min_pit_h$X <- ifelse(min_pit_h$X =='', team, min_pit_h$X) #use of team variable name here
names(min_pit_h)[names(min_pit_h) == 'X'] <- 'home'
###change class of Date
min_pit_h$Date <- as.character(min_pit_h$Date)
###filter down data
min_pit_h <- min_pit_h %>% select(Date, home, HHAh, RSAh)

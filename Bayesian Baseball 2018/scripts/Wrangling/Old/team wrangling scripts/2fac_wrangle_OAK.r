library(tidyverse)
library(rethinking)

###import batting data manually: oak_bat###
###import pitching data manually: oak_pit###

team <- 'OAK' #team name variable

###oak_homeaway for home/away###
oak_homeaway <- oak_bat %>% select(Date, Opp, X)
###create home variable
oak_homeaway$X <- as.character(oak_homeaway$X)
oak_homeaway$Opp <- as.character(oak_homeaway$Opp)
oak_homeaway$X <- ifelse(oak_homeaway$X =='@', oak_homeaway$Opp, oak_homeaway$X)
oak_homeaway$X <- ifelse(oak_homeaway$X =='', team, oak_homeaway$X) #use of team variable name here
names(oak_homeaway)[names(oak_homeaway) == 'X'] <- 'home'
###create away variable
oak_homeaway$Opp <- ifelse(oak_homeaway$Opp == oak_homeaway$home, team, oak_homeaway$Opp) #use of team variable name here
names(oak_homeaway)[names(oak_homeaway) == 'Opp'] <- 'away'
###change class of Date
oak_homeaway$Date <- as.character(oak_homeaway$Date)
###select key vars
oak_homeaway <- oak_homeaway %>% select(home, away, Date) 

###oak_bat_a for batting vars (away)###
oak_bat_a <- oak_bat %>% select(Date, Opp, Rslt, X, BA)
oak_bat_a <- oak_bat_a %>% filter(X == '@') #away games
###runs for away games###
oak_bat_a$Rslt <- as.character(oak_bat_a$Rslt) #character vector
TEMP<- strsplit(x=oak_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
oak_bat_a$Raway <- substring(TEMP$V1, 2); oak_bat_a$Raway <- as.numeric(oak_bat_a$Raway)
###BA for away games###
oak_bat_a$BAa <- oak_bat_a$BA
###create away variable###
oak_bat_a$X <- ifelse(oak_bat_a$X =='@', team, oak_bat_a$X) #use of team variable name here
names(oak_bat_a)[names(oak_bat_a) == 'X'] <- 'away'
###change class of Date
oak_bat_a$Date <- as.character(oak_bat_a$Date)
###select key vars
oak_bat_a <- oak_bat_a %>% select(Date, away, Raway, BAa)

###oak_bat_h for batting vars (home)###
oak_bat_h <- oak_bat %>% select(Date, Opp, Rslt, X, BA)
oak_bat_h <- oak_bat_h %>% filter(X == '') #home games
###runs for home games###
oak_bat_h$Rslt <- as.character(oak_bat_h$Rslt) #character vector
TEMP<- strsplit(x=oak_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
oak_bat_h$Rhome <- substring(TEMP$V1, 2); oak_bat_h$Rhome <- as.numeric(oak_bat_h$Rhome)
###BA for home games###
oak_bat_h$BAh <- oak_bat_h$BA
###create home variable###
oak_bat_h$X <- ifelse(oak_bat_h$X =='', team, oak_bat_h$X) #use of team variable name here
names(oak_bat_h)[names(oak_bat_h) == 'X'] <- 'home'
###change class of Date
oak_bat_h$Date <- as.character(oak_bat_h$Date)
###select key vars
oak_bat_h <- oak_bat_h %>% select(Date, home, Rhome, BAh)

###oak_pit_a for pitching voakables (away)###
oak_pit_a <- oak_pit %>% select(Date, Opp, X, H, R)
oak_pit_a <- oak_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(oak_pit_a)[names(oak_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(oak_pit_a)[names(oak_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
oak_pit_a$X <- ifelse(oak_pit_a$X =='@', team, oak_pit_a$X) #use of team variable name here
names(oak_pit_a)[names(oak_pit_a) == 'X'] <- 'away'
###change class of Date
oak_pit_a$Date <- as.character(oak_pit_a$Date)
###select key vars
oak_pit_a <- oak_pit_a %>% select(Date, away, HHAa, RSAa)

###oak_pit_h for pitching variables (home)###
oak_pit_h <- oak_pit %>% select(Date, Opp, X, H, R)
oak_pit_h <- oak_pit_h %>% filter(X == '') #home games
###splitting oak_pit_h vector###
names(oak_pit_h)[names(oak_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(oak_pit_h)[names(oak_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
oak_pit_h$X <- ifelse(oak_pit_h$X =='', team, oak_pit_h$X) #use of team variable name here
names(oak_pit_h)[names(oak_pit_h) == 'X'] <- 'home'
###change class of Date
oak_pit_h$Date <- as.character(oak_pit_h$Date)
###filter down data
oak_pit_h <- oak_pit_h %>% select(Date, home, HHAh, RSAh)

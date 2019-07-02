library(tidyverse)
library(rethinking)

###import batting data manually: mil_bat###
###import pitching data manually: mil_pit###

team <- 'MIL' #team name variable

###mil_homeaway for home/away###
mil_homeaway <- mil_bat %>% select(Date, Opp, X)
###create home variable
mil_homeaway$X <- as.character(mil_homeaway$X)
mil_homeaway$Opp <- as.character(mil_homeaway$Opp)
mil_homeaway$X <- ifelse(mil_homeaway$X =='@', mil_homeaway$Opp, mil_homeaway$X)
mil_homeaway$X <- ifelse(mil_homeaway$X =='', team, mil_homeaway$X) #use of team variable name here
names(mil_homeaway)[names(mil_homeaway) == 'X'] <- 'home'
###create away variable
mil_homeaway$Opp <- ifelse(mil_homeaway$Opp == mil_homeaway$home, team, mil_homeaway$Opp) #use of team variable name here
names(mil_homeaway)[names(mil_homeaway) == 'Opp'] <- 'away'
###change class of Date
mil_homeaway$Date <- as.character(mil_homeaway$Date)
###select key vars
mil_homeaway <- mil_homeaway %>% select(home, away, Date) 

###mil_bat_a for batting vars (away)###
mil_bat_a <- mil_bat %>% select(Date, Opp, Rslt, X, BA)
mil_bat_a <- mil_bat_a %>% filter(X == '@') #away games
###runs for away games###
mil_bat_a$Rslt <- as.character(mil_bat_a$Rslt) #character vector
TEMP<- strsplit(x=mil_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
mil_bat_a$Raway <- substring(TEMP$V1, 2); mil_bat_a$Raway <- as.numeric(mil_bat_a$Raway)
###BA for away games###
mil_bat_a$BAa <- mil_bat_a$BA
###create away variable###
mil_bat_a$X <- ifelse(mil_bat_a$X =='@', team, mil_bat_a$X) #use of team variable name here
names(mil_bat_a)[names(mil_bat_a) == 'X'] <- 'away'
###change class of Date
mil_bat_a$Date <- as.character(mil_bat_a$Date)
###select key vars
mil_bat_a <- mil_bat_a %>% select(Date, away, Raway, BAa)

###mil_bat_h for batting vars (home)###
mil_bat_h <- mil_bat %>% select(Date, Opp, Rslt, X, BA)
mil_bat_h <- mil_bat_h %>% filter(X == '') #home games
###runs for home games###
mil_bat_h$Rslt <- as.character(mil_bat_h$Rslt) #character vector
TEMP<- strsplit(x=mil_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
mil_bat_h$Rhome <- substring(TEMP$V1, 2); mil_bat_h$Rhome <- as.numeric(mil_bat_h$Rhome)
###BA for home games###
mil_bat_h$BAh <- mil_bat_h$BA
###create home variable###
mil_bat_h$X <- ifelse(mil_bat_h$X =='', team, mil_bat_h$X) #use of team variable name here
names(mil_bat_h)[names(mil_bat_h) == 'X'] <- 'home'
###change class of Date
mil_bat_h$Date <- as.character(mil_bat_h$Date)
###select key vars
mil_bat_h <- mil_bat_h %>% select(Date, home, Rhome, BAh)

###mil_pit_a for pitching vmilables (away)###
mil_pit_a <- mil_pit %>% select(Date, Opp, X, H, R)
mil_pit_a <- mil_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(mil_pit_a)[names(mil_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(mil_pit_a)[names(mil_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
mil_pit_a$X <- ifelse(mil_pit_a$X =='@', team, mil_pit_a$X) #use of team variable name here
names(mil_pit_a)[names(mil_pit_a) == 'X'] <- 'away'
###change class of Date
mil_pit_a$Date <- as.character(mil_pit_a$Date)
###select key vars
mil_pit_a <- mil_pit_a %>% select(Date, away, HHAa, RSAa)

###mil_pit_h for pitching variables (home)###
mil_pit_h <- mil_pit %>% select(Date, Opp, X, H, R)
mil_pit_h <- mil_pit_h %>% filter(X == '') #home games
###splitting mil_pit_h vector###
names(mil_pit_h)[names(mil_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(mil_pit_h)[names(mil_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
mil_pit_h$X <- ifelse(mil_pit_h$X =='', team, mil_pit_h$X) #use of team variable name here
names(mil_pit_h)[names(mil_pit_h) == 'X'] <- 'home'
###change class of Date
mil_pit_h$Date <- as.character(mil_pit_h$Date)
###filter down data
mil_pit_h <- mil_pit_h %>% select(Date, home, HHAh, RSAh)

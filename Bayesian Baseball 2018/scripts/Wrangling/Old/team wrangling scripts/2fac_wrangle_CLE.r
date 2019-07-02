library(tidyverse)
library(rethinking)

###import batting data manually: cle_bat###
###import pitching data manually: cle_pit###

team <- 'CLE' #team name variable

###cle_homeaway for home/away###
cle_homeaway <- cle_bat %>% select(Date, Opp, X)
###create home variable
cle_homeaway$X <- as.character(cle_homeaway$X)
cle_homeaway$Opp <- as.character(cle_homeaway$Opp)
cle_homeaway$X <- ifelse(cle_homeaway$X =='@', cle_homeaway$Opp, cle_homeaway$X)
cle_homeaway$X <- ifelse(cle_homeaway$X =='', team, cle_homeaway$X) #use of team variable name here
names(cle_homeaway)[names(cle_homeaway) == 'X'] <- 'home'
###create away variable
cle_homeaway$Opp <- ifelse(cle_homeaway$Opp == cle_homeaway$home, team, cle_homeaway$Opp) #use of team variable name here
names(cle_homeaway)[names(cle_homeaway) == 'Opp'] <- 'away'
###change class of Date
cle_homeaway$Date <- as.character(cle_homeaway$Date)
###select key vars
cle_homeaway <- cle_homeaway %>% select(home, away, Date) 

###cle_bat_a for batting vars (away)###
cle_bat_a <- cle_bat %>% select(Date, Opp, Rslt, X, BA)
cle_bat_a <- cle_bat_a %>% filter(X == '@') #away games
###runs for away games###
cle_bat_a$Rslt <- as.character(cle_bat_a$Rslt) #character vector
TEMP<- strsplit(x=cle_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
cle_bat_a$Raway <- substring(TEMP$V1, 2); cle_bat_a$Raway <- as.numeric(cle_bat_a$Raway)
###BA for away games###
cle_bat_a$BAa <- cle_bat_a$BA
###create away variable###
cle_bat_a$X <- ifelse(cle_bat_a$X =='@', team, cle_bat_a$X) #use of team variable name here
names(cle_bat_a)[names(cle_bat_a) == 'X'] <- 'away'
###change class of Date
cle_bat_a$Date <- as.character(cle_bat_a$Date)
###select key vars
cle_bat_a <- cle_bat_a %>% select(Date, away, Raway, BAa)

###cle_bat_h for batting vars (home)###
cle_bat_h <- cle_bat %>% select(Date, Opp, Rslt, X, BA)
cle_bat_h <- cle_bat_h %>% filter(X == '') #home games
###runs for home games###
cle_bat_h$Rslt <- as.character(cle_bat_h$Rslt) #character vector
TEMP<- strsplit(x=cle_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
cle_bat_h$Rhome <- substring(TEMP$V1, 2); cle_bat_h$Rhome <- as.numeric(cle_bat_h$Rhome)
###BA for home games###
cle_bat_h$BAh <- cle_bat_h$BA
###create home variable###
cle_bat_h$X <- ifelse(cle_bat_h$X =='', team, cle_bat_h$X) #use of team variable name here
names(cle_bat_h)[names(cle_bat_h) == 'X'] <- 'home'
###change class of Date
cle_bat_h$Date <- as.character(cle_bat_h$Date)
###select key vars
cle_bat_h <- cle_bat_h %>% select(Date, home, Rhome, BAh)

###cle_pit_a for pitching vcleables (away)###
cle_pit_a <- cle_pit %>% select(Date, Opp, X, H, R)
cle_pit_a <- cle_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(cle_pit_a)[names(cle_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(cle_pit_a)[names(cle_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
cle_pit_a$X <- ifelse(cle_pit_a$X =='@', team, cle_pit_a$X) #use of team variable name here
names(cle_pit_a)[names(cle_pit_a) == 'X'] <- 'away'
###change class of Date
cle_pit_a$Date <- as.character(cle_pit_a$Date)
###select key vars
cle_pit_a <- cle_pit_a %>% select(Date, away, HHAa, RSAa)

###cle_pit_h for pitching variables (home)###
cle_pit_h <- cle_pit %>% select(Date, Opp, X, H, R)
cle_pit_h <- cle_pit_h %>% filter(X == '') #home games
###splitting cle_pit_h vector###
names(cle_pit_h)[names(cle_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(cle_pit_h)[names(cle_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
cle_pit_h$X <- ifelse(cle_pit_h$X =='', team, cle_pit_h$X) #use of team variable name here
names(cle_pit_h)[names(cle_pit_h) == 'X'] <- 'home'
###change class of Date
cle_pit_h$Date <- as.character(cle_pit_h$Date)
###filter down data
cle_pit_h <- cle_pit_h %>% select(Date, home, HHAh, RSAh)

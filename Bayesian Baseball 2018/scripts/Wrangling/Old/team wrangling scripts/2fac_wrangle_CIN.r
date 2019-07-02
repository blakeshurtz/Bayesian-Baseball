library(tidyverse)
library(rethinking)

###import batting data manually: cin_bat###
###import pitching data manually: cin_pit###

team <- 'CIN' #team name variable

###cin_homeaway for home/away###
cin_homeaway <- cin_bat %>% select(Date, Opp, X)
###create home variable
cin_homeaway$X <- as.character(cin_homeaway$X)
cin_homeaway$Opp <- as.character(cin_homeaway$Opp)
cin_homeaway$X <- ifelse(cin_homeaway$X =='@', cin_homeaway$Opp, cin_homeaway$X)
cin_homeaway$X <- ifelse(cin_homeaway$X =='', team, cin_homeaway$X) #use of team variable name here
names(cin_homeaway)[names(cin_homeaway) == 'X'] <- 'home'
###create away variable
cin_homeaway$Opp <- ifelse(cin_homeaway$Opp == cin_homeaway$home, team, cin_homeaway$Opp) #use of team variable name here
names(cin_homeaway)[names(cin_homeaway) == 'Opp'] <- 'away'
###change class of Date
cin_homeaway$Date <- as.character(cin_homeaway$Date)
###select key vars
cin_homeaway <- cin_homeaway %>% select(home, away, Date) 

###cin_bat_a for batting vars (away)###
cin_bat_a <- cin_bat %>% select(Date, Opp, Rslt, X, BA)
cin_bat_a <- cin_bat_a %>% filter(X == '@') #away games
###runs for away games###
cin_bat_a$Rslt <- as.character(cin_bat_a$Rslt) #character vector
TEMP<- strsplit(x=cin_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
cin_bat_a$Raway <- substring(TEMP$V1, 2); cin_bat_a$Raway <- as.numeric(cin_bat_a$Raway)
###BA for away games###
cin_bat_a$BAa <- cin_bat_a$BA
###create away variable###
cin_bat_a$X <- ifelse(cin_bat_a$X =='@', team, cin_bat_a$X) #use of team variable name here
names(cin_bat_a)[names(cin_bat_a) == 'X'] <- 'away'
###change class of Date
cin_bat_a$Date <- as.character(cin_bat_a$Date)
###select key vars
cin_bat_a <- cin_bat_a %>% select(Date, away, Raway, BAa)

###cin_bat_h for batting vars (home)###
cin_bat_h <- cin_bat %>% select(Date, Opp, Rslt, X, BA)
cin_bat_h <- cin_bat_h %>% filter(X == '') #home games
###runs for home games###
cin_bat_h$Rslt <- as.character(cin_bat_h$Rslt) #character vector
TEMP<- strsplit(x=cin_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
cin_bat_h$Rhome <- substring(TEMP$V1, 2); cin_bat_h$Rhome <- as.numeric(cin_bat_h$Rhome)
###BA for home games###
cin_bat_h$BAh <- cin_bat_h$BA
###create home variable###
cin_bat_h$X <- ifelse(cin_bat_h$X =='', team, cin_bat_h$X) #use of team variable name here
names(cin_bat_h)[names(cin_bat_h) == 'X'] <- 'home'
###change class of Date
cin_bat_h$Date <- as.character(cin_bat_h$Date)
###select key vars
cin_bat_h <- cin_bat_h %>% select(Date, home, Rhome, BAh)

###cin_pit_a for pitching vcinables (away)###
cin_pit_a <- cin_pit %>% select(Date, Opp, X, H, R)
cin_pit_a <- cin_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(cin_pit_a)[names(cin_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(cin_pit_a)[names(cin_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
cin_pit_a$X <- ifelse(cin_pit_a$X =='@', team, cin_pit_a$X) #use of team variable name here
names(cin_pit_a)[names(cin_pit_a) == 'X'] <- 'away'
###change class of Date
cin_pit_a$Date <- as.character(cin_pit_a$Date)
###select key vars
cin_pit_a <- cin_pit_a %>% select(Date, away, HHAa, RSAa)

###cin_pit_h for pitching variables (home)###
cin_pit_h <- cin_pit %>% select(Date, Opp, X, H, R)
cin_pit_h <- cin_pit_h %>% filter(X == '') #home games
###splitting cin_pit_h vector###
names(cin_pit_h)[names(cin_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(cin_pit_h)[names(cin_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
cin_pit_h$X <- ifelse(cin_pit_h$X =='', team, cin_pit_h$X) #use of team variable name here
names(cin_pit_h)[names(cin_pit_h) == 'X'] <- 'home'
###change class of Date
cin_pit_h$Date <- as.character(cin_pit_h$Date)
###filter down data
cin_pit_h <- cin_pit_h %>% select(Date, home, HHAh, RSAh)

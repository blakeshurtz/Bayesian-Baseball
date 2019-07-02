library(tidyverse)
library(rethinking)

###import batting data manually: tor_bat###
###import pitching data manually: tor_pit###

team <- 'TOR' #team name variable

###tor_homeaway for home/away###
tor_homeaway <- tor_bat %>% select(Date, Opp, X)
###create home variable
tor_homeaway$X <- as.character(tor_homeaway$X)
tor_homeaway$Opp <- as.character(tor_homeaway$Opp)
tor_homeaway$X <- ifelse(tor_homeaway$X =='@', tor_homeaway$Opp, tor_homeaway$X)
tor_homeaway$X <- ifelse(tor_homeaway$X =='', team, tor_homeaway$X) #use of team variable name here
names(tor_homeaway)[names(tor_homeaway) == 'X'] <- 'home'
###create away variable
tor_homeaway$Opp <- ifelse(tor_homeaway$Opp == tor_homeaway$home, team, tor_homeaway$Opp) #use of team variable name here
names(tor_homeaway)[names(tor_homeaway) == 'Opp'] <- 'away'
###change class of Date
tor_homeaway$Date <- as.character(tor_homeaway$Date)
###select key vars
tor_homeaway <- tor_homeaway %>% select(home, away, Date) 

###tor_bat_a for batting vars (away)###
tor_bat_a <- tor_bat %>% select(Date, Opp, Rslt, X, BA)
tor_bat_a <- tor_bat_a %>% filter(X == '@') #away games
###runs for away games###
tor_bat_a$Rslt <- as.character(tor_bat_a$Rslt) #character vector
TEMP<- strsplit(x=tor_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
tor_bat_a$Raway <- substring(TEMP$V1, 2); tor_bat_a$Raway <- as.numeric(tor_bat_a$Raway)
###BA for away games###
tor_bat_a$BAa <- tor_bat_a$BA
###create away variable###
tor_bat_a$X <- ifelse(tor_bat_a$X =='@', team, tor_bat_a$X) #use of team variable name here
names(tor_bat_a)[names(tor_bat_a) == 'X'] <- 'away'
###change class of Date
tor_bat_a$Date <- as.character(tor_bat_a$Date)
###select key vars
tor_bat_a <- tor_bat_a %>% select(Date, away, Raway, BAa)

###tor_bat_h for batting vars (home)###
tor_bat_h <- tor_bat %>% select(Date, Opp, Rslt, X, BA)
tor_bat_h <- tor_bat_h %>% filter(X == '') #home games
###runs for home games###
tor_bat_h$Rslt <- as.character(tor_bat_h$Rslt) #character vector
TEMP<- strsplit(x=tor_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
tor_bat_h$Rhome <- substring(TEMP$V1, 2); tor_bat_h$Rhome <- as.numeric(tor_bat_h$Rhome)
###BA for home games###
tor_bat_h$BAh <- tor_bat_h$BA
###create home variable###
tor_bat_h$X <- ifelse(tor_bat_h$X =='', team, tor_bat_h$X) #use of team variable name here
names(tor_bat_h)[names(tor_bat_h) == 'X'] <- 'home'
###change class of Date
tor_bat_h$Date <- as.character(tor_bat_h$Date)
###select key vars
tor_bat_h <- tor_bat_h %>% select(Date, home, Rhome, BAh)

###tor_pit_a for pitching vtorables (away)###
tor_pit_a <- tor_pit %>% select(Date, Opp, X, H, R)
tor_pit_a <- tor_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(tor_pit_a)[names(tor_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(tor_pit_a)[names(tor_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
tor_pit_a$X <- ifelse(tor_pit_a$X =='@', team, tor_pit_a$X) #use of team variable name here
names(tor_pit_a)[names(tor_pit_a) == 'X'] <- 'away'
###change class of Date
tor_pit_a$Date <- as.character(tor_pit_a$Date)
###select key vars
tor_pit_a <- tor_pit_a %>% select(Date, away, HHAa, RSAa)

###tor_pit_h for pitching variables (home)###
tor_pit_h <- tor_pit %>% select(Date, Opp, X, H, R)
tor_pit_h <- tor_pit_h %>% filter(X == '') #home games
###splitting tor_pit_h vector###
names(tor_pit_h)[names(tor_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(tor_pit_h)[names(tor_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
tor_pit_h$X <- ifelse(tor_pit_h$X =='', team, tor_pit_h$X) #use of team variable name here
names(tor_pit_h)[names(tor_pit_h) == 'X'] <- 'home'
###change class of Date
tor_pit_h$Date <- as.character(tor_pit_h$Date)
###filter down data
tor_pit_h <- tor_pit_h %>% select(Date, home, HHAh, RSAh)

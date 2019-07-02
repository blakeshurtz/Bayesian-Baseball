library(tidyverse)
library(rethinking)

###import batting data manually: sfg_bat###
###import pitching data manually: sfg_pit###

team <- 'SFG' #team name variable

###sfg_homeaway for home/away###
sfg_homeaway <- sfg_bat %>% select(Date, Opp, X)
###create home variable
sfg_homeaway$X <- as.character(sfg_homeaway$X)
sfg_homeaway$Opp <- as.character(sfg_homeaway$Opp)
sfg_homeaway$X <- ifelse(sfg_homeaway$X =='@', sfg_homeaway$Opp, sfg_homeaway$X)
sfg_homeaway$X <- ifelse(sfg_homeaway$X =='', team, sfg_homeaway$X) #use of team variable name here
names(sfg_homeaway)[names(sfg_homeaway) == 'X'] <- 'home'
###create away variable
sfg_homeaway$Opp <- ifelse(sfg_homeaway$Opp == sfg_homeaway$home, team, sfg_homeaway$Opp) #use of team variable name here
names(sfg_homeaway)[names(sfg_homeaway) == 'Opp'] <- 'away'
###change class of Date
sfg_homeaway$Date <- as.character(sfg_homeaway$Date)
###select key vars
sfg_homeaway <- sfg_homeaway %>% select(home, away, Date) 

###sfg_bat_a for batting vars (away)###
sfg_bat_a <- sfg_bat %>% select(Date, Opp, Rslt, X, BA)
sfg_bat_a <- sfg_bat_a %>% filter(X == '@') #away games
###runs for away games###
sfg_bat_a$Rslt <- as.character(sfg_bat_a$Rslt) #character vector
TEMP<- strsplit(x=sfg_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
sfg_bat_a$Raway <- substring(TEMP$V1, 2); sfg_bat_a$Raway <- as.numeric(sfg_bat_a$Raway)
###BA for away games###
sfg_bat_a$BAa <- sfg_bat_a$BA
###create away variable###
sfg_bat_a$X <- ifelse(sfg_bat_a$X =='@', team, sfg_bat_a$X) #use of team variable name here
names(sfg_bat_a)[names(sfg_bat_a) == 'X'] <- 'away'
###change class of Date
sfg_bat_a$Date <- as.character(sfg_bat_a$Date)
###select key vars
sfg_bat_a <- sfg_bat_a %>% select(Date, away, Raway, BAa)

###sfg_bat_h for batting vars (home)###
sfg_bat_h <- sfg_bat %>% select(Date, Opp, Rslt, X, BA)
sfg_bat_h <- sfg_bat_h %>% filter(X == '') #home games
###runs for home games###
sfg_bat_h$Rslt <- as.character(sfg_bat_h$Rslt) #character vector
TEMP<- strsplit(x=sfg_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
sfg_bat_h$Rhome <- substring(TEMP$V1, 2); sfg_bat_h$Rhome <- as.numeric(sfg_bat_h$Rhome)
###BA for home games###
sfg_bat_h$BAh <- sfg_bat_h$BA
###create home variable###
sfg_bat_h$X <- ifelse(sfg_bat_h$X =='', team, sfg_bat_h$X) #use of team variable name here
names(sfg_bat_h)[names(sfg_bat_h) == 'X'] <- 'home'
###change class of Date
sfg_bat_h$Date <- as.character(sfg_bat_h$Date)
###select key vars
sfg_bat_h <- sfg_bat_h %>% select(Date, home, Rhome, BAh)

###sfg_pit_a for pitching vsfgables (away)###
sfg_pit_a <- sfg_pit %>% select(Date, Opp, X, H, R)
sfg_pit_a <- sfg_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(sfg_pit_a)[names(sfg_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(sfg_pit_a)[names(sfg_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
sfg_pit_a$X <- ifelse(sfg_pit_a$X =='@', team, sfg_pit_a$X) #use of team variable name here
names(sfg_pit_a)[names(sfg_pit_a) == 'X'] <- 'away'
###change class of Date
sfg_pit_a$Date <- as.character(sfg_pit_a$Date)
###select key vars
sfg_pit_a <- sfg_pit_a %>% select(Date, away, HHAa, RSAa)

###sfg_pit_h for pitching variables (home)###
sfg_pit_h <- sfg_pit %>% select(Date, Opp, X, H, R)
sfg_pit_h <- sfg_pit_h %>% filter(X == '') #home games
###splitting sfg_pit_h vector###
names(sfg_pit_h)[names(sfg_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(sfg_pit_h)[names(sfg_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
sfg_pit_h$X <- ifelse(sfg_pit_h$X =='', team, sfg_pit_h$X) #use of team variable name here
names(sfg_pit_h)[names(sfg_pit_h) == 'X'] <- 'home'
###change class of Date
sfg_pit_h$Date <- as.character(sfg_pit_h$Date)
###filter down data
sfg_pit_h <- sfg_pit_h %>% select(Date, home, HHAh, RSAh)

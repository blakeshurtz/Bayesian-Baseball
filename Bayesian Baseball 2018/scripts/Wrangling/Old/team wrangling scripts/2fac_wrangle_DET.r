library(tidyverse)
library(rethinking)

###import batting data manually: det_bat###
###import pitching data manually: det_pit###

team <- 'DET' #team name variable

###det_homeaway for home/away###
det_homeaway <- det_bat %>% select(Date, Opp, X)
###create home variable
det_homeaway$X <- as.character(det_homeaway$X)
det_homeaway$Opp <- as.character(det_homeaway$Opp)
det_homeaway$X <- ifelse(det_homeaway$X =='@', det_homeaway$Opp, det_homeaway$X)
det_homeaway$X <- ifelse(det_homeaway$X =='', team, det_homeaway$X) #use of team variable name here
names(det_homeaway)[names(det_homeaway) == 'X'] <- 'home'
###create away variable
det_homeaway$Opp <- ifelse(det_homeaway$Opp == det_homeaway$home, team, det_homeaway$Opp) #use of team variable name here
names(det_homeaway)[names(det_homeaway) == 'Opp'] <- 'away'
###change class of Date
det_homeaway$Date <- as.character(det_homeaway$Date)
###select key vars
det_homeaway <- det_homeaway %>% select(home, away, Date) 

###det_bat_a for batting vars (away)###
det_bat_a <- det_bat %>% select(Date, Opp, Rslt, X, BA)
det_bat_a <- det_bat_a %>% filter(X == '@') #away games
###runs for away games###
det_bat_a$Rslt <- as.character(det_bat_a$Rslt) #character vector
TEMP<- strsplit(x=det_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
det_bat_a$Raway <- substring(TEMP$V1, 2); det_bat_a$Raway <- as.numeric(det_bat_a$Raway)
###BA for away games###
det_bat_a$BAa <- det_bat_a$BA
###create away variable###
det_bat_a$X <- ifelse(det_bat_a$X =='@', team, det_bat_a$X) #use of team variable name here
names(det_bat_a)[names(det_bat_a) == 'X'] <- 'away'
###change class of Date
det_bat_a$Date <- as.character(det_bat_a$Date)
###select key vars
det_bat_a <- det_bat_a %>% select(Date, away, Raway, BAa)

###det_bat_h for batting vars (home)###
det_bat_h <- det_bat %>% select(Date, Opp, Rslt, X, BA)
det_bat_h <- det_bat_h %>% filter(X == '') #home games
###runs for home games###
det_bat_h$Rslt <- as.character(det_bat_h$Rslt) #character vector
TEMP<- strsplit(x=det_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
det_bat_h$Rhome <- substring(TEMP$V1, 2); det_bat_h$Rhome <- as.numeric(det_bat_h$Rhome)
###BA for home games###
det_bat_h$BAh <- det_bat_h$BA
###create home variable###
det_bat_h$X <- ifelse(det_bat_h$X =='', team, det_bat_h$X) #use of team variable name here
names(det_bat_h)[names(det_bat_h) == 'X'] <- 'home'
###change class of Date
det_bat_h$Date <- as.character(det_bat_h$Date)
###select key vars
det_bat_h <- det_bat_h %>% select(Date, home, Rhome, BAh)

###det_pit_a for pitching vdetables (away)###
det_pit_a <- det_pit %>% select(Date, Opp, X, H, R)
det_pit_a <- det_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(det_pit_a)[names(det_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(det_pit_a)[names(det_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
det_pit_a$X <- ifelse(det_pit_a$X =='@', team, det_pit_a$X) #use of team variable name here
names(det_pit_a)[names(det_pit_a) == 'X'] <- 'away'
###change class of Date
det_pit_a$Date <- as.character(det_pit_a$Date)
###select key vars
det_pit_a <- det_pit_a %>% select(Date, away, HHAa, RSAa)

###det_pit_h for pitching variables (home)###
det_pit_h <- det_pit %>% select(Date, Opp, X, H, R)
det_pit_h <- det_pit_h %>% filter(X == '') #home games
###splitting det_pit_h vector###
names(det_pit_h)[names(det_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(det_pit_h)[names(det_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
det_pit_h$X <- ifelse(det_pit_h$X =='', team, det_pit_h$X) #use of team variable name here
names(det_pit_h)[names(det_pit_h) == 'X'] <- 'home'
###change class of Date
det_pit_h$Date <- as.character(det_pit_h$Date)
###filter down data
det_pit_h <- det_pit_h %>% select(Date, home, HHAh, RSAh)

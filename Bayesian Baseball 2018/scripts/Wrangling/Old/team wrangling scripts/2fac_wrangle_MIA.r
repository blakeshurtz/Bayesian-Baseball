library(tidyverse)
library(rethinking)

###import batting data manually: mia_bat###
###import pitching data manually: mia_pit###

team <- 'MIA' #team name variable

###mia_homeaway for home/away###
mia_homeaway <- mia_bat %>% select(Date, Opp, X)
###create home variable
mia_homeaway$X <- as.character(mia_homeaway$X)
mia_homeaway$Opp <- as.character(mia_homeaway$Opp)
mia_homeaway$X <- ifelse(mia_homeaway$X =='@', mia_homeaway$Opp, mia_homeaway$X)
mia_homeaway$X <- ifelse(mia_homeaway$X =='', team, mia_homeaway$X) #use of team variable name here
names(mia_homeaway)[names(mia_homeaway) == 'X'] <- 'home'
###create away variable
mia_homeaway$Opp <- ifelse(mia_homeaway$Opp == mia_homeaway$home, team, mia_homeaway$Opp) #use of team variable name here
names(mia_homeaway)[names(mia_homeaway) == 'Opp'] <- 'away'
###change class of Date
mia_homeaway$Date <- as.character(mia_homeaway$Date)
###select key vars
mia_homeaway <- mia_homeaway %>% select(home, away, Date) 

###mia_bat_a for batting vars (away)###
mia_bat_a <- mia_bat %>% select(Date, Opp, Rslt, X, BA)
mia_bat_a <- mia_bat_a %>% filter(X == '@') #away games
###runs for away games###
mia_bat_a$Rslt <- as.character(mia_bat_a$Rslt) #character vector
TEMP<- strsplit(x=mia_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
mia_bat_a$Raway <- substring(TEMP$V1, 2); mia_bat_a$Raway <- as.numeric(mia_bat_a$Raway)
###BA for away games###
mia_bat_a$BAa <- mia_bat_a$BA
###create away variable###
mia_bat_a$X <- ifelse(mia_bat_a$X =='@', team, mia_bat_a$X) #use of team variable name here
names(mia_bat_a)[names(mia_bat_a) == 'X'] <- 'away'
###change class of Date
mia_bat_a$Date <- as.character(mia_bat_a$Date)
###select key vars
mia_bat_a <- mia_bat_a %>% select(Date, away, Raway, BAa)

###mia_bat_h for batting vars (home)###
mia_bat_h <- mia_bat %>% select(Date, Opp, Rslt, X, BA)
mia_bat_h <- mia_bat_h %>% filter(X == '') #home games
###runs for home games###
mia_bat_h$Rslt <- as.character(mia_bat_h$Rslt) #character vector
TEMP<- strsplit(x=mia_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
mia_bat_h$Rhome <- substring(TEMP$V1, 2); mia_bat_h$Rhome <- as.numeric(mia_bat_h$Rhome)
###BA for home games###
mia_bat_h$BAh <- mia_bat_h$BA
###create home variable###
mia_bat_h$X <- ifelse(mia_bat_h$X =='', team, mia_bat_h$X) #use of team variable name here
names(mia_bat_h)[names(mia_bat_h) == 'X'] <- 'home'
###change class of Date
mia_bat_h$Date <- as.character(mia_bat_h$Date)
###select key vars
mia_bat_h <- mia_bat_h %>% select(Date, home, Rhome, BAh)

###mia_pit_a for pitching vmiaables (away)###
mia_pit_a <- mia_pit %>% select(Date, Opp, X, H, R)
mia_pit_a <- mia_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(mia_pit_a)[names(mia_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(mia_pit_a)[names(mia_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
mia_pit_a$X <- ifelse(mia_pit_a$X =='@', team, mia_pit_a$X) #use of team variable name here
names(mia_pit_a)[names(mia_pit_a) == 'X'] <- 'away'
###change class of Date
mia_pit_a$Date <- as.character(mia_pit_a$Date)
###select key vars
mia_pit_a <- mia_pit_a %>% select(Date, away, HHAa, RSAa)

###mia_pit_h for pitching variables (home)###
mia_pit_h <- mia_pit %>% select(Date, Opp, X, H, R)
mia_pit_h <- mia_pit_h %>% filter(X == '') #home games
###splitting mia_pit_h vector###
names(mia_pit_h)[names(mia_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(mia_pit_h)[names(mia_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
mia_pit_h$X <- ifelse(mia_pit_h$X =='', team, mia_pit_h$X) #use of team variable name here
names(mia_pit_h)[names(mia_pit_h) == 'X'] <- 'home'
###change class of Date
mia_pit_h$Date <- as.character(mia_pit_h$Date)
###filter down data
mia_pit_h <- mia_pit_h %>% select(Date, home, HHAh, RSAh)

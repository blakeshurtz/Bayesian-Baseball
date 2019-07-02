library(tidyverse)
library(rethinking)

###import batting data manually: pit_bat###
###import pitching data manually: pit_pit###

team <- 'PIT' #team name variable

###pit_homeaway for home/away###
pit_homeaway <- pit_bat %>% select(Date, Opp, X)
###create home variable
pit_homeaway$X <- as.character(pit_homeaway$X)
pit_homeaway$Opp <- as.character(pit_homeaway$Opp)
pit_homeaway$X <- ifelse(pit_homeaway$X =='@', pit_homeaway$Opp, pit_homeaway$X)
pit_homeaway$X <- ifelse(pit_homeaway$X =='', team, pit_homeaway$X) #use of team variable name here
names(pit_homeaway)[names(pit_homeaway) == 'X'] <- 'home'
###create away variable
pit_homeaway$Opp <- ifelse(pit_homeaway$Opp == pit_homeaway$home, team, pit_homeaway$Opp) #use of team variable name here
names(pit_homeaway)[names(pit_homeaway) == 'Opp'] <- 'away'
###change class of Date
pit_homeaway$Date <- as.character(pit_homeaway$Date)
###select key vars
pit_homeaway <- pit_homeaway %>% select(home, away, Date) 

###pit_bat_a for batting vars (away)###
pit_bat_a <- pit_bat %>% select(Date, Opp, Rslt, X, BA)
pit_bat_a <- pit_bat_a %>% filter(X == '@') #away games
###runs for away games###
pit_bat_a$Rslt <- as.character(pit_bat_a$Rslt) #character vector
TEMP<- strsplit(x=pit_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
pit_bat_a$Raway <- substring(TEMP$V1, 2); pit_bat_a$Raway <- as.numeric(pit_bat_a$Raway)
###BA for away games###
pit_bat_a$BAa <- pit_bat_a$BA
###create away variable###
pit_bat_a$X <- ifelse(pit_bat_a$X =='@', team, pit_bat_a$X) #use of team variable name here
names(pit_bat_a)[names(pit_bat_a) == 'X'] <- 'away'
###change class of Date
pit_bat_a$Date <- as.character(pit_bat_a$Date)
###select key vars
pit_bat_a <- pit_bat_a %>% select(Date, away, Raway, BAa)

###pit_bat_h for batting vars (home)###
pit_bat_h <- pit_bat %>% select(Date, Opp, Rslt, X, BA)
pit_bat_h <- pit_bat_h %>% filter(X == '') #home games
###runs for home games###
pit_bat_h$Rslt <- as.character(pit_bat_h$Rslt) #character vector
TEMP<- strsplit(x=pit_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
pit_bat_h$Rhome <- substring(TEMP$V1, 2); pit_bat_h$Rhome <- as.numeric(pit_bat_h$Rhome)
###BA for home games###
pit_bat_h$BAh <- pit_bat_h$BA
###create home variable###
pit_bat_h$X <- ifelse(pit_bat_h$X =='', team, pit_bat_h$X) #use of team variable name here
names(pit_bat_h)[names(pit_bat_h) == 'X'] <- 'home'
###change class of Date
pit_bat_h$Date <- as.character(pit_bat_h$Date)
###select key vars
pit_bat_h <- pit_bat_h %>% select(Date, home, Rhome, BAh)

###pit_pit_a for pitching vpitables (away)###
pit_pit_a <- pit_pit %>% select(Date, Opp, X, H, R)
pit_pit_a <- pit_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(pit_pit_a)[names(pit_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(pit_pit_a)[names(pit_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
pit_pit_a$X <- ifelse(pit_pit_a$X =='@', team, pit_pit_a$X) #use of team variable name here
names(pit_pit_a)[names(pit_pit_a) == 'X'] <- 'away'
###change class of Date
pit_pit_a$Date <- as.character(pit_pit_a$Date)
###select key vars
pit_pit_a <- pit_pit_a %>% select(Date, away, HHAa, RSAa)

###pit_pit_h for pitching variables (home)###
pit_pit_h <- pit_pit %>% select(Date, Opp, X, H, R)
pit_pit_h <- pit_pit_h %>% filter(X == '') #home games
###splitting pit_pit_h vector###
names(pit_pit_h)[names(pit_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(pit_pit_h)[names(pit_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
pit_pit_h$X <- ifelse(pit_pit_h$X =='', team, pit_pit_h$X) #use of team variable name here
names(pit_pit_h)[names(pit_pit_h) == 'X'] <- 'home'
###change class of Date
pit_pit_h$Date <- as.character(pit_pit_h$Date)
###filter down data
pit_pit_h <- pit_pit_h %>% select(Date, home, HHAh, RSAh)

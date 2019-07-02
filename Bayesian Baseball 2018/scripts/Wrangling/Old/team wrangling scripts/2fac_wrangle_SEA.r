library(tidyverse)
library(rethinking)

###import batting data manually: sea_bat###
###import pitching data manually: sea_pit###

team <- 'SEA' #team name variable

###sea_homeaway for home/away###
sea_homeaway <- sea_bat %>% select(Date, Opp, X)
###create home variable
sea_homeaway$X <- as.character(sea_homeaway$X)
sea_homeaway$Opp <- as.character(sea_homeaway$Opp)
sea_homeaway$X <- ifelse(sea_homeaway$X =='@', sea_homeaway$Opp, sea_homeaway$X)
sea_homeaway$X <- ifelse(sea_homeaway$X =='', team, sea_homeaway$X) #use of team variable name here
names(sea_homeaway)[names(sea_homeaway) == 'X'] <- 'home'
###create away variable
sea_homeaway$Opp <- ifelse(sea_homeaway$Opp == sea_homeaway$home, team, sea_homeaway$Opp) #use of team variable name here
names(sea_homeaway)[names(sea_homeaway) == 'Opp'] <- 'away'
###change class of Date
sea_homeaway$Date <- as.character(sea_homeaway$Date)
###select key vars
sea_homeaway <- sea_homeaway %>% select(home, away, Date) 

###sea_bat_a for batting vars (away)###
sea_bat_a <- sea_bat %>% select(Date, Opp, Rslt, X, BA)
sea_bat_a <- sea_bat_a %>% filter(X == '@') #away games
###runs for away games###
sea_bat_a$Rslt <- as.character(sea_bat_a$Rslt) #character vector
TEMP<- strsplit(x=sea_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
sea_bat_a$Raway <- substring(TEMP$V1, 2); sea_bat_a$Raway <- as.numeric(sea_bat_a$Raway)
###BA for away games###
sea_bat_a$BAa <- sea_bat_a$BA
###create away variable###
sea_bat_a$X <- ifelse(sea_bat_a$X =='@', team, sea_bat_a$X) #use of team variable name here
names(sea_bat_a)[names(sea_bat_a) == 'X'] <- 'away'
###change class of Date
sea_bat_a$Date <- as.character(sea_bat_a$Date)
###select key vars
sea_bat_a <- sea_bat_a %>% select(Date, away, Raway, BAa)

###sea_bat_h for batting vars (home)###
sea_bat_h <- sea_bat %>% select(Date, Opp, Rslt, X, BA)
sea_bat_h <- sea_bat_h %>% filter(X == '') #home games
###runs for home games###
sea_bat_h$Rslt <- as.character(sea_bat_h$Rslt) #character vector
TEMP<- strsplit(x=sea_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
sea_bat_h$Rhome <- substring(TEMP$V1, 2); sea_bat_h$Rhome <- as.numeric(sea_bat_h$Rhome)
###BA for home games###
sea_bat_h$BAh <- sea_bat_h$BA
###create home variable###
sea_bat_h$X <- ifelse(sea_bat_h$X =='', team, sea_bat_h$X) #use of team variable name here
names(sea_bat_h)[names(sea_bat_h) == 'X'] <- 'home'
###change class of Date
sea_bat_h$Date <- as.character(sea_bat_h$Date)
###select key vars
sea_bat_h <- sea_bat_h %>% select(Date, home, Rhome, BAh)

###sea_pit_a for pitching vseaables (away)###
sea_pit_a <- sea_pit %>% select(Date, Opp, X, H, R)
sea_pit_a <- sea_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(sea_pit_a)[names(sea_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(sea_pit_a)[names(sea_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
sea_pit_a$X <- ifelse(sea_pit_a$X =='@', team, sea_pit_a$X) #use of team variable name here
names(sea_pit_a)[names(sea_pit_a) == 'X'] <- 'away'
###change class of Date
sea_pit_a$Date <- as.character(sea_pit_a$Date)
###select key vars
sea_pit_a <- sea_pit_a %>% select(Date, away, HHAa, RSAa)

###sea_pit_h for pitching variables (home)###
sea_pit_h <- sea_pit %>% select(Date, Opp, X, H, R)
sea_pit_h <- sea_pit_h %>% filter(X == '') #home games
###splitting sea_pit_h vector###
names(sea_pit_h)[names(sea_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(sea_pit_h)[names(sea_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
sea_pit_h$X <- ifelse(sea_pit_h$X =='', team, sea_pit_h$X) #use of team variable name here
names(sea_pit_h)[names(sea_pit_h) == 'X'] <- 'home'
###change class of Date
sea_pit_h$Date <- as.character(sea_pit_h$Date)
###filter down data
sea_pit_h <- sea_pit_h %>% select(Date, home, HHAh, RSAh)

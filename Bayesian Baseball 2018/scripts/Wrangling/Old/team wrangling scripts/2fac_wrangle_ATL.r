library(tidyverse)
library(rethinking)

###import batting data manually: atl_bat###
###import pitching data manually: atl_pit###

team <- 'ATL' #team name variable

###atl_homeaway for home/away###
atl_homeaway <- atl_bat %>% select(Date, Opp, X)
###create home variable
atl_homeaway$X <- as.character(atl_homeaway$X)
atl_homeaway$Opp <- as.character(atl_homeaway$Opp)
atl_homeaway$X <- ifelse(atl_homeaway$X =='@', atl_homeaway$Opp, atl_homeaway$X)
atl_homeaway$X <- ifelse(atl_homeaway$X =='', team, atl_homeaway$X) #use of team variable name here
names(atl_homeaway)[names(atl_homeaway) == 'X'] <- 'home'
###create away variable
atl_homeaway$Opp <- ifelse(atl_homeaway$Opp == atl_homeaway$home, team, atl_homeaway$Opp) #use of team variable name here
names(atl_homeaway)[names(atl_homeaway) == 'Opp'] <- 'away'
###change class of Date
atl_homeaway$Date <- as.character(atl_homeaway$Date)
###select key vars
atl_homeaway <- atl_homeaway %>% select(home, away, Date) 

###atl_bat_a for batting vars (away)###
atl_bat_a <- atl_bat %>% select(Date, Opp, Rslt, X, BA)
atl_bat_a <- atl_bat_a %>% filter(X == '@') #away games
###runs for away games###
atl_bat_a$Rslt <- as.character(atl_bat_a$Rslt) #character vector
TEMP<- strsplit(x=atl_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
atl_bat_a$Raway <- substring(TEMP$V1, 2); atl_bat_a$Raway <- as.numeric(atl_bat_a$Raway)
###BA for away games###
atl_bat_a$BAa <- atl_bat_a$BA
###create away variable###
atl_bat_a$X <- ifelse(atl_bat_a$X =='@', team, atl_bat_a$X) #use of team variable name here
names(atl_bat_a)[names(atl_bat_a) == 'X'] <- 'away'
###change class of Date
atl_bat_a$Date <- as.character(atl_bat_a$Date)
###select key vars
atl_bat_a <- atl_bat_a %>% select(Date, away, Raway, BAa)

###atl_bat_h for batting vars (home)###
atl_bat_h <- atl_bat %>% select(Date, Opp, Rslt, X, BA)
atl_bat_h <- atl_bat_h %>% filter(X == '') #home games
###runs for home games###
atl_bat_h$Rslt <- as.character(atl_bat_h$Rslt) #character vector
TEMP<- strsplit(x=atl_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
atl_bat_h$Rhome <- substring(TEMP$V1, 2); atl_bat_h$Rhome <- as.numeric(atl_bat_h$Rhome)
###BA for home games###
atl_bat_h$BAh <- atl_bat_h$BA
###create home variable###
atl_bat_h$X <- ifelse(atl_bat_h$X =='', team, atl_bat_h$X) #use of team variable name here
names(atl_bat_h)[names(atl_bat_h) == 'X'] <- 'home'
###change class of Date
atl_bat_h$Date <- as.character(atl_bat_h$Date)
###select key vars
atl_bat_h <- atl_bat_h %>% select(Date, home, Rhome, BAh)

###atl_pit_a for pitching vatlables (away)###
atl_pit_a <- atl_pit %>% select(Date, Opp, X, H, R)
atl_pit_a <- atl_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(atl_pit_a)[names(atl_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(atl_pit_a)[names(atl_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
atl_pit_a$X <- ifelse(atl_pit_a$X =='@', team, atl_pit_a$X) #use of team variable name here
names(atl_pit_a)[names(atl_pit_a) == 'X'] <- 'away'
###change class of Date
atl_pit_a$Date <- as.character(atl_pit_a$Date)
###select key vars
atl_pit_a <- atl_pit_a %>% select(Date, away, HHAa, RSAa)

###atl_pit_h for pitching variables (home)###
atl_pit_h <- atl_pit %>% select(Date, Opp, X, H, R)
atl_pit_h <- atl_pit_h %>% filter(X == '') #home games
###splitting atl_pit_h vector###
names(atl_pit_h)[names(atl_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(atl_pit_h)[names(atl_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
atl_pit_h$X <- ifelse(atl_pit_h$X =='', team, atl_pit_h$X) #use of team variable name here
names(atl_pit_h)[names(atl_pit_h) == 'X'] <- 'home'
###change class of Date
atl_pit_h$Date <- as.character(atl_pit_h$Date)
###filter down data
atl_pit_h <- atl_pit_h %>% select(Date, home, HHAh, RSAh)

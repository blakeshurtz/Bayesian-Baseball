library(tidyverse)
library(rethinking)

###import batting data manually: stl_bat###
###import pitching data manually: stl_pit###

team <- 'STL' #team name variable

###stl_homeaway for home/away###
stl_homeaway <- stl_bat %>% select(Date, Opp, X)
###create home variable
stl_homeaway$X <- as.character(stl_homeaway$X)
stl_homeaway$Opp <- as.character(stl_homeaway$Opp)
stl_homeaway$X <- ifelse(stl_homeaway$X =='@', stl_homeaway$Opp, stl_homeaway$X)
stl_homeaway$X <- ifelse(stl_homeaway$X =='', team, stl_homeaway$X) #use of team variable name here
names(stl_homeaway)[names(stl_homeaway) == 'X'] <- 'home'
###create away variable
stl_homeaway$Opp <- ifelse(stl_homeaway$Opp == stl_homeaway$home, team, stl_homeaway$Opp) #use of team variable name here
names(stl_homeaway)[names(stl_homeaway) == 'Opp'] <- 'away'
###change class of Date
stl_homeaway$Date <- as.character(stl_homeaway$Date)
###select key vars
stl_homeaway <- stl_homeaway %>% select(home, away, Date) 

###stl_bat_a for batting vars (away)###
stl_bat_a <- stl_bat %>% select(Date, Opp, Rslt, X, BA)
stl_bat_a <- stl_bat_a %>% filter(X == '@') #away games
###runs for away games###
stl_bat_a$Rslt <- as.character(stl_bat_a$Rslt) #character vector
TEMP<- strsplit(x=stl_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
stl_bat_a$Raway <- substring(TEMP$V1, 2); stl_bat_a$Raway <- as.numeric(stl_bat_a$Raway)
###BA for away games###
stl_bat_a$BAa <- stl_bat_a$BA
###create away variable###
stl_bat_a$X <- ifelse(stl_bat_a$X =='@', team, stl_bat_a$X) #use of team variable name here
names(stl_bat_a)[names(stl_bat_a) == 'X'] <- 'away'
###change class of Date
stl_bat_a$Date <- as.character(stl_bat_a$Date)
###select key vars
stl_bat_a <- stl_bat_a %>% select(Date, away, Raway, BAa)

###stl_bat_h for batting vars (home)###
stl_bat_h <- stl_bat %>% select(Date, Opp, Rslt, X, BA)
stl_bat_h <- stl_bat_h %>% filter(X == '') #home games
###runs for home games###
stl_bat_h$Rslt <- as.character(stl_bat_h$Rslt) #character vector
TEMP<- strsplit(x=stl_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
stl_bat_h$Rhome <- substring(TEMP$V1, 2); stl_bat_h$Rhome <- as.numeric(stl_bat_h$Rhome)
###BA for home games###
stl_bat_h$BAh <- stl_bat_h$BA
###create home variable###
stl_bat_h$X <- ifelse(stl_bat_h$X =='', team, stl_bat_h$X) #use of team variable name here
names(stl_bat_h)[names(stl_bat_h) == 'X'] <- 'home'
###change class of Date
stl_bat_h$Date <- as.character(stl_bat_h$Date)
###select key vars
stl_bat_h <- stl_bat_h %>% select(Date, home, Rhome, BAh)

###stl_pit_a for pitching vstlables (away)###
stl_pit_a <- stl_pit %>% select(Date, Opp, X, H, R)
stl_pit_a <- stl_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(stl_pit_a)[names(stl_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(stl_pit_a)[names(stl_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
stl_pit_a$X <- ifelse(stl_pit_a$X =='@', team, stl_pit_a$X) #use of team variable name here
names(stl_pit_a)[names(stl_pit_a) == 'X'] <- 'away'
###change class of Date
stl_pit_a$Date <- as.character(stl_pit_a$Date)
###select key vars
stl_pit_a <- stl_pit_a %>% select(Date, away, HHAa, RSAa)

###stl_pit_h for pitching variables (home)###
stl_pit_h <- stl_pit %>% select(Date, Opp, X, H, R)
stl_pit_h <- stl_pit_h %>% filter(X == '') #home games
###splitting stl_pit_h vector###
names(stl_pit_h)[names(stl_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(stl_pit_h)[names(stl_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
stl_pit_h$X <- ifelse(stl_pit_h$X =='', team, stl_pit_h$X) #use of team variable name here
names(stl_pit_h)[names(stl_pit_h) == 'X'] <- 'home'
###change class of Date
stl_pit_h$Date <- as.character(stl_pit_h$Date)
###filter down data
stl_pit_h <- stl_pit_h %>% select(Date, home, HHAh, RSAh)

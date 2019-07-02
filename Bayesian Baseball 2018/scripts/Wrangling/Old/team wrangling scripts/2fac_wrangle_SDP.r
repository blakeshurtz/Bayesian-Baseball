library(tidyverse)
library(rethinking)

###import batting data manually: sdp_bat###
###import pitching data manually: sdp_pit###

team <- 'SDP' #team name variable

###sdp_homeaway for home/away###
sdp_homeaway <- sdp_bat %>% select(Date, Opp, X)
###create home variable
sdp_homeaway$X <- as.character(sdp_homeaway$X)
sdp_homeaway$Opp <- as.character(sdp_homeaway$Opp)
sdp_homeaway$X <- ifelse(sdp_homeaway$X =='@', sdp_homeaway$Opp, sdp_homeaway$X)
sdp_homeaway$X <- ifelse(sdp_homeaway$X =='', team, sdp_homeaway$X) #use of team variable name here
names(sdp_homeaway)[names(sdp_homeaway) == 'X'] <- 'home'
###create away variable
sdp_homeaway$Opp <- ifelse(sdp_homeaway$Opp == sdp_homeaway$home, team, sdp_homeaway$Opp) #use of team variable name here
names(sdp_homeaway)[names(sdp_homeaway) == 'Opp'] <- 'away'
###change class of Date
sdp_homeaway$Date <- as.character(sdp_homeaway$Date)
###select key vars
sdp_homeaway <- sdp_homeaway %>% select(home, away, Date) 

###sdp_bat_a for batting vars (away)###
sdp_bat_a <- sdp_bat %>% select(Date, Opp, Rslt, X, BA)
sdp_bat_a <- sdp_bat_a %>% filter(X == '@') #away games
###runs for away games###
sdp_bat_a$Rslt <- as.character(sdp_bat_a$Rslt) #character vector
TEMP<- strsplit(x=sdp_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
sdp_bat_a$Raway <- substring(TEMP$V1, 2); sdp_bat_a$Raway <- as.numeric(sdp_bat_a$Raway)
###BA for away games###
sdp_bat_a$BAa <- sdp_bat_a$BA
###create away variable###
sdp_bat_a$X <- ifelse(sdp_bat_a$X =='@', team, sdp_bat_a$X) #use of team variable name here
names(sdp_bat_a)[names(sdp_bat_a) == 'X'] <- 'away'
###change class of Date
sdp_bat_a$Date <- as.character(sdp_bat_a$Date)
###select key vars
sdp_bat_a <- sdp_bat_a %>% select(Date, away, Raway, BAa)

###sdp_bat_h for batting vars (home)###
sdp_bat_h <- sdp_bat %>% select(Date, Opp, Rslt, X, BA)
sdp_bat_h <- sdp_bat_h %>% filter(X == '') #home games
###runs for home games###
sdp_bat_h$Rslt <- as.character(sdp_bat_h$Rslt) #character vector
TEMP<- strsplit(x=sdp_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
sdp_bat_h$Rhome <- substring(TEMP$V1, 2); sdp_bat_h$Rhome <- as.numeric(sdp_bat_h$Rhome)
###BA for home games###
sdp_bat_h$BAh <- sdp_bat_h$BA
###create home variable###
sdp_bat_h$X <- ifelse(sdp_bat_h$X =='', team, sdp_bat_h$X) #use of team variable name here
names(sdp_bat_h)[names(sdp_bat_h) == 'X'] <- 'home'
###change class of Date
sdp_bat_h$Date <- as.character(sdp_bat_h$Date)
###select key vars
sdp_bat_h <- sdp_bat_h %>% select(Date, home, Rhome, BAh)

###sdp_pit_a for pitching vsdpables (away)###
sdp_pit_a <- sdp_pit %>% select(Date, Opp, X, H, R)
sdp_pit_a <- sdp_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(sdp_pit_a)[names(sdp_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(sdp_pit_a)[names(sdp_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
sdp_pit_a$X <- ifelse(sdp_pit_a$X =='@', team, sdp_pit_a$X) #use of team variable name here
names(sdp_pit_a)[names(sdp_pit_a) == 'X'] <- 'away'
###change class of Date
sdp_pit_a$Date <- as.character(sdp_pit_a$Date)
###select key vars
sdp_pit_a <- sdp_pit_a %>% select(Date, away, HHAa, RSAa)

###sdp_pit_h for pitching variables (home)###
sdp_pit_h <- sdp_pit %>% select(Date, Opp, X, H, R)
sdp_pit_h <- sdp_pit_h %>% filter(X == '') #home games
###splitting sdp_pit_h vector###
names(sdp_pit_h)[names(sdp_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(sdp_pit_h)[names(sdp_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
sdp_pit_h$X <- ifelse(sdp_pit_h$X =='', team, sdp_pit_h$X) #use of team variable name here
names(sdp_pit_h)[names(sdp_pit_h) == 'X'] <- 'home'
###change class of Date
sdp_pit_h$Date <- as.character(sdp_pit_h$Date)
###filter down data
sdp_pit_h <- sdp_pit_h %>% select(Date, home, HHAh, RSAh)

library(tidyverse)
library(rethinking)

###import batting data manually: nym_bat###
###import pitching data manually: nym_pit###

team <- 'NYM' #team name variable

###nym_homeaway for home/away###
nym_homeaway <- nym_bat %>% select(Date, Opp, X)
###create home variable
nym_homeaway$X <- as.character(nym_homeaway$X)
nym_homeaway$Opp <- as.character(nym_homeaway$Opp)
nym_homeaway$X <- ifelse(nym_homeaway$X =='@', nym_homeaway$Opp, nym_homeaway$X)
nym_homeaway$X <- ifelse(nym_homeaway$X =='', team, nym_homeaway$X) #use of team variable name here
names(nym_homeaway)[names(nym_homeaway) == 'X'] <- 'home'
###create away variable
nym_homeaway$Opp <- ifelse(nym_homeaway$Opp == nym_homeaway$home, team, nym_homeaway$Opp) #use of team variable name here
names(nym_homeaway)[names(nym_homeaway) == 'Opp'] <- 'away'
###change class of Date
nym_homeaway$Date <- as.character(nym_homeaway$Date)
###select key vars
nym_homeaway <- nym_homeaway %>% select(home, away, Date) 

###nym_bat_a for batting vars (away)###
nym_bat_a <- nym_bat %>% select(Date, Opp, Rslt, X, BA)
nym_bat_a <- nym_bat_a %>% filter(X == '@') #away games
###runs for away games###
nym_bat_a$Rslt <- as.character(nym_bat_a$Rslt) #character vector
TEMP<- strsplit(x=nym_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
nym_bat_a$Raway <- substring(TEMP$V1, 2); nym_bat_a$Raway <- as.numeric(nym_bat_a$Raway)
###BA for away games###
nym_bat_a$BAa <- nym_bat_a$BA
###create away variable###
nym_bat_a$X <- ifelse(nym_bat_a$X =='@', team, nym_bat_a$X) #use of team variable name here
names(nym_bat_a)[names(nym_bat_a) == 'X'] <- 'away'
###change class of Date
nym_bat_a$Date <- as.character(nym_bat_a$Date)
###select key vars
nym_bat_a <- nym_bat_a %>% select(Date, away, Raway, BAa)

###nym_bat_h for batting vars (home)###
nym_bat_h <- nym_bat %>% select(Date, Opp, Rslt, X, BA)
nym_bat_h <- nym_bat_h %>% filter(X == '') #home games
###runs for home games###
nym_bat_h$Rslt <- as.character(nym_bat_h$Rslt) #character vector
TEMP<- strsplit(x=nym_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
nym_bat_h$Rhome <- substring(TEMP$V1, 2); nym_bat_h$Rhome <- as.numeric(nym_bat_h$Rhome)
###BA for home games###
nym_bat_h$BAh <- nym_bat_h$BA
###create home variable###
nym_bat_h$X <- ifelse(nym_bat_h$X =='', team, nym_bat_h$X) #use of team variable name here
names(nym_bat_h)[names(nym_bat_h) == 'X'] <- 'home'
###change class of Date
nym_bat_h$Date <- as.character(nym_bat_h$Date)
###select key vars
nym_bat_h <- nym_bat_h %>% select(Date, home, Rhome, BAh)

###nym_pit_a for pitching vnymables (away)###
nym_pit_a <- nym_pit %>% select(Date, Opp, X, H, R)
nym_pit_a <- nym_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(nym_pit_a)[names(nym_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(nym_pit_a)[names(nym_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
nym_pit_a$X <- ifelse(nym_pit_a$X =='@', team, nym_pit_a$X) #use of team variable name here
names(nym_pit_a)[names(nym_pit_a) == 'X'] <- 'away'
###change class of Date
nym_pit_a$Date <- as.character(nym_pit_a$Date)
###select key vars
nym_pit_a <- nym_pit_a %>% select(Date, away, HHAa, RSAa)

###nym_pit_h for pitching variables (home)###
nym_pit_h <- nym_pit %>% select(Date, Opp, X, H, R)
nym_pit_h <- nym_pit_h %>% filter(X == '') #home games
###splitting nym_pit_h vector###
names(nym_pit_h)[names(nym_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(nym_pit_h)[names(nym_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
nym_pit_h$X <- ifelse(nym_pit_h$X =='', team, nym_pit_h$X) #use of team variable name here
names(nym_pit_h)[names(nym_pit_h) == 'X'] <- 'home'
###change class of Date
nym_pit_h$Date <- as.character(nym_pit_h$Date)
###filter down data
nym_pit_h <- nym_pit_h %>% select(Date, home, HHAh, RSAh)

library(tidyverse)
library(rethinking)

###import batting data manually: laa_bat###
###import pitching data manually: laa_pit###

team <- 'LAA' #team name variable

###laa_homeaway for home/away###
laa_homeaway <- laa_bat %>% select(Date, Opp, X)
###create home variable
laa_homeaway$X <- as.character(laa_homeaway$X)
laa_homeaway$Opp <- as.character(laa_homeaway$Opp)
laa_homeaway$X <- ifelse(laa_homeaway$X =='@', laa_homeaway$Opp, laa_homeaway$X)
laa_homeaway$X <- ifelse(laa_homeaway$X =='', team, laa_homeaway$X) #use of team variable name here
names(laa_homeaway)[names(laa_homeaway) == 'X'] <- 'home'
###create away variable
laa_homeaway$Opp <- ifelse(laa_homeaway$Opp == laa_homeaway$home, team, laa_homeaway$Opp) #use of team variable name here
names(laa_homeaway)[names(laa_homeaway) == 'Opp'] <- 'away'
###change class of Date
laa_homeaway$Date <- as.character(laa_homeaway$Date)
###select key vars
laa_homeaway <- laa_homeaway %>% select(home, away, Date) 

###laa_bat_a for batting vars (away)###
laa_bat_a <- laa_bat %>% select(Date, Opp, Rslt, X, BA)
laa_bat_a <- laa_bat_a %>% filter(X == '@') #away games
###runs for away games###
laa_bat_a$Rslt <- as.character(laa_bat_a$Rslt) #character vector
TEMP<- strsplit(x=laa_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
laa_bat_a$Raway <- substring(TEMP$V1, 2); laa_bat_a$Raway <- as.numeric(laa_bat_a$Raway)
###BA for away games###
laa_bat_a$BAa <- laa_bat_a$BA
###create away variable###
laa_bat_a$X <- ifelse(laa_bat_a$X =='@', team, laa_bat_a$X) #use of team variable name here
names(laa_bat_a)[names(laa_bat_a) == 'X'] <- 'away'
###change class of Date
laa_bat_a$Date <- as.character(laa_bat_a$Date)
###select key vars
laa_bat_a <- laa_bat_a %>% select(Date, away, Raway, BAa)

###laa_bat_h for batting vars (home)###
laa_bat_h <- laa_bat %>% select(Date, Opp, Rslt, X, BA)
laa_bat_h <- laa_bat_h %>% filter(X == '') #home games
###runs for home games###
laa_bat_h$Rslt <- as.character(laa_bat_h$Rslt) #character vector
TEMP<- strsplit(x=laa_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
laa_bat_h$Rhome <- substring(TEMP$V1, 2); laa_bat_h$Rhome <- as.numeric(laa_bat_h$Rhome)
###BA for home games###
laa_bat_h$BAh <- laa_bat_h$BA
###create home variable###
laa_bat_h$X <- ifelse(laa_bat_h$X =='', team, laa_bat_h$X) #use of team variable name here
names(laa_bat_h)[names(laa_bat_h) == 'X'] <- 'home'
###change class of Date
laa_bat_h$Date <- as.character(laa_bat_h$Date)
###select key vars
laa_bat_h <- laa_bat_h %>% select(Date, home, Rhome, BAh)

###laa_pit_a for pitching vlaaables (away)###
laa_pit_a <- laa_pit %>% select(Date, Opp, X, H, R)
laa_pit_a <- laa_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(laa_pit_a)[names(laa_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(laa_pit_a)[names(laa_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
laa_pit_a$X <- ifelse(laa_pit_a$X =='@', team, laa_pit_a$X) #use of team variable name here
names(laa_pit_a)[names(laa_pit_a) == 'X'] <- 'away'
###change class of Date
laa_pit_a$Date <- as.character(laa_pit_a$Date)
###select key vars
laa_pit_a <- laa_pit_a %>% select(Date, away, HHAa, RSAa)

###laa_pit_h for pitching variables (home)###
laa_pit_h <- laa_pit %>% select(Date, Opp, X, H, R)
laa_pit_h <- laa_pit_h %>% filter(X == '') #home games
###splitting laa_pit_h vector###
names(laa_pit_h)[names(laa_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(laa_pit_h)[names(laa_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
laa_pit_h$X <- ifelse(laa_pit_h$X =='', team, laa_pit_h$X) #use of team variable name here
names(laa_pit_h)[names(laa_pit_h) == 'X'] <- 'home'
###change class of Date
laa_pit_h$Date <- as.character(laa_pit_h$Date)
###filter down data
laa_pit_h <- laa_pit_h %>% select(Date, home, HHAh, RSAh)

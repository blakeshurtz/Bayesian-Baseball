library(tidyverse)
library(rethinking)

###import batting data manually: lad_bat###
###import pitching data manually: lad_pit###

team <- 'LAD' #team name variable

###lad_homeaway for home/away###
lad_homeaway <- lad_bat %>% select(Date, Opp, X)
###create home variable
lad_homeaway$X <- as.character(lad_homeaway$X)
lad_homeaway$Opp <- as.character(lad_homeaway$Opp)
lad_homeaway$X <- ifelse(lad_homeaway$X =='@', lad_homeaway$Opp, lad_homeaway$X)
lad_homeaway$X <- ifelse(lad_homeaway$X =='', team, lad_homeaway$X) #use of team variable name here
names(lad_homeaway)[names(lad_homeaway) == 'X'] <- 'home'
###create away variable
lad_homeaway$Opp <- ifelse(lad_homeaway$Opp == lad_homeaway$home, team, lad_homeaway$Opp) #use of team variable name here
names(lad_homeaway)[names(lad_homeaway) == 'Opp'] <- 'away'
###change class of Date
lad_homeaway$Date <- as.character(lad_homeaway$Date)
###select key vars
lad_homeaway <- lad_homeaway %>% select(home, away, Date) 

###lad_bat_a for batting vars (away)###
lad_bat_a <- lad_bat %>% select(Date, Opp, Rslt, X, BA)
lad_bat_a <- lad_bat_a %>% filter(X == '@') #away games
###runs for away games###
lad_bat_a$Rslt <- as.character(lad_bat_a$Rslt) #character vector
TEMP<- strsplit(x=lad_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
lad_bat_a$Raway <- substring(TEMP$V1, 2); lad_bat_a$Raway <- as.numeric(lad_bat_a$Raway)
###BA for away games###
lad_bat_a$BAa <- lad_bat_a$BA
###create away variable###
lad_bat_a$X <- ifelse(lad_bat_a$X =='@', team, lad_bat_a$X) #use of team variable name here
names(lad_bat_a)[names(lad_bat_a) == 'X'] <- 'away'
###change class of Date
lad_bat_a$Date <- as.character(lad_bat_a$Date)
###select key vars
lad_bat_a <- lad_bat_a %>% select(Date, away, Raway, BAa)

###lad_bat_h for batting vars (home)###
lad_bat_h <- lad_bat %>% select(Date, Opp, Rslt, X, BA)
lad_bat_h <- lad_bat_h %>% filter(X == '') #home games
###runs for home games###
lad_bat_h$Rslt <- as.character(lad_bat_h$Rslt) #character vector
TEMP<- strsplit(x=lad_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
lad_bat_h$Rhome <- substring(TEMP$V1, 2); lad_bat_h$Rhome <- as.numeric(lad_bat_h$Rhome)
###BA for home games###
lad_bat_h$BAh <- lad_bat_h$BA
###create home variable###
lad_bat_h$X <- ifelse(lad_bat_h$X =='', team, lad_bat_h$X) #use of team variable name here
names(lad_bat_h)[names(lad_bat_h) == 'X'] <- 'home'
###change class of Date
lad_bat_h$Date <- as.character(lad_bat_h$Date)
###select key vars
lad_bat_h <- lad_bat_h %>% select(Date, home, Rhome, BAh)

###lad_pit_a for pitching vladables (away)###
lad_pit_a <- lad_pit %>% select(Date, Opp, X, H, R)
lad_pit_a <- lad_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(lad_pit_a)[names(lad_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(lad_pit_a)[names(lad_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
lad_pit_a$X <- ifelse(lad_pit_a$X =='@', team, lad_pit_a$X) #use of team variable name here
names(lad_pit_a)[names(lad_pit_a) == 'X'] <- 'away'
###change class of Date
lad_pit_a$Date <- as.character(lad_pit_a$Date)
###select key vars
lad_pit_a <- lad_pit_a %>% select(Date, away, HHAa, RSAa)

###lad_pit_h for pitching variables (home)###
lad_pit_h <- lad_pit %>% select(Date, Opp, X, H, R)
lad_pit_h <- lad_pit_h %>% filter(X == '') #home games
###splitting lad_pit_h vector###
names(lad_pit_h)[names(lad_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(lad_pit_h)[names(lad_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
lad_pit_h$X <- ifelse(lad_pit_h$X =='', team, lad_pit_h$X) #use of team variable name here
names(lad_pit_h)[names(lad_pit_h) == 'X'] <- 'home'
###change class of Date
lad_pit_h$Date <- as.character(lad_pit_h$Date)
###filter down data
lad_pit_h <- lad_pit_h %>% select(Date, home, HHAh, RSAh)

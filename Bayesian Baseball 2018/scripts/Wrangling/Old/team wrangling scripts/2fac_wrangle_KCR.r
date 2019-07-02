library(tidyverse)
library(rethinking)

###import batting data manually: kcr_bat###
###import pitching data manually: kcr_pit###

team <- 'KCR' #team name variable

###kcr_homeaway for home/away###
kcr_homeaway <- kcr_bat %>% select(Date, Opp, X)
###create home variable
kcr_homeaway$X <- as.character(kcr_homeaway$X)
kcr_homeaway$Opp <- as.character(kcr_homeaway$Opp)
kcr_homeaway$X <- ifelse(kcr_homeaway$X =='@', kcr_homeaway$Opp, kcr_homeaway$X)
kcr_homeaway$X <- ifelse(kcr_homeaway$X =='', team, kcr_homeaway$X) #use of team variable name here
names(kcr_homeaway)[names(kcr_homeaway) == 'X'] <- 'home'
###create away variable
kcr_homeaway$Opp <- ifelse(kcr_homeaway$Opp == kcr_homeaway$home, team, kcr_homeaway$Opp) #use of team variable name here
names(kcr_homeaway)[names(kcr_homeaway) == 'Opp'] <- 'away'
###change class of Date
kcr_homeaway$Date <- as.character(kcr_homeaway$Date)
###select key vars
kcr_homeaway <- kcr_homeaway %>% select(home, away, Date) 

###kcr_bat_a for batting vars (away)###
kcr_bat_a <- kcr_bat %>% select(Date, Opp, Rslt, X, BA)
kcr_bat_a <- kcr_bat_a %>% filter(X == '@') #away games
###runs for away games###
kcr_bat_a$Rslt <- as.character(kcr_bat_a$Rslt) #character vector
TEMP<- strsplit(x=kcr_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
kcr_bat_a$Raway <- substring(TEMP$V1, 2); kcr_bat_a$Raway <- as.numeric(kcr_bat_a$Raway)
###BA for away games###
kcr_bat_a$BAa <- kcr_bat_a$BA
###create away variable###
kcr_bat_a$X <- ifelse(kcr_bat_a$X =='@', team, kcr_bat_a$X) #use of team variable name here
names(kcr_bat_a)[names(kcr_bat_a) == 'X'] <- 'away'
###change class of Date
kcr_bat_a$Date <- as.character(kcr_bat_a$Date)
###select key vars
kcr_bat_a <- kcr_bat_a %>% select(Date, away, Raway, BAa)

###kcr_bat_h for batting vars (home)###
kcr_bat_h <- kcr_bat %>% select(Date, Opp, Rslt, X, BA)
kcr_bat_h <- kcr_bat_h %>% filter(X == '') #home games
###runs for home games###
kcr_bat_h$Rslt <- as.character(kcr_bat_h$Rslt) #character vector
TEMP<- strsplit(x=kcr_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
kcr_bat_h$Rhome <- substring(TEMP$V1, 2); kcr_bat_h$Rhome <- as.numeric(kcr_bat_h$Rhome)
###BA for home games###
kcr_bat_h$BAh <- kcr_bat_h$BA
###create home variable###
kcr_bat_h$X <- ifelse(kcr_bat_h$X =='', team, kcr_bat_h$X) #use of team variable name here
names(kcr_bat_h)[names(kcr_bat_h) == 'X'] <- 'home'
###change class of Date
kcr_bat_h$Date <- as.character(kcr_bat_h$Date)
###select key vars
kcr_bat_h <- kcr_bat_h %>% select(Date, home, Rhome, BAh)

###kcr_pit_a for pitching vkcrables (away)###
kcr_pit_a <- kcr_pit %>% select(Date, Opp, X, H, R)
kcr_pit_a <- kcr_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(kcr_pit_a)[names(kcr_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(kcr_pit_a)[names(kcr_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
kcr_pit_a$X <- ifelse(kcr_pit_a$X =='@', team, kcr_pit_a$X) #use of team variable name here
names(kcr_pit_a)[names(kcr_pit_a) == 'X'] <- 'away'
###change class of Date
kcr_pit_a$Date <- as.character(kcr_pit_a$Date)
###select key vars
kcr_pit_a <- kcr_pit_a %>% select(Date, away, HHAa, RSAa)

###kcr_pit_h for pitching variables (home)###
kcr_pit_h <- kcr_pit %>% select(Date, Opp, X, H, R)
kcr_pit_h <- kcr_pit_h %>% filter(X == '') #home games
###splitting kcr_pit_h vector###
names(kcr_pit_h)[names(kcr_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(kcr_pit_h)[names(kcr_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
kcr_pit_h$X <- ifelse(kcr_pit_h$X =='', team, kcr_pit_h$X) #use of team variable name here
names(kcr_pit_h)[names(kcr_pit_h) == 'X'] <- 'home'
###change class of Date
kcr_pit_h$Date <- as.character(kcr_pit_h$Date)
###filter down data
kcr_pit_h <- kcr_pit_h %>% select(Date, home, HHAh, RSAh)

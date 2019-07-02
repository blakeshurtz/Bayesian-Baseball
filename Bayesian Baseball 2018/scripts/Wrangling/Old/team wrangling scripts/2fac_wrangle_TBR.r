library(tidyverse)
library(rethinking)

###import batting data manually: tbr_bat###
###import pitching data manually: tbr_pit###

team <- 'TBR' #team name variable

###tbr_homeaway for home/away###
tbr_homeaway <- tbr_bat %>% select(Date, Opp, X)
###create home variable
tbr_homeaway$X <- as.character(tbr_homeaway$X)
tbr_homeaway$Opp <- as.character(tbr_homeaway$Opp)
tbr_homeaway$X <- ifelse(tbr_homeaway$X =='@', tbr_homeaway$Opp, tbr_homeaway$X)
tbr_homeaway$X <- ifelse(tbr_homeaway$X =='', team, tbr_homeaway$X) #use of team variable name here
names(tbr_homeaway)[names(tbr_homeaway) == 'X'] <- 'home'
###create away variable
tbr_homeaway$Opp <- ifelse(tbr_homeaway$Opp == tbr_homeaway$home, team, tbr_homeaway$Opp) #use of team variable name here
names(tbr_homeaway)[names(tbr_homeaway) == 'Opp'] <- 'away'
###change class of Date
tbr_homeaway$Date <- as.character(tbr_homeaway$Date)
###select key vars
tbr_homeaway <- tbr_homeaway %>% select(home, away, Date) 

###tbr_bat_a for batting vars (away)###
tbr_bat_a <- tbr_bat %>% select(Date, Opp, Rslt, X, BA)
tbr_bat_a <- tbr_bat_a %>% filter(X == '@') #away games
###runs for away games###
tbr_bat_a$Rslt <- as.character(tbr_bat_a$Rslt) #character vector
TEMP<- strsplit(x=tbr_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
tbr_bat_a$Raway <- substring(TEMP$V1, 2); tbr_bat_a$Raway <- as.numeric(tbr_bat_a$Raway)
###BA for away games###
tbr_bat_a$BAa <- tbr_bat_a$BA
###create away variable###
tbr_bat_a$X <- ifelse(tbr_bat_a$X =='@', team, tbr_bat_a$X) #use of team variable name here
names(tbr_bat_a)[names(tbr_bat_a) == 'X'] <- 'away'
###change class of Date
tbr_bat_a$Date <- as.character(tbr_bat_a$Date)
###select key vars
tbr_bat_a <- tbr_bat_a %>% select(Date, away, Raway, BAa)

###tbr_bat_h for batting vars (home)###
tbr_bat_h <- tbr_bat %>% select(Date, Opp, Rslt, X, BA)
tbr_bat_h <- tbr_bat_h %>% filter(X == '') #home games
###runs for home games###
tbr_bat_h$Rslt <- as.character(tbr_bat_h$Rslt) #character vector
TEMP<- strsplit(x=tbr_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
tbr_bat_h$Rhome <- substring(TEMP$V1, 2); tbr_bat_h$Rhome <- as.numeric(tbr_bat_h$Rhome)
###BA for home games###
tbr_bat_h$BAh <- tbr_bat_h$BA
###create home variable###
tbr_bat_h$X <- ifelse(tbr_bat_h$X =='', team, tbr_bat_h$X) #use of team variable name here
names(tbr_bat_h)[names(tbr_bat_h) == 'X'] <- 'home'
###change class of Date
tbr_bat_h$Date <- as.character(tbr_bat_h$Date)
###select key vars
tbr_bat_h <- tbr_bat_h %>% select(Date, home, Rhome, BAh)

###tbr_pit_a for pitching vtbrables (away)###
tbr_pit_a <- tbr_pit %>% select(Date, Opp, X, H, R)
tbr_pit_a <- tbr_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(tbr_pit_a)[names(tbr_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(tbr_pit_a)[names(tbr_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
tbr_pit_a$X <- ifelse(tbr_pit_a$X =='@', team, tbr_pit_a$X) #use of team variable name here
names(tbr_pit_a)[names(tbr_pit_a) == 'X'] <- 'away'
###change class of Date
tbr_pit_a$Date <- as.character(tbr_pit_a$Date)
###select key vars
tbr_pit_a <- tbr_pit_a %>% select(Date, away, HHAa, RSAa)

###tbr_pit_h for pitching variables (home)###
tbr_pit_h <- tbr_pit %>% select(Date, Opp, X, H, R)
tbr_pit_h <- tbr_pit_h %>% filter(X == '') #home games
###splitting tbr_pit_h vector###
names(tbr_pit_h)[names(tbr_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(tbr_pit_h)[names(tbr_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
tbr_pit_h$X <- ifelse(tbr_pit_h$X =='', team, tbr_pit_h$X) #use of team variable name here
names(tbr_pit_h)[names(tbr_pit_h) == 'X'] <- 'home'
###change class of Date
tbr_pit_h$Date <- as.character(tbr_pit_h$Date)
###filter down data
tbr_pit_h <- tbr_pit_h %>% select(Date, home, HHAh, RSAh)

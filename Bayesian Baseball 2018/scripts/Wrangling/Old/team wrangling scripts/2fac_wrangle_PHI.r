library(tidyverse)
library(rethinking)

###import batting data manually: phi_bat###
###import pitching data manually: phi_pit###

team <- 'PHI' #team name variable

###phi_homeaway for home/away###
phi_homeaway <- phi_bat %>% select(Date, Opp, X)
###create home variable
phi_homeaway$X <- as.character(phi_homeaway$X)
phi_homeaway$Opp <- as.character(phi_homeaway$Opp)
phi_homeaway$X <- ifelse(phi_homeaway$X =='@', phi_homeaway$Opp, phi_homeaway$X)
phi_homeaway$X <- ifelse(phi_homeaway$X =='', team, phi_homeaway$X) #use of team variable name here
names(phi_homeaway)[names(phi_homeaway) == 'X'] <- 'home'
###create away variable
phi_homeaway$Opp <- ifelse(phi_homeaway$Opp == phi_homeaway$home, team, phi_homeaway$Opp) #use of team variable name here
names(phi_homeaway)[names(phi_homeaway) == 'Opp'] <- 'away'
###change class of Date
phi_homeaway$Date <- as.character(phi_homeaway$Date)
###select key vars
phi_homeaway <- phi_homeaway %>% select(home, away, Date) 

###phi_bat_a for batting vars (away)###
phi_bat_a <- phi_bat %>% select(Date, Opp, Rslt, X, BA)
phi_bat_a <- phi_bat_a %>% filter(X == '@') #away games
###runs for away games###
phi_bat_a$Rslt <- as.character(phi_bat_a$Rslt) #character vector
TEMP<- strsplit(x=phi_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
phi_bat_a$Raway <- substring(TEMP$V1, 2); phi_bat_a$Raway <- as.numeric(phi_bat_a$Raway)
###BA for away games###
phi_bat_a$BAa <- phi_bat_a$BA
###create away variable###
phi_bat_a$X <- ifelse(phi_bat_a$X =='@', team, phi_bat_a$X) #use of team variable name here
names(phi_bat_a)[names(phi_bat_a) == 'X'] <- 'away'
###change class of Date
phi_bat_a$Date <- as.character(phi_bat_a$Date)
###select key vars
phi_bat_a <- phi_bat_a %>% select(Date, away, Raway, BAa)

###phi_bat_h for batting vars (home)###
phi_bat_h <- phi_bat %>% select(Date, Opp, Rslt, X, BA)
phi_bat_h <- phi_bat_h %>% filter(X == '') #home games
###runs for home games###
phi_bat_h$Rslt <- as.character(phi_bat_h$Rslt) #character vector
TEMP<- strsplit(x=phi_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
phi_bat_h$Rhome <- substring(TEMP$V1, 2); phi_bat_h$Rhome <- as.numeric(phi_bat_h$Rhome)
###BA for home games###
phi_bat_h$BAh <- phi_bat_h$BA
###create home variable###
phi_bat_h$X <- ifelse(phi_bat_h$X =='', team, phi_bat_h$X) #use of team variable name here
names(phi_bat_h)[names(phi_bat_h) == 'X'] <- 'home'
###change class of Date
phi_bat_h$Date <- as.character(phi_bat_h$Date)
###select key vars
phi_bat_h <- phi_bat_h %>% select(Date, home, Rhome, BAh)

###phi_pit_a for pitching vphiables (away)###
phi_pit_a <- phi_pit %>% select(Date, Opp, X, H, R)
phi_pit_a <- phi_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(phi_pit_a)[names(phi_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(phi_pit_a)[names(phi_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
phi_pit_a$X <- ifelse(phi_pit_a$X =='@', team, phi_pit_a$X) #use of team variable name here
names(phi_pit_a)[names(phi_pit_a) == 'X'] <- 'away'
###change class of Date
phi_pit_a$Date <- as.character(phi_pit_a$Date)
###select key vars
phi_pit_a <- phi_pit_a %>% select(Date, away, HHAa, RSAa)

###phi_pit_h for pitching variables (home)###
phi_pit_h <- phi_pit %>% select(Date, Opp, X, H, R)
phi_pit_h <- phi_pit_h %>% filter(X == '') #home games
###splitting phi_pit_h vector###
names(phi_pit_h)[names(phi_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(phi_pit_h)[names(phi_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
phi_pit_h$X <- ifelse(phi_pit_h$X =='', team, phi_pit_h$X) #use of team variable name here
names(phi_pit_h)[names(phi_pit_h) == 'X'] <- 'home'
###change class of Date
phi_pit_h$Date <- as.character(phi_pit_h$Date)
###filter down data
phi_pit_h <- phi_pit_h %>% select(Date, home, HHAh, RSAh)

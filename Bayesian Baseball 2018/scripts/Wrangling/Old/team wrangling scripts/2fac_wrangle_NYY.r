library(tidyverse)
library(rethinking)

###import batting data manually: nyy_bat###
###import pitching data manually: nyy_pit###

team <- 'NYY' #team name variable

###nyy_homeaway for home/away###
nyy_homeaway <- nyy_bat %>% select(Date, Opp, X)
###create home variable
nyy_homeaway$X <- as.character(nyy_homeaway$X)
nyy_homeaway$Opp <- as.character(nyy_homeaway$Opp)
nyy_homeaway$X <- ifelse(nyy_homeaway$X =='@', nyy_homeaway$Opp, nyy_homeaway$X)
nyy_homeaway$X <- ifelse(nyy_homeaway$X =='', team, nyy_homeaway$X) #use of team variable name here
names(nyy_homeaway)[names(nyy_homeaway) == 'X'] <- 'home'
###create away variable
nyy_homeaway$Opp <- ifelse(nyy_homeaway$Opp == nyy_homeaway$home, team, nyy_homeaway$Opp) #use of team variable name here
names(nyy_homeaway)[names(nyy_homeaway) == 'Opp'] <- 'away'
###change class of Date
nyy_homeaway$Date <- as.character(nyy_homeaway$Date)
###select key vars
nyy_homeaway <- nyy_homeaway %>% select(home, away, Date) 

###nyy_bat_a for batting vars (away)###
nyy_bat_a <- nyy_bat %>% select(Date, Opp, Rslt, X, BA)
nyy_bat_a <- nyy_bat_a %>% filter(X == '@') #away games
###runs for away games###
nyy_bat_a$Rslt <- as.character(nyy_bat_a$Rslt) #character vector
TEMP<- strsplit(x=nyy_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
nyy_bat_a$Raway <- substring(TEMP$V1, 2); nyy_bat_a$Raway <- as.numeric(nyy_bat_a$Raway)
###BA for away games###
nyy_bat_a$BAa <- nyy_bat_a$BA
###create away variable###
nyy_bat_a$X <- ifelse(nyy_bat_a$X =='@', team, nyy_bat_a$X) #use of team variable name here
names(nyy_bat_a)[names(nyy_bat_a) == 'X'] <- 'away'
###change class of Date
nyy_bat_a$Date <- as.character(nyy_bat_a$Date)
###select key vars
nyy_bat_a <- nyy_bat_a %>% select(Date, away, Raway, BAa)

###nyy_bat_h for batting vars (home)###
nyy_bat_h <- nyy_bat %>% select(Date, Opp, Rslt, X, BA)
nyy_bat_h <- nyy_bat_h %>% filter(X == '') #home games
###runs for home games###
nyy_bat_h$Rslt <- as.character(nyy_bat_h$Rslt) #character vector
TEMP<- strsplit(x=nyy_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
nyy_bat_h$Rhome <- substring(TEMP$V1, 2); nyy_bat_h$Rhome <- as.numeric(nyy_bat_h$Rhome)
###BA for home games###
nyy_bat_h$BAh <- nyy_bat_h$BA
###create home variable###
nyy_bat_h$X <- ifelse(nyy_bat_h$X =='', team, nyy_bat_h$X) #use of team variable name here
names(nyy_bat_h)[names(nyy_bat_h) == 'X'] <- 'home'
###change class of Date
nyy_bat_h$Date <- as.character(nyy_bat_h$Date)
###select key vars
nyy_bat_h <- nyy_bat_h %>% select(Date, home, Rhome, BAh)

###nyy_pit_a for pitching vnyyables (away)###
nyy_pit_a <- nyy_pit %>% select(Date, Opp, X, H, R)
nyy_pit_a <- nyy_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(nyy_pit_a)[names(nyy_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(nyy_pit_a)[names(nyy_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
nyy_pit_a$X <- ifelse(nyy_pit_a$X =='@', team, nyy_pit_a$X) #use of team variable name here
names(nyy_pit_a)[names(nyy_pit_a) == 'X'] <- 'away'
###change class of Date
nyy_pit_a$Date <- as.character(nyy_pit_a$Date)
###select key vars
nyy_pit_a <- nyy_pit_a %>% select(Date, away, HHAa, RSAa)

###nyy_pit_h for pitching variables (home)###
nyy_pit_h <- nyy_pit %>% select(Date, Opp, X, H, R)
nyy_pit_h <- nyy_pit_h %>% filter(X == '') #home games
###splitting nyy_pit_h vector###
names(nyy_pit_h)[names(nyy_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(nyy_pit_h)[names(nyy_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
nyy_pit_h$X <- ifelse(nyy_pit_h$X =='', team, nyy_pit_h$X) #use of team variable name here
names(nyy_pit_h)[names(nyy_pit_h) == 'X'] <- 'home'
###change class of Date
nyy_pit_h$Date <- as.character(nyy_pit_h$Date)
###filter down data
nyy_pit_h <- nyy_pit_h %>% select(Date, home, HHAh, RSAh)

library(tidyverse)
library(rethinking)

###import batting data manually: col_bat###
###import pitching data manually: col_pit###

team <- 'COL' #team name variable

###col_homeaway for home/away###
col_homeaway <- col_bat %>% select(Date, Opp, X)
###create home variable
col_homeaway$X <- as.character(col_homeaway$X)
col_homeaway$Opp <- as.character(col_homeaway$Opp)
col_homeaway$X <- ifelse(col_homeaway$X =='@', col_homeaway$Opp, col_homeaway$X)
col_homeaway$X <- ifelse(col_homeaway$X =='', team, col_homeaway$X) #use of team variable name here
names(col_homeaway)[names(col_homeaway) == 'X'] <- 'home'
###create away variable
col_homeaway$Opp <- ifelse(col_homeaway$Opp == col_homeaway$home, team, col_homeaway$Opp) #use of team variable name here
names(col_homeaway)[names(col_homeaway) == 'Opp'] <- 'away'
###change class of Date
col_homeaway$Date <- as.character(col_homeaway$Date)
###select key vars
col_homeaway <- col_homeaway %>% select(home, away, Date) 

###col_bat_a for batting vars (away)###
col_bat_a <- col_bat %>% select(Date, Opp, Rslt, X, BA)
col_bat_a <- col_bat_a %>% filter(X == '@') #away games
###runs for away games###
col_bat_a$Rslt <- as.character(col_bat_a$Rslt) #character vector
TEMP<- strsplit(x=col_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
col_bat_a$Raway <- substring(TEMP$V1, 2); col_bat_a$Raway <- as.numeric(col_bat_a$Raway)
###BA for away games###
col_bat_a$BAa <- col_bat_a$BA
###create away variable###
col_bat_a$X <- ifelse(col_bat_a$X =='@', team, col_bat_a$X) #use of team variable name here
names(col_bat_a)[names(col_bat_a) == 'X'] <- 'away'
###change class of Date
col_bat_a$Date <- as.character(col_bat_a$Date)
###select key vars
col_bat_a <- col_bat_a %>% select(Date, away, Raway, BAa)

###col_bat_h for batting vars (home)###
col_bat_h <- col_bat %>% select(Date, Opp, Rslt, X, BA)
col_bat_h <- col_bat_h %>% filter(X == '') #home games
###runs for home games###
col_bat_h$Rslt <- as.character(col_bat_h$Rslt) #character vector
TEMP<- strsplit(x=col_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
col_bat_h$Rhome <- substring(TEMP$V1, 2); col_bat_h$Rhome <- as.numeric(col_bat_h$Rhome)
###BA for home games###
col_bat_h$BAh <- col_bat_h$BA
###create home variable###
col_bat_h$X <- ifelse(col_bat_h$X =='', team, col_bat_h$X) #use of team variable name here
names(col_bat_h)[names(col_bat_h) == 'X'] <- 'home'
###change class of Date
col_bat_h$Date <- as.character(col_bat_h$Date)
###select key vars
col_bat_h <- col_bat_h %>% select(Date, home, Rhome, BAh)

###col_pit_a for pitching variables (away)###
col_pit_a <- col_pit %>% select(Date, Opp, X, H, R)
col_pit_a <- col_pit_a %>% filter(X == '@') #away games
###splitting col_pit_a vector###
names(col_pit_a)[names(col_pit_a) == 'R'] <- 'RSAa'
names(col_pit_a)[names(col_pit_a) == 'H'] <- 'HHAa'
###create away variable###
col_pit_a$X <- ifelse(col_pit_a$X =='@', team, col_pit_a$X) #use of team variable name here
names(col_pit_a)[names(col_pit_a) == 'X'] <- 'away'
###change class of Date
col_pit_a$Date <- as.character(col_pit_a$Date)
###select key vars
col_pit_a <- col_pit_a %>% select(Date, away, HHAa, RSAa)

###col_pit_h for pitching variables (home)###
col_pit_h <- col_pit %>% select(Date, Opp, X, H, R)
col_pit_h <- col_pit_h %>% filter(X == '') #home games
###splitting col_pit_h vector###
names(col_pit_h)[names(col_pit_h) == 'R'] <- 'RSAh'
names(col_pit_h)[names(col_pit_h) == 'H'] <- 'HHAh'
###create home variable###
col_pit_h$X <- ifelse(col_pit_h$X =='', team, col_pit_h$X) #use of team variable name here
names(col_pit_h)[names(col_pit_h) == 'X'] <- 'home'
###change class of Date
col_pit_h$Date <- as.character(col_pit_h$Date)
###select key vars
col_pit_h <- col_pit_h %>% select(Date, home, HHAh, RSAh)

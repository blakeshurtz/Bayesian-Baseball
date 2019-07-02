library(tidyverse)
library(rethinking)

###import batting data manually: bos_bat###
###import pitching data manually: bos_pit###

team <- 'BOS' #team name variable

###bos_homeaway for home/away###
bos_homeaway <- bos_bat %>% select(Date, Opp, X)
###create home variable
bos_homeaway$X <- as.character(bos_homeaway$X)
bos_homeaway$Opp <- as.character(bos_homeaway$Opp)
bos_homeaway$X <- ifelse(bos_homeaway$X =='@', bos_homeaway$Opp, bos_homeaway$X)
bos_homeaway$X <- ifelse(bos_homeaway$X =='', team, bos_homeaway$X) #use of team variable name here
names(bos_homeaway)[names(bos_homeaway) == 'X'] <- 'home'
###create away variable
bos_homeaway$Opp <- ifelse(bos_homeaway$Opp == bos_homeaway$home, team, bos_homeaway$Opp) #use of team variable name here
names(bos_homeaway)[names(bos_homeaway) == 'Opp'] <- 'away'
###change class of Date
bos_homeaway$Date <- as.character(bos_homeaway$Date)
###select key vars
bos_homeaway <- bos_homeaway %>% select(home, away, Date) 

###bos_bat_a for batting vars (away)###
bos_bat_a <- bos_bat %>% select(Date, Opp, Rslt, X, BA)
bos_bat_a <- bos_bat_a %>% filter(X == '@') #away games
###runs for away games###
bos_bat_a$Rslt <- as.character(bos_bat_a$Rslt) #character vector
TEMP<- strsplit(x=bos_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
bos_bat_a$Raway <- substring(TEMP$V1, 2); bos_bat_a$Raway <- as.numeric(bos_bat_a$Raway)
###BA for away games###
bos_bat_a$BAa <- bos_bat_a$BA
###create away variable###
bos_bat_a$X <- ifelse(bos_bat_a$X =='@', team, bos_bat_a$X) #use of team variable name here
names(bos_bat_a)[names(bos_bat_a) == 'X'] <- 'away'
###change class of Date
bos_bat_a$Date <- as.character(bos_bat_a$Date)
###select key vars
bos_bat_a <- bos_bat_a %>% select(Date, away, Raway, BAa)

###bos_bat_h for batting vars (home)###
bos_bat_h <- bos_bat %>% select(Date, Opp, Rslt, X, BA)
bos_bat_h <- bos_bat_h %>% filter(X == '') #home games
###runs for home games###
bos_bat_h$Rslt <- as.character(bos_bat_h$Rslt) #character vector
TEMP<- strsplit(x=bos_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
bos_bat_h$Rhome <- substring(TEMP$V1, 2); bos_bat_h$Rhome <- as.numeric(bos_bat_h$Rhome)
###BA for home games###
bos_bat_h$BAh <- bos_bat_h$BA
###create home variable###
bos_bat_h$X <- ifelse(bos_bat_h$X =='', team, bos_bat_h$X) #use of team variable name here
names(bos_bat_h)[names(bos_bat_h) == 'X'] <- 'home'
###change class of Date
bos_bat_h$Date <- as.character(bos_bat_h$Date)
###select key vars
bos_bat_h <- bos_bat_h %>% select(Date, home, Rhome, BAh)

###bos_pit_a for pitching vbosables (away)###
bos_pit_a <- bos_pit %>% select(Date, Opp, X, H, R)
bos_pit_a <- bos_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(bos_pit_a)[names(bos_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(bos_pit_a)[names(bos_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
bos_pit_a$X <- ifelse(bos_pit_a$X =='@', team, bos_pit_a$X) #use of team variable name here
names(bos_pit_a)[names(bos_pit_a) == 'X'] <- 'away'
###change class of Date
bos_pit_a$Date <- as.character(bos_pit_a$Date)
###select key vars
bos_pit_a <- bos_pit_a %>% select(Date, away, HHAa, RSAa)

###bos_pit_h for pitching variables (home)###
bos_pit_h <- bos_pit %>% select(Date, Opp, X, H, R)
bos_pit_h <- bos_pit_h %>% filter(X == '') #home games
###splitting bos_pit_h vector###
names(bos_pit_h)[names(bos_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(bos_pit_h)[names(bos_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
bos_pit_h$X <- ifelse(bos_pit_h$X =='', team, bos_pit_h$X) #use of team variable name here
names(bos_pit_h)[names(bos_pit_h) == 'X'] <- 'home'
###change class of Date
bos_pit_h$Date <- as.character(bos_pit_h$Date)
###filter down data
bos_pit_h <- bos_pit_h %>% select(Date, home, HHAh, RSAh)

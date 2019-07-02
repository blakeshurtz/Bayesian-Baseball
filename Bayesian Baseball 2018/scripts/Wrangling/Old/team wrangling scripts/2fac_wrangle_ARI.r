library(tidyverse)
library(rethinking)

###set wd either data/batting logs or data/pitching logs###
###import batting data manually: ari_bat###
ari_bat <- read.csv("ari_bat.txt")
###import pitching data manually: ari_pit###

team <- 'ARI' #team name variable

###ari_homeaway for home/away###
ari_homeaway <- ari_bat %>% select(Date, Opp, X)
###create home variable
ari_homeaway$X <- as.character(ari_homeaway$X)
ari_homeaway$Opp <- as.character(ari_homeaway$Opp)
ari_homeaway$X <- ifelse(ari_homeaway$X =='@', ari_homeaway$Opp, ari_homeaway$X)
ari_homeaway$X <- ifelse(ari_homeaway$X =='', team, ari_homeaway$X) #use of team variable name here
names(ari_homeaway)[names(ari_homeaway) == 'X'] <- 'home'
###create away variable
ari_homeaway$Opp <- ifelse(ari_homeaway$Opp == ari_homeaway$home, team, ari_homeaway$Opp) #use of team variable name here
names(ari_homeaway)[names(ari_homeaway) == 'Opp'] <- 'away'
###change class of Date
ari_homeaway$Date <- as.character(ari_homeaway$Date)
###select key vars
ari_homeaway <- ari_homeaway %>% select(home, away, Date) 

###ari_bat_a for batting vars (away)###
ari_bat_a <- ari_bat %>% select(Date, Opp, Rslt, X, BA)
ari_bat_a <- ari_bat_a %>% filter(X == '@') #away games
###runs for away games###
ari_bat_a$Rslt <- as.character(ari_bat_a$Rslt) #character vector
TEMP<- strsplit(x=ari_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
ari_bat_a$Raway <- substring(TEMP$V1, 2); ari_bat_a$Raway <- as.numeric(ari_bat_a$Raway)
###BA for away games###
ari_bat_a$BAa <- ari_bat_a$BA
###create away variable###
ari_bat_a$X <- ifelse(ari_bat_a$X =='@', team, ari_bat_a$X) #use of team variable name here
names(ari_bat_a)[names(ari_bat_a) == 'X'] <- 'away'
###change class of Date
ari_bat_a$Date <- as.character(ari_bat_a$Date)
###select key vars
ari_bat_a <- ari_bat_a %>% select(Date, away, Raway, BAa)

###ari_bat_h for batting vars (home)###
ari_bat_h <- ari_bat %>% select(Date, Opp, Rslt, X, BA)
ari_bat_h <- ari_bat_h %>% filter(X == '') #home games
###runs for home games###
ari_bat_h$Rslt <- as.character(ari_bat_h$Rslt) #character vector
TEMP<- strsplit(x=ari_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
ari_bat_h$Rhome <- substring(TEMP$V1, 2); ari_bat_h$Rhome <- as.numeric(ari_bat_h$Rhome)
###BA for home games###
ari_bat_h$BAh <- ari_bat_h$BA
###create home variable###
ari_bat_h$X <- ifelse(ari_bat_h$X =='', team, ari_bat_h$X) #use of team variable name here
names(ari_bat_h)[names(ari_bat_h) == 'X'] <- 'home'
###change class of Date
ari_bat_h$Date <- as.character(ari_bat_h$Date)
###select key vars
ari_bat_h <- ari_bat_h %>% select(Date, home, Rhome, BAh)

###ari_pit_a for pitching variables (away)###
ari_pit_a <- ari_pit %>% select(Date, Opp, X, H, R)
ari_pit_a <- ari_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(ari_pit_a)[names(ari_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(ari_pit_a)[names(ari_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
ari_pit_a$X <- ifelse(ari_pit_a$X =='@', team, ari_pit_a$X) #use of team variable name here
names(ari_pit_a)[names(ari_pit_a) == 'X'] <- 'away'
###change class of Date
ari_pit_a$Date <- as.character(ari_pit_a$Date)
###select key vars
ari_pit_a <- ari_pit_a %>% select(Date, away, HHAa, RSAa)

###ari_pit_h for pitching variables (home)###
ari_pit_h <- ari_pit %>% select(Date, Opp, X, H, R)
ari_pit_h <- ari_pit_h %>% filter(X == '') #home games
###splitting ari_pit_h vector###
names(ari_pit_h)[names(ari_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(ari_pit_h)[names(ari_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
ari_pit_h$X <- ifelse(ari_pit_h$X =='', team, ari_pit_h$X) #use of team variable name here
names(ari_pit_h)[names(ari_pit_h) == 'X'] <- 'home'
###change class of Date
ari_pit_h$Date <- as.character(ari_pit_h$Date)
###filter down data
ari_pit_h <- ari_pit_h %>% select(Date, home, HHAh, RSAh)

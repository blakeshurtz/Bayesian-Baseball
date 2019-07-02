library(tidyverse)
library(rethinking)

###import batting data manually: tex_bat###
###import pitching data manually: tex_pit###

team <- 'TEX' #team name variable

###tex_homeaway for home/away###
tex_homeaway <- tex_bat %>% select(Date, Opp, X)
###create home variable
tex_homeaway$X <- as.character(tex_homeaway$X)
tex_homeaway$Opp <- as.character(tex_homeaway$Opp)
tex_homeaway$X <- ifelse(tex_homeaway$X =='@', tex_homeaway$Opp, tex_homeaway$X)
tex_homeaway$X <- ifelse(tex_homeaway$X =='', team, tex_homeaway$X) #use of team variable name here
names(tex_homeaway)[names(tex_homeaway) == 'X'] <- 'home'
###create away variable
tex_homeaway$Opp <- ifelse(tex_homeaway$Opp == tex_homeaway$home, team, tex_homeaway$Opp) #use of team variable name here
names(tex_homeaway)[names(tex_homeaway) == 'Opp'] <- 'away'
###change class of Date
tex_homeaway$Date <- as.character(tex_homeaway$Date)
###select key vars
tex_homeaway <- tex_homeaway %>% select(home, away, Date) 

###tex_bat_a for batting vars (away)###
tex_bat_a <- tex_bat %>% select(Date, Opp, Rslt, X, BA)
tex_bat_a <- tex_bat_a %>% filter(X == '@') #away games
###runs for away games###
tex_bat_a$Rslt <- as.character(tex_bat_a$Rslt) #character vector
TEMP<- strsplit(x=tex_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
tex_bat_a$Raway <- substring(TEMP$V1, 2); tex_bat_a$Raway <- as.numeric(tex_bat_a$Raway)
###BA for away games###
tex_bat_a$BAa <- tex_bat_a$BA
###create away variable###
tex_bat_a$X <- ifelse(tex_bat_a$X =='@', team, tex_bat_a$X) #use of team variable name here
names(tex_bat_a)[names(tex_bat_a) == 'X'] <- 'away'
###change class of Date
tex_bat_a$Date <- as.character(tex_bat_a$Date)
###select key vars
tex_bat_a <- tex_bat_a %>% select(Date, away, Raway, BAa)

###tex_bat_h for batting vars (home)###
tex_bat_h <- tex_bat %>% select(Date, Opp, Rslt, X, BA)
tex_bat_h <- tex_bat_h %>% filter(X == '') #home games
###runs for home games###
tex_bat_h$Rslt <- as.character(tex_bat_h$Rslt) #character vector
TEMP<- strsplit(x=tex_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
tex_bat_h$Rhome <- substring(TEMP$V1, 2); tex_bat_h$Rhome <- as.numeric(tex_bat_h$Rhome)
###BA for home games###
tex_bat_h$BAh <- tex_bat_h$BA
###create home variable###
tex_bat_h$X <- ifelse(tex_bat_h$X =='', team, tex_bat_h$X) #use of team variable name here
names(tex_bat_h)[names(tex_bat_h) == 'X'] <- 'home'
###change class of Date
tex_bat_h$Date <- as.character(tex_bat_h$Date)
###select key vars
tex_bat_h <- tex_bat_h %>% select(Date, home, Rhome, BAh)

###tex_pit_a for pitching vtexables (away)###
tex_pit_a <- tex_pit %>% select(Date, Opp, X, H, R)
tex_pit_a <- tex_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(tex_pit_a)[names(tex_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(tex_pit_a)[names(tex_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
###create away variable###
tex_pit_a$X <- ifelse(tex_pit_a$X =='@', team, tex_pit_a$X) #use of team variable name here
names(tex_pit_a)[names(tex_pit_a) == 'X'] <- 'away'
###change class of Date
tex_pit_a$Date <- as.character(tex_pit_a$Date)
###select key vars
tex_pit_a <- tex_pit_a %>% select(Date, away, HHAa, RSAa)

###tex_pit_h for pitching variables (home)###
tex_pit_h <- tex_pit %>% select(Date, Opp, X, H, R)
tex_pit_h <- tex_pit_h %>% filter(X == '') #home games
###splitting tex_pit_h vector###
names(tex_pit_h)[names(tex_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(tex_pit_h)[names(tex_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
###create home variable###
tex_pit_h$X <- ifelse(tex_pit_h$X =='', team, tex_pit_h$X) #use of team variable name here
names(tex_pit_h)[names(tex_pit_h) == 'X'] <- 'home'
###change class of Date
tex_pit_h$Date <- as.character(tex_pit_h$Date)
###filter down data
tex_pit_h <- tex_pit_h %>% select(Date, home, HHAh, RSAh)

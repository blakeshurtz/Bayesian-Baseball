library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
nyy_bat <- read.csv("nyy_bat.txt")

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
nyy_bat_a <- nyy_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
nyy_bat_a <- nyy_bat_a %>% filter(X == '@') #away games
###runs for away games###
nyy_bat_a$Rslt <- as.character(nyy_bat_a$Rslt) #character vector
TEMP<- strsplit(x=nyy_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
nyy_bat_a$Raway <- substring(TEMP$V1, 2); nyy_bat_a$Raway <- as.numeric(nyy_bat_a$Raway)
###Creating batting predictors for away games###
nyy_bat_a$BAa <- nyy_bat_a$BA
nyy_bat_a$PAa <- nyy_bat_a$PA
nyy_bat_a$ABa <- nyy_bat_a$AB
nyy_bat_a$Ra <- nyy_bat_a$R
nyy_bat_a$Ha <- nyy_bat_a$H
nyy_bat_a$B2Ba <- nyy_bat_a$X2B
nyy_bat_a$B3Ba <- nyy_bat_a$X3B
nyy_bat_a$BHRa <- nyy_bat_a$HR
nyy_bat_a$RBIa <- nyy_bat_a$RBI
nyy_bat_a$BBBa <- nyy_bat_a$BB
nyy_bat_a$BSOa <- nyy_bat_a$SO
nyy_bat_a$SHa <- nyy_bat_a$SH
nyy_bat_a$SFa <- nyy_bat_a$SF
nyy_bat_a$SBa <- nyy_bat_a$SB
###create away variable###
nyy_bat_a$X <- ifelse(nyy_bat_a$X =='@', team, nyy_bat_a$X) #use of team variable name here
names(nyy_bat_a)[names(nyy_bat_a) == 'X'] <- 'away'
###change class of Date
nyy_bat_a$Date <- as.character(nyy_bat_a$Date)
###select key vars
nyy_bat_a <- nyy_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###nyy_bat_h for batting vars (home)###
nyy_bat_h <- nyy_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
nyy_bat_h <- nyy_bat_h %>% filter(X == '') #home games
###runs for home games###
nyy_bat_h$Rslt <- as.character(nyy_bat_h$Rslt) #character vector
TEMP<- strsplit(x=nyy_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
nyy_bat_h$Rhome <- substring(TEMP$V1, 2); nyy_bat_h$Rhome <- as.numeric(nyy_bat_h$Rhome)
###Creating batting predictors for home games###
nyy_bat_h$BAh <- nyy_bat_h$BA
nyy_bat_h$PAh <- nyy_bat_h$PA
nyy_bat_h$ABh <- nyy_bat_h$AB
nyy_bat_h$Rh <- nyy_bat_h$R
nyy_bat_h$Hh <- nyy_bat_h$H
nyy_bat_h$B2Bh <- nyy_bat_h$X2B
nyy_bat_h$B3Bh <- nyy_bat_h$X3B
nyy_bat_h$BHRh <- nyy_bat_h$HR
nyy_bat_h$RBIh <- nyy_bat_h$RBI
nyy_bat_h$BBBh <- nyy_bat_h$BB
nyy_bat_h$BSOh <- nyy_bat_h$SO
nyy_bat_h$SHh <- nyy_bat_h$SH
nyy_bat_h$SFh <- nyy_bat_h$SF
nyy_bat_h$SBh <- nyy_bat_h$SB
###create home variable###
nyy_bat_h$X <- ifelse(nyy_bat_h$X =='', team, nyy_bat_h$X) #use of team variable name here
names(nyy_bat_h)[names(nyy_bat_h) == 'X'] <- 'home'
###change class of Date
nyy_bat_h$Date <- as.character(nyy_bat_h$Date)
###select key vars
nyy_bat_h <- nyy_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: nyy_pit###
nyy_pit <- read.csv("nyy_pit.txt")

###nyy_pit_a for pitching variables (away)###
nyy_pit_a <- nyy_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
nyy_pit_a <- nyy_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(nyy_pit_a)[names(nyy_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(nyy_pit_a)[names(nyy_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
nyy_pit_a$ERa <- nyy_pit_a$ER
nyy_pit_a$UERa <- nyy_pit_a$UER
nyy_pit_a$PBBa <- nyy_pit_a$BB
nyy_pit_a$PSOa <- nyy_pit_a$SO
nyy_pit_a$PHRa <- nyy_pit_a$HR
nyy_pit_a$BFa <- nyy_pit_a$BF
nyy_pit_a$Pita <- nyy_pit_a$Pit
nyy_pit_a$Stra <- nyy_pit_a$Str
nyy_pit_a$P2Ba <- nyy_pit_a$X2B
nyy_pit_a$P3Ba <- nyy_pit_a$X3B
###create away variable###
nyy_pit_a$X <- ifelse(nyy_pit_a$X =='@', team, nyy_pit_a$X) #use of team variable name here
names(nyy_pit_a)[names(nyy_pit_a) == 'X'] <- 'away'
###change class of Date
nyy_pit_a$Date <- as.character(nyy_pit_a$Date)
###select key vars
nyy_pit_a <- nyy_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###nyy_pit_h for pitching variables (home)###
nyy_pit_h <- nyy_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
nyy_pit_h <- nyy_pit_h %>% filter(X == '') #home games
###splitting nyy_pit_h vector###
names(nyy_pit_h)[names(nyy_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(nyy_pit_h)[names(nyy_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
nyy_pit_h$ERh <- nyy_pit_h$ER
nyy_pit_h$UERh <- nyy_pit_h$UER
nyy_pit_h$PBBh <- nyy_pit_h$BB
nyy_pit_h$PSOh <- nyy_pit_h$SO
nyy_pit_h$PHRh <- nyy_pit_h$HR
nyy_pit_h$BFh <- nyy_pit_h$BF
nyy_pit_h$Pith <- nyy_pit_h$Pit
nyy_pit_h$Strh <- nyy_pit_h$Str
nyy_pit_h$P2Bh <- nyy_pit_h$X2B
nyy_pit_h$P3Bh <- nyy_pit_h$X3B
###create home variable###
nyy_pit_h$X <- ifelse(nyy_pit_h$X =='', team, nyy_pit_h$X) #use of team variable name here
names(nyy_pit_h)[names(nyy_pit_h) == 'X'] <- 'home'
###change class of Date
nyy_pit_h$Date <- as.character(nyy_pit_h$Date)
###filter down data
nyy_pit_h <- nyy_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)

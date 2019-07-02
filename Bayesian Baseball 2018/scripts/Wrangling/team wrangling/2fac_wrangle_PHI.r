library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
phi_bat <- read.csv("phi_bat.txt")

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
phi_bat_a <- phi_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
phi_bat_a <- phi_bat_a %>% filter(X == '@') #away games
###runs for away games###
phi_bat_a$Rslt <- as.character(phi_bat_a$Rslt) #character vector
TEMP<- strsplit(x=phi_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
phi_bat_a$Raway <- substring(TEMP$V1, 2); phi_bat_a$Raway <- as.numeric(phi_bat_a$Raway)
###Creating batting predictors for away games###
phi_bat_a$BAa <- phi_bat_a$BA
phi_bat_a$PAa <- phi_bat_a$PA
phi_bat_a$ABa <- phi_bat_a$AB
phi_bat_a$Ra <- phi_bat_a$R
phi_bat_a$Ha <- phi_bat_a$H
phi_bat_a$B2Ba <- phi_bat_a$X2B
phi_bat_a$B3Ba <- phi_bat_a$X3B
phi_bat_a$BHRa <- phi_bat_a$HR
phi_bat_a$RBIa <- phi_bat_a$RBI
phi_bat_a$BBBa <- phi_bat_a$BB
phi_bat_a$BSOa <- phi_bat_a$SO
phi_bat_a$SHa <- phi_bat_a$SH
phi_bat_a$SFa <- phi_bat_a$SF
phi_bat_a$SBa <- phi_bat_a$SB
###create away variable###
phi_bat_a$X <- ifelse(phi_bat_a$X =='@', team, phi_bat_a$X) #use of team variable name here
names(phi_bat_a)[names(phi_bat_a) == 'X'] <- 'away'
###change class of Date
phi_bat_a$Date <- as.character(phi_bat_a$Date)
###select key vars
phi_bat_a <- phi_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###phi_bat_h for batting vars (home)###
phi_bat_h <- phi_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
phi_bat_h <- phi_bat_h %>% filter(X == '') #home games
###runs for home games###
phi_bat_h$Rslt <- as.character(phi_bat_h$Rslt) #character vector
TEMP<- strsplit(x=phi_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
phi_bat_h$Rhome <- substring(TEMP$V1, 2); phi_bat_h$Rhome <- as.numeric(phi_bat_h$Rhome)
###Creating batting predictors for home games###
phi_bat_h$BAh <- phi_bat_h$BA
phi_bat_h$PAh <- phi_bat_h$PA
phi_bat_h$ABh <- phi_bat_h$AB
phi_bat_h$Rh <- phi_bat_h$R
phi_bat_h$Hh <- phi_bat_h$H
phi_bat_h$B2Bh <- phi_bat_h$X2B
phi_bat_h$B3Bh <- phi_bat_h$X3B
phi_bat_h$BHRh <- phi_bat_h$HR
phi_bat_h$RBIh <- phi_bat_h$RBI
phi_bat_h$BBBh <- phi_bat_h$BB
phi_bat_h$BSOh <- phi_bat_h$SO
phi_bat_h$SHh <- phi_bat_h$SH
phi_bat_h$SFh <- phi_bat_h$SF
phi_bat_h$SBh <- phi_bat_h$SB
###create home variable###
phi_bat_h$X <- ifelse(phi_bat_h$X =='', team, phi_bat_h$X) #use of team variable name here
names(phi_bat_h)[names(phi_bat_h) == 'X'] <- 'home'
###change class of Date
phi_bat_h$Date <- as.character(phi_bat_h$Date)
###select key vars
phi_bat_h <- phi_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: phi_pit###
phi_pit <- read.csv("phi_pit.txt")

###phi_pit_a for pitching variables (away)###
phi_pit_a <- phi_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
phi_pit_a <- phi_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(phi_pit_a)[names(phi_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(phi_pit_a)[names(phi_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
phi_pit_a$ERa <- phi_pit_a$ER
phi_pit_a$UERa <- phi_pit_a$UER
phi_pit_a$PBBa <- phi_pit_a$BB
phi_pit_a$PSOa <- phi_pit_a$SO
phi_pit_a$PHRa <- phi_pit_a$HR
phi_pit_a$BFa <- phi_pit_a$BF
phi_pit_a$Pita <- phi_pit_a$Pit
phi_pit_a$Stra <- phi_pit_a$Str
phi_pit_a$P2Ba <- phi_pit_a$X2B
phi_pit_a$P3Ba <- phi_pit_a$X3B
###create away variable###
phi_pit_a$X <- ifelse(phi_pit_a$X =='@', team, phi_pit_a$X) #use of team variable name here
names(phi_pit_a)[names(phi_pit_a) == 'X'] <- 'away'
###change class of Date
phi_pit_a$Date <- as.character(phi_pit_a$Date)
###select key vars
phi_pit_a <- phi_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###phi_pit_h for pitching variables (home)###
phi_pit_h <- phi_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
phi_pit_h <- phi_pit_h %>% filter(X == '') #home games
###splitting phi_pit_h vector###
names(phi_pit_h)[names(phi_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(phi_pit_h)[names(phi_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
phi_pit_h$ERh <- phi_pit_h$ER
phi_pit_h$UERh <- phi_pit_h$UER
phi_pit_h$PBBh <- phi_pit_h$BB
phi_pit_h$PSOh <- phi_pit_h$SO
phi_pit_h$PHRh <- phi_pit_h$HR
phi_pit_h$BFh <- phi_pit_h$BF
phi_pit_h$Pith <- phi_pit_h$Pit
phi_pit_h$Strh <- phi_pit_h$Str
phi_pit_h$P2Bh <- phi_pit_h$X2B
phi_pit_h$P3Bh <- phi_pit_h$X3B
###create home variable###
phi_pit_h$X <- ifelse(phi_pit_h$X =='', team, phi_pit_h$X) #use of team variable name here
names(phi_pit_h)[names(phi_pit_h) == 'X'] <- 'home'
###change class of Date
phi_pit_h$Date <- as.character(phi_pit_h$Date)
###filter down data
phi_pit_h <- phi_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)

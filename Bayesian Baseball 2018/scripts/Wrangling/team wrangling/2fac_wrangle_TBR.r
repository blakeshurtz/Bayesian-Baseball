library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
tbr_bat <- read.csv("tbr_bat.txt")

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
tbr_bat_a <- tbr_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
tbr_bat_a <- tbr_bat_a %>% filter(X == '@') #away games
###runs for away games###
tbr_bat_a$Rslt <- as.character(tbr_bat_a$Rslt) #character vector
TEMP<- strsplit(x=tbr_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
tbr_bat_a$Raway <- substring(TEMP$V1, 2); tbr_bat_a$Raway <- as.numeric(tbr_bat_a$Raway)
###Creating batting predictors for away games###
tbr_bat_a$BAa <- tbr_bat_a$BA
tbr_bat_a$PAa <- tbr_bat_a$PA
tbr_bat_a$ABa <- tbr_bat_a$AB
tbr_bat_a$Ra <- tbr_bat_a$R
tbr_bat_a$Ha <- tbr_bat_a$H
tbr_bat_a$B2Ba <- tbr_bat_a$X2B
tbr_bat_a$B3Ba <- tbr_bat_a$X3B
tbr_bat_a$BHRa <- tbr_bat_a$HR
tbr_bat_a$RBIa <- tbr_bat_a$RBI
tbr_bat_a$BBBa <- tbr_bat_a$BB
tbr_bat_a$BSOa <- tbr_bat_a$SO
tbr_bat_a$SHa <- tbr_bat_a$SH
tbr_bat_a$SFa <- tbr_bat_a$SF
tbr_bat_a$SBa <- tbr_bat_a$SB
###create away variable###
tbr_bat_a$X <- ifelse(tbr_bat_a$X =='@', team, tbr_bat_a$X) #use of team variable name here
names(tbr_bat_a)[names(tbr_bat_a) == 'X'] <- 'away'
###change class of Date
tbr_bat_a$Date <- as.character(tbr_bat_a$Date)
###select key vars
tbr_bat_a <- tbr_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###tbr_bat_h for batting vars (home)###
tbr_bat_h <- tbr_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
tbr_bat_h <- tbr_bat_h %>% filter(X == '') #home games
###runs for home games###
tbr_bat_h$Rslt <- as.character(tbr_bat_h$Rslt) #character vector
TEMP<- strsplit(x=tbr_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
tbr_bat_h$Rhome <- substring(TEMP$V1, 2); tbr_bat_h$Rhome <- as.numeric(tbr_bat_h$Rhome)
###Creating batting predictors for home games###
tbr_bat_h$BAh <- tbr_bat_h$BA
tbr_bat_h$PAh <- tbr_bat_h$PA
tbr_bat_h$ABh <- tbr_bat_h$AB
tbr_bat_h$Rh <- tbr_bat_h$R
tbr_bat_h$Hh <- tbr_bat_h$H
tbr_bat_h$B2Bh <- tbr_bat_h$X2B
tbr_bat_h$B3Bh <- tbr_bat_h$X3B
tbr_bat_h$BHRh <- tbr_bat_h$HR
tbr_bat_h$RBIh <- tbr_bat_h$RBI
tbr_bat_h$BBBh <- tbr_bat_h$BB
tbr_bat_h$BSOh <- tbr_bat_h$SO
tbr_bat_h$SHh <- tbr_bat_h$SH
tbr_bat_h$SFh <- tbr_bat_h$SF
tbr_bat_h$SBh <- tbr_bat_h$SB
###create home variable###
tbr_bat_h$X <- ifelse(tbr_bat_h$X =='', team, tbr_bat_h$X) #use of team variable name here
names(tbr_bat_h)[names(tbr_bat_h) == 'X'] <- 'home'
###change class of Date
tbr_bat_h$Date <- as.character(tbr_bat_h$Date)
###select key vars
tbr_bat_h <- tbr_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: tbr_pit###
tbr_pit <- read.csv("tbr_pit.txt")

###tbr_pit_a for pitching variables (away)###
tbr_pit_a <- tbr_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
tbr_pit_a <- tbr_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(tbr_pit_a)[names(tbr_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(tbr_pit_a)[names(tbr_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
tbr_pit_a$ERa <- tbr_pit_a$ER
tbr_pit_a$UERa <- tbr_pit_a$UER
tbr_pit_a$PBBa <- tbr_pit_a$BB
tbr_pit_a$PSOa <- tbr_pit_a$SO
tbr_pit_a$PHRa <- tbr_pit_a$HR
tbr_pit_a$BFa <- tbr_pit_a$BF
tbr_pit_a$Pita <- tbr_pit_a$Pit
tbr_pit_a$Stra <- tbr_pit_a$Str
tbr_pit_a$P2Ba <- tbr_pit_a$X2B
tbr_pit_a$P3Ba <- tbr_pit_a$X3B
###create away variable###
tbr_pit_a$X <- ifelse(tbr_pit_a$X =='@', team, tbr_pit_a$X) #use of team variable name here
names(tbr_pit_a)[names(tbr_pit_a) == 'X'] <- 'away'
###change class of Date
tbr_pit_a$Date <- as.character(tbr_pit_a$Date)
###select key vars
tbr_pit_a <- tbr_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###tbr_pit_h for pitching variables (home)###
tbr_pit_h <- tbr_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
tbr_pit_h <- tbr_pit_h %>% filter(X == '') #home games
###splitting tbr_pit_h vector###
names(tbr_pit_h)[names(tbr_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(tbr_pit_h)[names(tbr_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
tbr_pit_h$ERh <- tbr_pit_h$ER
tbr_pit_h$UERh <- tbr_pit_h$UER
tbr_pit_h$PBBh <- tbr_pit_h$BB
tbr_pit_h$PSOh <- tbr_pit_h$SO
tbr_pit_h$PHRh <- tbr_pit_h$HR
tbr_pit_h$BFh <- tbr_pit_h$BF
tbr_pit_h$Pith <- tbr_pit_h$Pit
tbr_pit_h$Strh <- tbr_pit_h$Str
tbr_pit_h$P2Bh <- tbr_pit_h$X2B
tbr_pit_h$P3Bh <- tbr_pit_h$X3B
###create home variable###
tbr_pit_h$X <- ifelse(tbr_pit_h$X =='', team, tbr_pit_h$X) #use of team variable name here
names(tbr_pit_h)[names(tbr_pit_h) == 'X'] <- 'home'
###change class of Date
tbr_pit_h$Date <- as.character(tbr_pit_h$Date)
###filter down data
tbr_pit_h <- tbr_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)

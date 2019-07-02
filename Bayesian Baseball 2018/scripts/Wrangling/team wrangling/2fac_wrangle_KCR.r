library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
kcr_bat <- read.csv("kcr_bat.txt")

team <- 'KCR' #team name variable

###kcr_homeaway for home/away###
kcr_homeaway <- kcr_bat %>% select(Date, Opp, X)
###create home variable
kcr_homeaway$X <- as.character(kcr_homeaway$X)
kcr_homeaway$Opp <- as.character(kcr_homeaway$Opp)
kcr_homeaway$X <- ifelse(kcr_homeaway$X =='@', kcr_homeaway$Opp, kcr_homeaway$X)
kcr_homeaway$X <- ifelse(kcr_homeaway$X =='', team, kcr_homeaway$X) #use of team variable name here
names(kcr_homeaway)[names(kcr_homeaway) == 'X'] <- 'home'
###create away variable
kcr_homeaway$Opp <- ifelse(kcr_homeaway$Opp == kcr_homeaway$home, team, kcr_homeaway$Opp) #use of team variable name here
names(kcr_homeaway)[names(kcr_homeaway) == 'Opp'] <- 'away'
###change class of Date
kcr_homeaway$Date <- as.character(kcr_homeaway$Date)
###select key vars
kcr_homeaway <- kcr_homeaway %>% select(home, away, Date) 

###kcr_bat_a for batting vars (away)###
kcr_bat_a <- kcr_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
kcr_bat_a <- kcr_bat_a %>% filter(X == '@') #away games
###runs for away games###
kcr_bat_a$Rslt <- as.character(kcr_bat_a$Rslt) #character vector
TEMP<- strsplit(x=kcr_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
kcr_bat_a$Raway <- substring(TEMP$V1, 2); kcr_bat_a$Raway <- as.numeric(kcr_bat_a$Raway)
###Creating batting predictors for away games###
kcr_bat_a$BAa <- kcr_bat_a$BA
kcr_bat_a$PAa <- kcr_bat_a$PA
kcr_bat_a$ABa <- kcr_bat_a$AB
kcr_bat_a$Ra <- kcr_bat_a$R
kcr_bat_a$Ha <- kcr_bat_a$H
kcr_bat_a$B2Ba <- kcr_bat_a$X2B
kcr_bat_a$B3Ba <- kcr_bat_a$X3B
kcr_bat_a$BHRa <- kcr_bat_a$HR
kcr_bat_a$RBIa <- kcr_bat_a$RBI
kcr_bat_a$BBBa <- kcr_bat_a$BB
kcr_bat_a$BSOa <- kcr_bat_a$SO
kcr_bat_a$SHa <- kcr_bat_a$SH
kcr_bat_a$SFa <- kcr_bat_a$SF
kcr_bat_a$SBa <- kcr_bat_a$SB
###create away variable###
kcr_bat_a$X <- ifelse(kcr_bat_a$X =='@', team, kcr_bat_a$X) #use of team variable name here
names(kcr_bat_a)[names(kcr_bat_a) == 'X'] <- 'away'
###change class of Date
kcr_bat_a$Date <- as.character(kcr_bat_a$Date)
###select key vars
kcr_bat_a <- kcr_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###kcr_bat_h for batting vars (home)###
kcr_bat_h <- kcr_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
kcr_bat_h <- kcr_bat_h %>% filter(X == '') #home games
###runs for home games###
kcr_bat_h$Rslt <- as.character(kcr_bat_h$Rslt) #character vector
TEMP<- strsplit(x=kcr_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
kcr_bat_h$Rhome <- substring(TEMP$V1, 2); kcr_bat_h$Rhome <- as.numeric(kcr_bat_h$Rhome)
###Creating batting predictors for home games###
kcr_bat_h$BAh <- kcr_bat_h$BA
kcr_bat_h$PAh <- kcr_bat_h$PA
kcr_bat_h$ABh <- kcr_bat_h$AB
kcr_bat_h$Rh <- kcr_bat_h$R
kcr_bat_h$Hh <- kcr_bat_h$H
kcr_bat_h$B2Bh <- kcr_bat_h$X2B
kcr_bat_h$B3Bh <- kcr_bat_h$X3B
kcr_bat_h$BHRh <- kcr_bat_h$HR
kcr_bat_h$RBIh <- kcr_bat_h$RBI
kcr_bat_h$BBBh <- kcr_bat_h$BB
kcr_bat_h$BSOh <- kcr_bat_h$SO
kcr_bat_h$SHh <- kcr_bat_h$SH
kcr_bat_h$SFh <- kcr_bat_h$SF
kcr_bat_h$SBh <- kcr_bat_h$SB
###create home variable###
kcr_bat_h$X <- ifelse(kcr_bat_h$X =='', team, kcr_bat_h$X) #use of team variable name here
names(kcr_bat_h)[names(kcr_bat_h) == 'X'] <- 'home'
###change class of Date
kcr_bat_h$Date <- as.character(kcr_bat_h$Date)
###select key vars
kcr_bat_h <- kcr_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: kcr_pit###
kcr_pit <- read.csv("kcr_pit.txt")

###kcr_pit_a for pitching variables (away)###
kcr_pit_a <- kcr_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
kcr_pit_a <- kcr_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(kcr_pit_a)[names(kcr_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(kcr_pit_a)[names(kcr_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
kcr_pit_a$ERa <- kcr_pit_a$ER
kcr_pit_a$UERa <- kcr_pit_a$UER
kcr_pit_a$PBBa <- kcr_pit_a$BB
kcr_pit_a$PSOa <- kcr_pit_a$SO
kcr_pit_a$PHRa <- kcr_pit_a$HR
kcr_pit_a$BFa <- kcr_pit_a$BF
kcr_pit_a$Pita <- kcr_pit_a$Pit
kcr_pit_a$Stra <- kcr_pit_a$Str
kcr_pit_a$P2Ba <- kcr_pit_a$X2B
kcr_pit_a$P3Ba <- kcr_pit_a$X3B
###create away variable###
kcr_pit_a$X <- ifelse(kcr_pit_a$X =='@', team, kcr_pit_a$X) #use of team variable name here
names(kcr_pit_a)[names(kcr_pit_a) == 'X'] <- 'away'
###change class of Date
kcr_pit_a$Date <- as.character(kcr_pit_a$Date)
###select key vars
kcr_pit_a <- kcr_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###kcr_pit_h for pitching variables (home)###
kcr_pit_h <- kcr_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
kcr_pit_h <- kcr_pit_h %>% filter(X == '') #home games
###splitting kcr_pit_h vector###
names(kcr_pit_h)[names(kcr_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(kcr_pit_h)[names(kcr_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
kcr_pit_h$ERh <- kcr_pit_h$ER
kcr_pit_h$UERh <- kcr_pit_h$UER
kcr_pit_h$PBBh <- kcr_pit_h$BB
kcr_pit_h$PSOh <- kcr_pit_h$SO
kcr_pit_h$PHRh <- kcr_pit_h$HR
kcr_pit_h$BFh <- kcr_pit_h$BF
kcr_pit_h$Pith <- kcr_pit_h$Pit
kcr_pit_h$Strh <- kcr_pit_h$Str
kcr_pit_h$P2Bh <- kcr_pit_h$X2B
kcr_pit_h$P3Bh <- kcr_pit_h$X3B
###create home variable###
kcr_pit_h$X <- ifelse(kcr_pit_h$X =='', team, kcr_pit_h$X) #use of team variable name here
names(kcr_pit_h)[names(kcr_pit_h) == 'X'] <- 'home'
###change class of Date
kcr_pit_h$Date <- as.character(kcr_pit_h$Date)
###filter down data
kcr_pit_h <- kcr_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)

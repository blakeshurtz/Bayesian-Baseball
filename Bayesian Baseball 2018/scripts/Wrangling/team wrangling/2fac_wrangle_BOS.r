library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
bos_bat <- read.csv("bos_bat.txt")

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
bos_bat_a <- bos_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
bos_bat_a <- bos_bat_a %>% filter(X == '@') #away games
###runs for away games###
bos_bat_a$Rslt <- as.character(bos_bat_a$Rslt) #character vector
TEMP<- strsplit(x=bos_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
bos_bat_a$Raway <- substring(TEMP$V1, 2); bos_bat_a$Raway <- as.numeric(bos_bat_a$Raway)
###Creating batting predictors for away games###
bos_bat_a$BAa <- bos_bat_a$BA
bos_bat_a$PAa <- bos_bat_a$PA
bos_bat_a$ABa <- bos_bat_a$AB
bos_bat_a$Ra <- bos_bat_a$R
bos_bat_a$Ha <- bos_bat_a$H
bos_bat_a$B2Ba <- bos_bat_a$X2B
bos_bat_a$B3Ba <- bos_bat_a$X3B
bos_bat_a$BHRa <- bos_bat_a$HR
bos_bat_a$RBIa <- bos_bat_a$RBI
bos_bat_a$BBBa <- bos_bat_a$BB
bos_bat_a$BSOa <- bos_bat_a$SO
bos_bat_a$SHa <- bos_bat_a$SH
bos_bat_a$SFa <- bos_bat_a$SF
bos_bat_a$SBa <- bos_bat_a$SB
###create away variable###
bos_bat_a$X <- ifelse(bos_bat_a$X =='@', team, bos_bat_a$X) #use of team variable name here
names(bos_bat_a)[names(bos_bat_a) == 'X'] <- 'away'
###change class of Date
bos_bat_a$Date <- as.character(bos_bat_a$Date)
###select key vars
bos_bat_a <- bos_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###bos_bat_h for batting vars (home)###
bos_bat_h <- bos_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
bos_bat_h <- bos_bat_h %>% filter(X == '') #home games
###runs for home games###
bos_bat_h$Rslt <- as.character(bos_bat_h$Rslt) #character vector
TEMP<- strsplit(x=bos_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
bos_bat_h$Rhome <- substring(TEMP$V1, 2); bos_bat_h$Rhome <- as.numeric(bos_bat_h$Rhome)
###Creating batting predictors for home games###
bos_bat_h$BAh <- bos_bat_h$BA
bos_bat_h$PAh <- bos_bat_h$PA
bos_bat_h$ABh <- bos_bat_h$AB
bos_bat_h$Rh <- bos_bat_h$R
bos_bat_h$Hh <- bos_bat_h$H
bos_bat_h$B2Bh <- bos_bat_h$X2B
bos_bat_h$B3Bh <- bos_bat_h$X3B
bos_bat_h$BHRh <- bos_bat_h$HR
bos_bat_h$RBIh <- bos_bat_h$RBI
bos_bat_h$BBBh <- bos_bat_h$BB
bos_bat_h$BSOh <- bos_bat_h$SO
bos_bat_h$SHh <- bos_bat_h$SH
bos_bat_h$SFh <- bos_bat_h$SF
bos_bat_h$SBh <- bos_bat_h$SB
###create home variable###
bos_bat_h$X <- ifelse(bos_bat_h$X =='', team, bos_bat_h$X) #use of team variable name here
names(bos_bat_h)[names(bos_bat_h) == 'X'] <- 'home'
###change class of Date
bos_bat_h$Date <- as.character(bos_bat_h$Date)
###select key vars
bos_bat_h <- bos_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: bos_pit###
bos_pit <- read.csv("bos_pit.txt")

###bos_pit_a for pitching variables (away)###
bos_pit_a <- bos_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
bos_pit_a <- bos_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(bos_pit_a)[names(bos_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(bos_pit_a)[names(bos_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
bos_pit_a$ERa <- bos_pit_a$ER
bos_pit_a$UERa <- bos_pit_a$UER
bos_pit_a$PBBa <- bos_pit_a$BB
bos_pit_a$PSOa <- bos_pit_a$SO
bos_pit_a$PHRa <- bos_pit_a$HR
bos_pit_a$BFa <- bos_pit_a$BF
bos_pit_a$Pita <- bos_pit_a$Pit
bos_pit_a$Stra <- bos_pit_a$Str
bos_pit_a$P2Ba <- bos_pit_a$X2B
bos_pit_a$P3Ba <- bos_pit_a$X3B
###create away variable###
bos_pit_a$X <- ifelse(bos_pit_a$X =='@', team, bos_pit_a$X) #use of team variable name here
names(bos_pit_a)[names(bos_pit_a) == 'X'] <- 'away'
###change class of Date
bos_pit_a$Date <- as.character(bos_pit_a$Date)
###select key vars
bos_pit_a <- bos_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###bos_pit_h for pitching variables (home)###
bos_pit_h <- bos_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
bos_pit_h <- bos_pit_h %>% filter(X == '') #home games
###splitting bos_pit_h vector###
names(bos_pit_h)[names(bos_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(bos_pit_h)[names(bos_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
bos_pit_h$ERh <- bos_pit_h$ER
bos_pit_h$UERh <- bos_pit_h$UER
bos_pit_h$PBBh <- bos_pit_h$BB
bos_pit_h$PSOh <- bos_pit_h$SO
bos_pit_h$PHRh <- bos_pit_h$HR
bos_pit_h$BFh <- bos_pit_h$BF
bos_pit_h$Pith <- bos_pit_h$Pit
bos_pit_h$Strh <- bos_pit_h$Str
bos_pit_h$P2Bh <- bos_pit_h$X2B
bos_pit_h$P3Bh <- bos_pit_h$X3B
###create home variable###
bos_pit_h$X <- ifelse(bos_pit_h$X =='', team, bos_pit_h$X) #use of team variable name here
names(bos_pit_h)[names(bos_pit_h) == 'X'] <- 'home'
###change class of Date
bos_pit_h$Date <- as.character(bos_pit_h$Date)
###filter down data
bos_pit_h <- bos_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)

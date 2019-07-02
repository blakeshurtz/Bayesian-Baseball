library(tidyverse)
library(rethinking)

###set wd for data/battingpitchinglogs###
lad_bat <- read.csv("lad_bat.txt")

team <- 'LAD' #team name variable

###lad_homeaway for home/away###
lad_homeaway <- lad_bat %>% select(Date, Opp, X)
###create home variable
lad_homeaway$X <- as.character(lad_homeaway$X)
lad_homeaway$Opp <- as.character(lad_homeaway$Opp)
lad_homeaway$X <- ifelse(lad_homeaway$X =='@', lad_homeaway$Opp, lad_homeaway$X)
lad_homeaway$X <- ifelse(lad_homeaway$X =='', team, lad_homeaway$X) #use of team variable name here
names(lad_homeaway)[names(lad_homeaway) == 'X'] <- 'home'
###create away variable
lad_homeaway$Opp <- ifelse(lad_homeaway$Opp == lad_homeaway$home, team, lad_homeaway$Opp) #use of team variable name here
names(lad_homeaway)[names(lad_homeaway) == 'Opp'] <- 'away'
###change class of Date
lad_homeaway$Date <- as.character(lad_homeaway$Date)
###select key vars
lad_homeaway <- lad_homeaway %>% select(home, away, Date) 

###lad_bat_a for batting vars (away)###
lad_bat_a <- lad_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
lad_bat_a <- lad_bat_a %>% filter(X == '@') #away games
###runs for away games###
lad_bat_a$Rslt <- as.character(lad_bat_a$Rslt) #character vector
TEMP<- strsplit(x=lad_bat_a$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
lad_bat_a$Raway <- substring(TEMP$V1, 2); lad_bat_a$Raway <- as.numeric(lad_bat_a$Raway)
###Creating batting predictors for away games###
lad_bat_a$BAa <- lad_bat_a$BA
lad_bat_a$PAa <- lad_bat_a$PA
lad_bat_a$ABa <- lad_bat_a$AB
lad_bat_a$Ra <- lad_bat_a$R
lad_bat_a$Ha <- lad_bat_a$H
lad_bat_a$B2Ba <- lad_bat_a$X2B
lad_bat_a$B3Ba <- lad_bat_a$X3B
lad_bat_a$BHRa <- lad_bat_a$HR
lad_bat_a$RBIa <- lad_bat_a$RBI
lad_bat_a$BBBa <- lad_bat_a$BB
lad_bat_a$BSOa <- lad_bat_a$SO
lad_bat_a$SHa <- lad_bat_a$SH
lad_bat_a$SFa <- lad_bat_a$SF
lad_bat_a$SBa <- lad_bat_a$SB
###create away variable###
lad_bat_a$X <- ifelse(lad_bat_a$X =='@', team, lad_bat_a$X) #use of team variable name here
names(lad_bat_a)[names(lad_bat_a) == 'X'] <- 'away'
###change class of Date
lad_bat_a$Date <- as.character(lad_bat_a$Date)
###select key vars
lad_bat_a <- lad_bat_a %>% select(Date, away, Raway, BAa, PAa, ABa, Ra, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa)

###lad_bat_h for batting vars (home)###
lad_bat_h <- lad_bat %>% select(Date, Opp, Rslt, X, BA, PA, AB, R, H, X2B, X3B, HR, RBI, BB, SO, SH, SF, SB)
lad_bat_h <- lad_bat_h %>% filter(X == '') #home games
###runs for home games###
lad_bat_h$Rslt <- as.character(lad_bat_h$Rslt) #character vector
TEMP<- strsplit(x=lad_bat_h$Rslt, "-"); TEMP<- matrix(unlist(TEMP), ncol=2, byrow=TRUE); TEMP <- as.data.frame(TEMP)   
TEMP$V1 <- as.character(TEMP$V1)
lad_bat_h$Rhome <- substring(TEMP$V1, 2); lad_bat_h$Rhome <- as.numeric(lad_bat_h$Rhome)
###Creating batting predictors for home games###
lad_bat_h$BAh <- lad_bat_h$BA
lad_bat_h$PAh <- lad_bat_h$PA
lad_bat_h$ABh <- lad_bat_h$AB
lad_bat_h$Rh <- lad_bat_h$R
lad_bat_h$Hh <- lad_bat_h$H
lad_bat_h$B2Bh <- lad_bat_h$X2B
lad_bat_h$B3Bh <- lad_bat_h$X3B
lad_bat_h$BHRh <- lad_bat_h$HR
lad_bat_h$RBIh <- lad_bat_h$RBI
lad_bat_h$BBBh <- lad_bat_h$BB
lad_bat_h$BSOh <- lad_bat_h$SO
lad_bat_h$SHh <- lad_bat_h$SH
lad_bat_h$SFh <- lad_bat_h$SF
lad_bat_h$SBh <- lad_bat_h$SB
###create home variable###
lad_bat_h$X <- ifelse(lad_bat_h$X =='', team, lad_bat_h$X) #use of team variable name here
names(lad_bat_h)[names(lad_bat_h) == 'X'] <- 'home'
###change class of Date
lad_bat_h$Date <- as.character(lad_bat_h$Date)
###select key vars
lad_bat_h <- lad_bat_h %>% select(Date, home, Rhome, BAh, PAh, ABh, Rh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh)

###import pitching: lad_pit###
lad_pit <- read.csv("lad_pit.txt")

###lad_pit_a for pitching variables (away)###
lad_pit_a <- lad_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
lad_pit_a <- lad_pit_a %>% filter(X == '@') #away games
###selecting pitching variables###
names(lad_pit_a)[names(lad_pit_a) == 'R'] <- 'RSAa' #Runs Scored/Runs allowed (redundant!)
names(lad_pit_a)[names(lad_pit_a) == 'H'] <- 'HHAa' #Hits/Hits Allowed
lad_pit_a$ERa <- lad_pit_a$ER
lad_pit_a$UERa <- lad_pit_a$UER
lad_pit_a$PBBa <- lad_pit_a$BB
lad_pit_a$PSOa <- lad_pit_a$SO
lad_pit_a$PHRa <- lad_pit_a$HR
lad_pit_a$BFa <- lad_pit_a$BF
lad_pit_a$Pita <- lad_pit_a$Pit
lad_pit_a$Stra <- lad_pit_a$Str
lad_pit_a$P2Ba <- lad_pit_a$X2B
lad_pit_a$P3Ba <- lad_pit_a$X3B
###create away variable###
lad_pit_a$X <- ifelse(lad_pit_a$X =='@', team, lad_pit_a$X) #use of team variable name here
names(lad_pit_a)[names(lad_pit_a) == 'X'] <- 'away'
###change class of Date
lad_pit_a$Date <- as.character(lad_pit_a$Date)
###select key vars
lad_pit_a <- lad_pit_a %>% select(Date, away, HHAa, RSAa, ERa, UERa, PBBa, PSOa, PHRa, BFa, Pita, Stra, P2Ba, P3Ba)

###lad_pit_h for pitching variables (home)###
lad_pit_h <- lad_pit %>% select(Date, Opp, X, H, R, ER, UER, BB, SO, HR, BF, Pit, Str, X2B, X3B)
lad_pit_h <- lad_pit_h %>% filter(X == '') #home games
###splitting lad_pit_h vector###
names(lad_pit_h)[names(lad_pit_h) == 'R'] <- 'RSAh' #Runs Scored/Runs allowed (redundant!)
names(lad_pit_h)[names(lad_pit_h) == 'H'] <- 'HHAh' #Hits/Hits Allowed
lad_pit_h$ERh <- lad_pit_h$ER
lad_pit_h$UERh <- lad_pit_h$UER
lad_pit_h$PBBh <- lad_pit_h$BB
lad_pit_h$PSOh <- lad_pit_h$SO
lad_pit_h$PHRh <- lad_pit_h$HR
lad_pit_h$BFh <- lad_pit_h$BF
lad_pit_h$Pith <- lad_pit_h$Pit
lad_pit_h$Strh <- lad_pit_h$Str
lad_pit_h$P2Bh <- lad_pit_h$X2B
lad_pit_h$P3Bh <- lad_pit_h$X3B
###create home variable###
lad_pit_h$X <- ifelse(lad_pit_h$X =='', team, lad_pit_h$X) #use of team variable name here
names(lad_pit_h)[names(lad_pit_h) == 'X'] <- 'home'
###change class of Date
lad_pit_h$Date <- as.character(lad_pit_h$Date)
###filter down data
lad_pit_h <- lad_pit_h %>% select(Date, home, HHAh, RSAh, ERh, UERh, PBBh, PSOh, PHRh, BFh, Pith, Strh, P2Bh, P3Bh)

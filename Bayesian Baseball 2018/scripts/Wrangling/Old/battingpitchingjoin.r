###bind data###
library(tidyverse)
#doop<- bind_rows(mydata, mydata2)
#doop<- intersect(mydata, mydata2)

###Join all home/away datasets
mydata <- bind_rows(ari_homeaway, atl_homeaway, bal_homeaway, bos_homeaway, chc_homeaway, chw_homeaway, cin_homeaway, cle_homeaway, col_homeaway,
                det_homeaway, hou_homeaway, kcr_homeaway, laa_homeaway, lad_homeaway, mia_homeaway, mil_homeaway, min_homeaway, nym_homeaway,
                nyy_homeaway, oak_homeaway, phi_homeaway, pit_homeaway, sdp_homeaway, sea_homeaway, sfg_homeaway, stl_homeaway, tbr_homeaway,
                tex_homeaway, tor_homeaway, wsn_homeaway)

mydata <- unique( mydata )

###ARI
mydata <- left_join(mydata, ari_bat_a, by=c("Date", "away")) #add batting vars (away)
mydata <- left_join(mydata, ari_bat_h, by=c("Date", "home")) #add batting vars (home)
mydata <- left_join(mydata, ari_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata <- left_join(mydata, ari_pit_h, by=c("Date", "home")) #add pitching vars (home)

###ATL###
###atl_bat_a###
mydata <- left_join(mydata, atl_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###atl_bat_h###
mydata <- left_join(mydata, atl_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###atl_pit_h###
mydata <- left_join(mydata, atl_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###atl_pit_a###
mydata <- left_join(mydata, atl_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###BAL###
###bal_bat_a###
mydata <- left_join(mydata, bal_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###bal_bat_h###
mydata <- left_join(mydata, bal_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###bal_pit_h###
mydata <- left_join(mydata, bal_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###bal_pit_a###
mydata <- left_join(mydata, bal_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###BOS###
###bos_bat_a###
mydata <- left_join(mydata, bos_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###bos_bat_h###
mydata <- left_join(mydata, bos_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###bos_pit_h###
mydata <- left_join(mydata, bos_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###bos_pit_a###
mydata <- left_join(mydata, bos_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###CHC###
###chc_bat_a###
mydata <- left_join(mydata, chc_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###chc_bat_h###
mydata <- left_join(mydata, chc_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###chc_pit_h###
mydata <- left_join(mydata, chc_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###chc_pit_a###
mydata <- left_join(mydata, chc_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###CHW###
###chw_bat_a###
mydata <- left_join(mydata, chw_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###chw_bat_h###
mydata <- left_join(mydata, chw_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###chw_pit_h###
mydata <- left_join(mydata, chw_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###chw_pit_a###
mydata <- left_join(mydata, chw_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###CIN###
###cin_bat_a###
mydata <- left_join(mydata, cin_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###cin_bat_h###
mydata <- left_join(mydata, cin_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###cin_pit_h###
mydata <- left_join(mydata, cin_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###cin_pit_a###
mydata <- left_join(mydata, cin_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'

###CLE###
###cle_bat_a###
mydata <- left_join(mydata, cle_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###cle_bat_h###
mydata <- left_join(mydata, cle_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###cle_pit_h###
mydata <- left_join(mydata, cle_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###cle_pit_a###
mydata <- left_join(mydata, cle_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'

###COL###
###col_bat_a###
mydata <- left_join(mydata, col_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###col_bat_h###
mydata <- left_join(mydata, col_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###col_pit_h###
mydata <- left_join(mydata, col_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###col_pit_a###
mydata <- left_join(mydata, col_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'

###DET###
###det_bat_a###
mydata <- left_join(mydata, det_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###det_bat_h###
mydata <- left_join(mydata, det_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###det_pit_h###
mydata <- left_join(mydata, det_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###det_pit_a###
mydata <- left_join(mydata, det_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'

###HOU###
###hou_bat_a###
mydata <- left_join(mydata, hou_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###hou_bat_h###
mydata <- left_join(mydata, hou_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###hou_pit_h###
mydata <- left_join(mydata, hou_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###hou_pit_a###
mydata <- left_join(mydata, hou_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'

###KCR###
###kcr_bat_a###
mydata <- left_join(mydata, kcr_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###kcr_bat_h###
mydata <- left_join(mydata, kcr_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###kcr_pit_h###
mydata <- left_join(mydata, kcr_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###kcr_pit_a###
mydata <- left_join(mydata, kcr_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###LAA###
###laa_bat_a###
mydata <- left_join(mydata, laa_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###laa_bat_h###
mydata <- left_join(mydata, laa_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###laa_pit_h###
mydata <- left_join(mydata, laa_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###laa_pit_a###
mydata <- left_join(mydata, laa_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###LAD###
###lad_bat_a###
mydata <- left_join(mydata, lad_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###lad_bat_h###
mydata <- left_join(mydata, lad_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###lad_pit_h###
mydata <- left_join(mydata, lad_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###lad_pit_a###
mydata <- left_join(mydata, lad_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###MIA###
###mia_bat_a###
mydata <- left_join(mydata, mia_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###mia_bat_h###
mydata <- left_join(mydata, mia_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###mia_pit_h###
mydata <- left_join(mydata, mia_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###mia_pit_a###
mydata <- left_join(mydata, mia_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###MIL###
###mil_bat_a###
mydata <- left_join(mydata, mil_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###mil_bat_h###
mydata <- left_join(mydata, mil_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###mil_pit_h###
mydata <- left_join(mydata, mil_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###mil_pit_a###
mydata <- left_join(mydata, mil_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###MIN###
###min_bat_a###
mydata <- left_join(mydata, min_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###min_bat_h###
mydata <- left_join(mydata, min_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###min_pit_h###
mydata <- left_join(mydata, min_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###min_pit_a###
mydata <- left_join(mydata, min_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###NYM###
###nym_bat_a###
mydata <- left_join(mydata, nym_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###nym_bat_h###
mydata <- left_join(mydata, nym_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###nym_pit_h###
mydata <- left_join(mydata, nym_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###nym_pit_a###
mydata <- left_join(mydata, nym_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###NYY###
###nyy_bat_a###
mydata <- left_join(mydata, nyy_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###nyy_bat_h###
mydata <- left_join(mydata, nyy_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###nyy_pit_h###
mydata <- left_join(mydata, nyy_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###nyy_pit_a###
mydata <- left_join(mydata, nyy_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###OAK###
###oak_bat_a###
mydata <- left_join(mydata, oak_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###oak_bat_h###
mydata <- left_join(mydata, oak_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###oak_pit_h###
mydata <- left_join(mydata, oak_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###oak_pit_a###
mydata <- left_join(mydata, oak_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###PHI###
###phi_bat_a###
mydata <- left_join(mydata, phi_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###phi_bat_h###
mydata <- left_join(mydata, phi_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###phi_pit_h###
mydata <- left_join(mydata, phi_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###phi_pit_a###
mydata <- left_join(mydata, phi_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###PIT###
###pit_bat_a###
mydata <- left_join(mydata, pit_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###pit_bat_h###
mydata <- left_join(mydata, pit_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###pit_pit_h###
mydata <- left_join(mydata, pit_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###pit_pit_a###
mydata <- left_join(mydata, pit_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###SDP###
###sdp_bat_a###
mydata <- left_join(mydata, sdp_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###sdp_bat_h###
mydata <- left_join(mydata, sdp_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###sdp_pit_h###
mydata <- left_join(mydata, sdp_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###sdp_pit_a###
mydata <- left_join(mydata, sdp_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###SEA###
###sea_bat_a###
mydata <- left_join(mydata, sea_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###sea_bat_h###
mydata <- left_join(mydata, sea_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###sea_pit_h###
mydata <- left_join(mydata, sea_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###sea_pit_a###
mydata <- left_join(mydata, sea_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###SFG###
###sfg_bat_a###
mydata <- left_join(mydata, sfg_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###sfg_bat_h###
mydata <- left_join(mydata, sfg_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###sfg_pit_h###
mydata <- left_join(mydata, sfg_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###sfg_pit_a###
mydata <- left_join(mydata, sfg_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###STL###
###stl_bat_a###
mydata <- left_join(mydata, stl_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###stl_bat_h###
mydata <- left_join(mydata, stl_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###stl_pit_h###
mydata <- left_join(mydata, stl_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###stl_pit_a###
mydata <- left_join(mydata, stl_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###TBR###
###tbr_bat_a###
mydata <- left_join(mydata, tbr_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###tbr_bat_h###
mydata <- left_join(mydata, tbr_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###tbr_pit_h###
mydata <- left_join(mydata, tbr_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###tbr_pit_a###
mydata <- left_join(mydata, tbr_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###TEX###
###tex_bat_a###
mydata <- left_join(mydata, tex_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###tex_bat_h###
mydata <- left_join(mydata, tex_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###tex_pit_h###
mydata <- left_join(mydata, tex_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###tex_pit_a###
mydata <- left_join(mydata, tex_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###TOR###
###tor_bat_a###
mydata <- left_join(mydata, tor_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###tor_bat_h###
mydata <- left_join(mydata, tor_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###tor_pit_h###
mydata <- left_join(mydata, tor_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###tor_pit_a###
mydata <- left_join(mydata, tor_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
###WSN###
###wsn_bat_a###
mydata <- left_join(mydata, wsn_bat_a, by=c("Date", "away")) #add batting/away data
mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata <- mydata %>% select(Date, home, away, Raway.x, Rhome, BAa.x, BAh, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
###wsn_bat_h###
mydata <- left_join(mydata, wsn_bat_h, by=c("Date", "home")) #add batting/home data
mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata <- mydata %>% select(Date, home, away, Raway, Rhome.x, BAa, BAh.x, HHAa, HHAh, RSAa, RSAh)
names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
###wsn_pit_h###
mydata <- left_join(mydata, wsn_pit_h, by=c("Date", "home")) #add pitching vars (home)
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #HHA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa, HHAh.x, RSAh.x, RSAa)
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
###wsn_pit_a###
mydata <- left_join(mydata, wsn_pit_a, by=c("Date", "away")) #add pitching vars (away)
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #HHA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata <- mydata %>% select(home, away, Date, Raway, Rhome, BAa, BAh, HHAa.x, RSAa.x, HHAh, RSAh)
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
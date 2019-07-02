###SDP###
###sdp_bat_a###
mydata <- left_join(mydata, sdp_bat_a, by=c("Date", "away")) #add batting/away data

mydata$Raway.x <- ifelse(is.na(mydata$Raway.x) & !is.na(mydata$Raway.y), mydata$Raway.y, mydata$Raway.x) #runs (away)
mydata$BAa.x <- ifelse(is.na(mydata$BAa.x) & !is.na(mydata$BAa.y), mydata$BAa.y, mydata$BAa.x) #BA (away)
mydata$PAa.x <- ifelse(is.na(mydata$PAa.x) & !is.na(mydata$PAa.y), mydata$PAa.y, mydata$PAa.x) #BA (away)
mydata$ABa.x <- ifelse(is.na(mydata$ABa.x) & !is.na(mydata$ABa.y), mydata$ABa.y, mydata$ABa.x) #BA (away)
mydata$Ha.x <- ifelse(is.na(mydata$Ha.x) & !is.na(mydata$Ha.y), mydata$Ha.y, mydata$Ha.x) #BA (away)
mydata$B2Ba.x <- ifelse(is.na(mydata$B2Ba.x) & !is.na(mydata$B2Ba.y), mydata$B2Ba.y, mydata$B2Ba.x) #BA (away)
mydata$B3Ba.x <- ifelse(is.na(mydata$B3Ba.x) & !is.na(mydata$B3Ba.y), mydata$B3Ba.y, mydata$B3Ba.x) #BA (away)
mydata$BHRa.x <- ifelse(is.na(mydata$BHRa.x) & !is.na(mydata$BHRa.y), mydata$BHRa.y, mydata$BHRa.x) #BA (away)
mydata$RBIa.x <- ifelse(is.na(mydata$RBIa.x) & !is.na(mydata$RBIa.y), mydata$RBIa.y, mydata$RBIa.x) #BA (away)
mydata$BBBa.x <- ifelse(is.na(mydata$BBBa.x) & !is.na(mydata$BBBa.y), mydata$BBBa.y, mydata$BBBa.x) #BA (away)
mydata$BSOa.x <- ifelse(is.na(mydata$BSOa.x) & !is.na(mydata$BSOa.y), mydata$BSOa.y, mydata$BSOa.x) #BA (away)
mydata$SHa.x <- ifelse(is.na(mydata$SHa.x) & !is.na(mydata$SHa.y), mydata$SHa.y, mydata$SHa.x) #BA (away)
mydata$SFa.x <- ifelse(is.na(mydata$SFa.x) & !is.na(mydata$SFa.y), mydata$SFa.y, mydata$SFa.x) #BA (away)
mydata$SBa.x <- ifelse(is.na(mydata$SBa.x) & !is.na(mydata$SBa.y), mydata$SBa.y, mydata$SBa.x) #BA (away)

names(mydata)[names(mydata) == 'Raway.x'] <- 'Raway'
names(mydata)[names(mydata) == 'BAa.x'] <- 'BAa'
names(mydata)[names(mydata) == 'PAa.x'] <- 'PAa'
names(mydata)[names(mydata) == 'ABa.x'] <- 'ABa'
names(mydata)[names(mydata) == 'Ha.x'] <- 'Ha'
names(mydata)[names(mydata) == 'B2Ba.x'] <- 'B2Ba'
names(mydata)[names(mydata) == 'B3Ba.x'] <- 'B3Ba'
names(mydata)[names(mydata) == 'BHRa.x'] <- 'BHRa'
names(mydata)[names(mydata) == 'RBIa.x'] <- 'RBIa'
names(mydata)[names(mydata) == 'BBBa.x'] <- 'BBBa'
names(mydata)[names(mydata) == 'BSOa.x'] <- 'BSOa'
names(mydata)[names(mydata) == 'SHa.x'] <- 'SHa'
names(mydata)[names(mydata) == 'SFa.x'] <- 'SFa'
names(mydata)[names(mydata) == 'SBa.x'] <- 'SBa'

mydata <- mydata %>% select(Date, home, away, 
                            Raway, Rhome,
                            BAa, PAa, ABa, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa, #batting away
                            BAh, PAh, ABh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh, #batting home
                            P3Bh, P2Bh, PHRh, PSOh, PBBh, BFh, HHAh, RSAh, ABh, Hh, ERh, UERh, Pith, Strh, #pitching home
                            P3Ba, P2Ba, PHRa, PSOa, PBBa, BFa, HHAa, RSAa, ABa, Ha, ERa, UERa, Pita, Stra) #pitching away

###sdp_bat_h###
mydata <- left_join(mydata, sdp_bat_h, by=c("Date", "home")) #add batting/home data

mydata$Rhome.x <- ifelse(is.na(mydata$Rhome.x) & !is.na(mydata$Rhome.y), mydata$Rhome.y, mydata$Rhome.x) #runs (home)
mydata$BAh.x <- ifelse(is.na(mydata$BAh.x) & !is.na(mydata$BAh.y), mydata$BAh.y, mydata$BAh.x) #BA (home)
mydata$PAh.x <- ifelse(is.na(mydata$PAh.x) & !is.na(mydata$PAh.y), mydata$PAh.y, mydata$PAh.x) #BA (away)
mydata$ABh.x <- ifelse(is.na(mydata$ABh.x) & !is.na(mydata$ABh.y), mydata$ABh.y, mydata$ABh.x) #BA (away)
mydata$Hh.x <- ifelse(is.na(mydata$Hh.x) & !is.na(mydata$Hh.y), mydata$Hh.y, mydata$Hh.x) #BA (away)
mydata$B2Bh.x <- ifelse(is.na(mydata$B2Bh.x) & !is.na(mydata$B2Bh.y), mydata$B2Bh.y, mydata$B2Bh.x) #BA (away)
mydata$B3Bh.x <- ifelse(is.na(mydata$B3Bh.x) & !is.na(mydata$B3Bh.y), mydata$B3Bh.y, mydata$B3Bh.x) #BA (away)
mydata$BHRh.x <- ifelse(is.na(mydata$BHRh.x) & !is.na(mydata$BHRh.y), mydata$BHRh.y, mydata$BHRh.x) #BA (away)
mydata$RBIh.x <- ifelse(is.na(mydata$RBIh.x) & !is.na(mydata$RBIh.y), mydata$RBIh.y, mydata$RBIh.x) #BA (away)
mydata$BBBh.x <- ifelse(is.na(mydata$BBBh.x) & !is.na(mydata$BBBh.y), mydata$BBBh.y, mydata$BBBh.x) #BA (away)
mydata$BSOh.x <- ifelse(is.na(mydata$BSOh.x) & !is.na(mydata$BSOh.y), mydata$BSOh.y, mydata$BSOh.x) #BA (away)
mydata$SHh.x <- ifelse(is.na(mydata$SHh.x) & !is.na(mydata$SHh.y), mydata$SHh.y, mydata$SHh.x) #BA (away)
mydata$SFh.x <- ifelse(is.na(mydata$SFh.x) & !is.na(mydata$SFh.y), mydata$SFh.y, mydata$SFh.x) #BA (away)
mydata$SBh.x <- ifelse(is.na(mydata$SBh.x) & !is.na(mydata$SBh.y), mydata$SBh.y, mydata$SBh.x) #BA (away)

names(mydata)[names(mydata) == 'Rhome.x'] <- 'Rhome'
names(mydata)[names(mydata) == 'BAh.x'] <- 'BAh'
names(mydata)[names(mydata) == 'PAh.x'] <- 'PAh'
names(mydata)[names(mydata) == 'ABh.x'] <- 'ABh'
names(mydata)[names(mydata) == 'Hh.x'] <- 'Hh'
names(mydata)[names(mydata) == 'B2Bh.x'] <- 'B2Bh'
names(mydata)[names(mydata) == 'B3Bh.x'] <- 'B3Bh'
names(mydata)[names(mydata) == 'BHRh.x'] <- 'BHRh'
names(mydata)[names(mydata) == 'RBIh.x'] <- 'RBIh'
names(mydata)[names(mydata) == 'BBBh.x'] <- 'BBBh'
names(mydata)[names(mydata) == 'BSOh.x'] <- 'BSOh'
names(mydata)[names(mydata) == 'SHh.x'] <- 'SHh'
names(mydata)[names(mydata) == 'SFh.x'] <- 'SFh'
names(mydata)[names(mydata) == 'SBh.x'] <- 'SBh'

mydata <- mydata %>% select(Date, home, away, Rhome, Raway,  
                            BAa, PAa, ABa, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa, #batting away
                            BAh, PAh, ABh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh, #batting home
                            P3Bh, P2Bh, PHRh, PSOh, PBBh, BFh, HHAh, RSAh, ABh, Hh, ERh, UERh, Pith, Strh, #pitching home
                            P3Ba, P2Ba, PHRa, PSOa, PBBa, BFa, HHAa, RSAa, ABa, Ha, ERa, UERa, Pita, Stra) #pitching away


###sdp_pit_h###
mydata <- left_join(mydata, sdp_pit_h, by=c("Date", "home")) #add pitching vars (home)

mydata$P3Bh.x <- ifelse(is.na(mydata$P3Bh.x) & !is.na(mydata$P3Bh.y), mydata$P3Bh.y, mydata$P3Bh.x) #HHA
mydata$P2Bh.x <- ifelse(is.na(mydata$P2Bh.x) & !is.na(mydata$P2Bh.y), mydata$P2Bh.y, mydata$P2Bh.x) #HHA
mydata$PHRh.x <- ifelse(is.na(mydata$PHRh.x) & !is.na(mydata$PHRh.y), mydata$PHRh.y, mydata$PHRh.x) #HHA
mydata$PSOh.x <- ifelse(is.na(mydata$PSOh.x) & !is.na(mydata$PSOh.y), mydata$PSOh.y, mydata$PSOh.x) #HHA
mydata$PBBh.x <- ifelse(is.na(mydata$PBBh.x) & !is.na(mydata$PBBh.y), mydata$PBBh.y, mydata$PBBh.x) #HHA
mydata$BFh.x <- ifelse(is.na(mydata$BFh.x) & !is.na(mydata$BFh.y), mydata$BFh.y, mydata$BFh.x) #RSA
mydata$HHAh.x <- ifelse(is.na(mydata$HHAh.x) & !is.na(mydata$HHAh.y), mydata$HHAh.y, mydata$HHAh.x) #RSA
mydata$RSAh.x <- ifelse(is.na(mydata$RSAh.x) & !is.na(mydata$RSAh.y), mydata$RSAh.y, mydata$RSAh.x) #RSA
mydata$ERh.x <- ifelse(is.na(mydata$ERh.x) & !is.na(mydata$ERh.y), mydata$ERh.y, mydata$ERh.x) #RSA
mydata$UERh.x <- ifelse(is.na(mydata$UERh.x) & !is.na(mydata$UERh.y), mydata$UERh.y, mydata$UERh.x) #RSA
mydata$Pith.x <- ifelse(is.na(mydata$Pith.x) & !is.na(mydata$Pith.y), mydata$Pith.y, mydata$Pith.x) #RSA
mydata$Strh.x <- ifelse(is.na(mydata$Strh.x) & !is.na(mydata$Strh.y), mydata$Strh.y, mydata$Strh.x) #RSA

names(mydata)[names(mydata) == 'P3Bh.x'] <- 'P3Bh'
names(mydata)[names(mydata) == 'P2Bh.x'] <- 'P2Bh'
names(mydata)[names(mydata) == 'PHRh.x'] <- 'PHRh'
names(mydata)[names(mydata) == 'PSOh.x'] <- 'PSOh'
names(mydata)[names(mydata) == 'PBBh.x'] <- 'PBBh'
names(mydata)[names(mydata) == 'BFh.x'] <- 'BFh'
names(mydata)[names(mydata) == 'HHAh.x'] <- 'HHAh'
names(mydata)[names(mydata) == 'RSAh.x'] <- 'RSAh'
names(mydata)[names(mydata) == 'ERh.x'] <- 'ERh'
names(mydata)[names(mydata) == 'UERh.x'] <- 'UERh'
names(mydata)[names(mydata) == 'Pith.x'] <- 'Pith'
names(mydata)[names(mydata) == 'Strh.x'] <- 'Strh'

mydata <- mydata %>% select(Date, home, away, Rhome, Raway,  
                            BAa, PAa, ABa, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa, #batting away
                            BAh, PAh, ABh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh, #batting home
                            P3Bh, P2Bh, PHRh, PSOh, PBBh, BFh, HHAh, RSAh, ABh, Hh, ERh, UERh, Pith, Strh, #pitching home
                            P3Ba, P2Ba, PHRa, PSOa, PBBa, BFa, HHAa, RSAa, ABa, Ha, ERa, UERa, Pita, Stra) #pitching away

###sdp_pit_a###
mydata <- left_join(mydata, sdp_pit_a, by=c("Date", "away")) #add pitching vars (away)

mydata$P3Ba.x <- ifelse(is.na(mydata$P3Ba.x) & !is.na(mydata$P3Ba.y), mydata$P3Ba.y, mydata$P3Ba.x) #HHA
mydata$P2Ba.x <- ifelse(is.na(mydata$P2Ba.x) & !is.na(mydata$P2Ba.y), mydata$P2Ba.y, mydata$P2Ba.x) #HHA
mydata$PHRa.x <- ifelse(is.na(mydata$PHRa.x) & !is.na(mydata$PHRa.y), mydata$PHRa.y, mydata$PHRa.x) #HHA
mydata$PSOa.x <- ifelse(is.na(mydata$PSOa.x) & !is.na(mydata$PSOa.y), mydata$PSOa.y, mydata$PSOa.x) #HHA
mydata$PBBa.x <- ifelse(is.na(mydata$PBBa.x) & !is.na(mydata$PBBa.y), mydata$PBBa.y, mydata$PBBa.x) #HHA
mydata$BFa.x <- ifelse(is.na(mydata$BFa.x) & !is.na(mydata$BFa.y), mydata$BFa.y, mydata$BFa.x) #RSA
mydata$HHAa.x <- ifelse(is.na(mydata$HHAa.x) & !is.na(mydata$HHAa.y), mydata$HHAa.y, mydata$HHAa.x) #RSA
mydata$RSAa.x <- ifelse(is.na(mydata$RSAa.x) & !is.na(mydata$RSAa.y), mydata$RSAa.y, mydata$RSAa.x) #RSA
mydata$ERa.x <- ifelse(is.na(mydata$ERa.x) & !is.na(mydata$ERa.y), mydata$ERa.y, mydata$ERa.x) #RSA
mydata$UERa.x <- ifelse(is.na(mydata$UERa.x) & !is.na(mydata$UERa.y), mydata$UERa.y, mydata$UERa.x) #RSA
mydata$Pita.x <- ifelse(is.na(mydata$Pita.x) & !is.na(mydata$Pita.y), mydata$Pita.y, mydata$Pita.x) #RSA
mydata$Stra.x <- ifelse(is.na(mydata$Stra.x) & !is.na(mydata$Stra.y), mydata$Stra.y, mydata$Stra.x) #RSA

names(mydata)[names(mydata) == 'P3Ba.x'] <- 'P3Ba'
names(mydata)[names(mydata) == 'P2Ba.x'] <- 'P2Ba'
names(mydata)[names(mydata) == 'PHRa.x'] <- 'PHRa'
names(mydata)[names(mydata) == 'PSOa.x'] <- 'PSOa'
names(mydata)[names(mydata) == 'PBBa.x'] <- 'PBBa'
names(mydata)[names(mydata) == 'BFa.x'] <- 'BFa'
names(mydata)[names(mydata) == 'HHAa.x'] <- 'HHAa'
names(mydata)[names(mydata) == 'RSAa.x'] <- 'RSAa'
names(mydata)[names(mydata) == 'ERa.x'] <- 'ERa'
names(mydata)[names(mydata) == 'UERa.x'] <- 'UERa'
names(mydata)[names(mydata) == 'Pita.x'] <- 'Pita'
names(mydata)[names(mydata) == 'Stra.x'] <- 'Stra'

mydata <- mydata %>% select(Date, home, away, Rhome, Raway,  
                            BAa, PAa, ABa, Ha, B2Ba, B3Ba, BHRa, RBIa, BBBa, BSOa, SHa, SFa, SBa, #batting away
                            BAh, PAh, ABh, Hh, B2Bh, B3Bh, BHRh, RBIh, BBBh, BSOh, SHh, SFh, SBh, #batting home
                            P3Bh, P2Bh, PHRh, PSOh, PBBh, BFh, HHAh, RSAh, ABh, Hh, ERh, UERh, Pith, Strh, #pitching home
                            P3Ba, P2Ba, PHRa, PSOa, PBBa, BFa, HHAa, RSAa, ABa, Ha, ERa, UERa, Pita, Stra) #pitching away

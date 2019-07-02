###use this script to being exploratory analysis
library(skimr)
library(tidyverse)
library(RMySQL)

###read in data as guest
con <- dbConnect(MySQL(),
                 user = 'guest',
                 password = 'password',
                 host = 'mydbinstance4.c1uducbod6js.us-west-1.rds.amazonaws.com',
                 dbname='blakeobeans')

###standings
standings <- dbReadTable(conn = con, name = 'standings', value = standings, overwrite = TRUE)
standings <- standings %>% select(Tm, Rdiff) #team ID and run differential (runs scored - runs allowed)

###teambatting
teambatting <- dbReadTable(conn = con, name = 'teambatting', value = teambatting, overwrite = TRUE)
teambatting <- teambatting %>% select(Tm, R) #selecting teams, and number of runs
names(teambatting) <- c("Tm", "rb") #runs to "runsbatting"

###fielding
fielding <- dbReadTable(conn = con, name = 'fielding', value = fielding, overwrite = TRUE)
fielding <- fielding %>% select(Tm, DefEff) #selecting teams, and "defensive efficiency"

###pitching
pitching <- dbReadTable(conn = con, name = 'pitching', value = pitching, overwrite = TRUE)
pitching <- pitching %>% select(Tm, R) #selecting teams, and number of runs scored / allowed
names(pitching) <- c("Tm", "rp") # runs to "runs pitching"

###management
#management <- dbReadTable(conn = con, name = 'management', value = management, overwrite = TRUE)
#management <- management %>% select(Tm, W.L.) #selecting teams, and number of runs scored / allowed

###joining data
mydata <- left_join(standings, teambatting, by='Tm')
mydata <- left_join(mydata, fielding, by='Tm')
mydata <- left_join(mydata, pitching, by='Tm')
#mydata <- left_join(mydata, management, by='Tm')
head(mydata)

#upload mydata
con <- dbConnect(MySQL(),
                 user = 'blakeobeans',
                 password = 'MrJ0nes123',
                 host = 'mydbinstance4.c1uducbod6js.us-west-1.rds.amazonaws.com',
                 dbname='blakeobeans')

dbWriteTable(conn = con, name = 'mydata', value = mydata, overwrite = TRUE) 

#save data to server
setwd("D:/Google Drive/Life/Statistics/Bayesian Baseball II/data")
write.csv(mydata, "mydata.csv")

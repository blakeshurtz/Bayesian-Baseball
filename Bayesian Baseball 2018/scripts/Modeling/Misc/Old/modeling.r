library(RMariaDB)

###read in data as guest
con <- dbConnect(MySQL(),
                 user = 'guest',
                 password = 'password',
                 host = 'mydbinstance4.c1uducbod6js.us-west-1.rds.amazonaws.com',
                 dbname='blakeobeans')


###basic regression, obviously the data overfits
mydata <- dbReadTable(conn = con, name = 'mydata', value = mydata, overwrite = TRUE) 
m1 <- lm(Rdiff ~ rb + DefEff + rp, data=mydata)
summary(m1)

###game-level regression, a negative beta coefficient is means a decrease in the difference
###this won't work, 4 level model
mydata3 <- dbReadTable(conn = con, name = 'teamschedule', value = mydata3, overwrite = TRUE) 
m2 <- lm(diff~ home + away, data=mydata3); summary(m2)

###team-level regression for each individual team
ari <- dbReadTable(conn = con, name = 'ari', value = ari, overwrite = TRUE) 
###widen data for regression- will need to do this upstream

m3 <- lm(rdiff~ Opp, data=ari); summary(m3)

###modeling in rethinking
library(rethinking)
ari <- dbReadTable(conn = con, name = 'ari', value = ari, overwrite = TRUE) 
ari$Opp <- coerce_index(ari$Opp) 
m4 <- map(
  alist(
    rdiff ~ dnorm(mu, sigma) , 
    mu <- a + b * Opp , 
    a ~ dnorm(0, 1) ,
    b ~ dnorm(0, 5) ,
    sigma ~ dexp(1)
  ) , data = ari)
precis(m4)

###modeling in stan
library(rethinking)
ari <- dbReadTable(conn = con, name = 'ari', value = ari, overwrite = TRUE) 
ari$Opp <- coerce_index(ari$Opp) 
m5 <- map2stan(
  alist(
    rdiff ~ dnorm(mu, sigma) , 
    mu <- a + b * Opp , 
    a ~ dnorm(0, 1) ,
    b ~ dnorm(0, 5) ,
    sigma ~ dexp(1)
  ) , data = ari)
precis(m5)

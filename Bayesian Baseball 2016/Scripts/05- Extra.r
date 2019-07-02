#Calculate the "Bayesian" R^2
library(rstanarm)
bayes_R2 <- function(fit) {
  y <- get_y(fit)
  ypred <- posterior_linpred(fit)
  e <- -1 * sweep(ypred, 2, y)
  var_ypred <- apply(ypred, 1, var)
  var_e <- apply(e, 1, var)
  var_ypred / (var_ypred + var_e)
}
#rstanarm model
M1 <- stan_lmer(score ~ wl_norm + pitera_norm + (1 | opp_team), data=data)
print(median(bayes_R2(M1)))

#Ratio of Variances (using lme4 package)
library(lme4)
fit1 <- lmer(score ~ wl_norm + pitera_norm + (1 |opp_team), data = data, REML = FALSE)
summary(fit1)
.256/17.23

#Shiny App
library(rstan)
library(shinystan)
library(rsconnect)
linkagecode <- stancode(model2)
set.seed(1234)
model_obj <- stan_model(model_code = linkagecode, model_name = "BayesianBaseball")
dat <- list()
dat$score <- standata$score
dat$opp_team <- standata$opp_team
dat$wl_norm <- standata$wl_norm
dat$pitera_norm <- standata$pitera_norm
dat$N <- 161
dat$N_opp_team <- 20
fit <- sampling(model_obj, data = dat, iter=6000, warmup=3000, chains = 4, pars="mu", include=FALSE)
print(fit)
shinyfit<-as.shinystan(fit)
deploy_shinystan(shinyfit, "BayesianBaseball")


R <- rlkjcorr(1e4, K=2, eta=4)
#prior
prior <- as.data.frame(R[,1,2]); 
prior$prepost = rep("pre", nrow(prior))
colnames(prior) <- c("value", "prepost")
#post
cov <- melt(post$Rho_home[,1,2]); 
cov$prepost = rep("post", nrow(cov)); 
#
TEMP <- as.data.frame(rbind(prior, cov))
head(TEMP)
#
ggplot(TEMP) + 
  geom_density(aes(x = value, group=prepost, col=prepost), kernel="gaussian") 

ggplot(posterior.distributions) +
  geom_freqpoly(aes(x=V1, y=..density.., alpha=0.6), col='#c6dbef', size=1.2) +
  geom_freqpoly(aes(x=V2, y=..density.., alpha=0.7), col='#9ecae1', size=1.2) + #following BOS first win
  geom_freqpoly(aes(x=V3, y=..density.., alpha=0.8), col='#6baed6', size=1.2) +
  geom_freqpoly(aes(x=V4, y=..density.., alpha=0.9), col='#3182bd', size=1.2) +
  geom_freqpoly(aes(x=V5, y=..density.., alpha=1), col='#08519c', size=1.2) +
  theme_minimal() + guides(fill=FALSE) + 
  labs(title="World Series 2018: Posterior Predictive Simulation Games 1-5") +
  xlab("Difference in Points (LAD - BOS)") +
  scale_alpha_continuous(labels = c("Game 1", "Game 2", "Game 3", "Game 4", "Game 5")) +
  guides(alpha=guide_legend("")) +
  theme(plot.title = element_text(size=48), plot.subtitle = element_text(size=24),
        axis.text.x = element_text(size=36), axis.title.x = element_text(size=36),
        axis.text.y = element_text(size=36), axis.title.y = element_text(size=36),
        legend.text=element_text(size=36), legend.title = element_text(size=36), legend.position="right")

ggplot(parametric) + 
  geom_density(aes(x = Value, group=Category, col=Category), kernel="gaussian") +
  facet_wrap(vars(Group), scales="free") +
  theme_minimal() +
  labs(title="Game 5: Parameterized Posterior Predictive Simulations") +
  xlab("Difference in Points") +
  theme(legend.position="bottom") +
  scale_color_manual( values = c("#a50f15","#08519c")) +
  theme(plot.title = element_text(size=48), 
        plot.subtitle = element_text(size=24),
        axis.text.x = element_text(size=36), axis.title.x = element_text(size=36),
        axis.text.y = element_text(size=36), axis.title.y = element_text(size=36, angle =-0),
        legend.text=element_text(size=36), legend.title = element_text(size=30), legend.position="right",
        strip.text = element_text(size = 36)) 

ggplot(TEMP, aes(inning, surviv)) +
  geom_step(direction="hv", col='#08519c', size=1.2) +
  theme_minimal() +
  labs(title="Game 5: Play-by-Play Probability Updates") +
  xlab("Inning") + ylab('Probably of \n LAD Win') + scale_x_continuous(breaks=0:9) +
  theme(plot.title = element_text(size=48), 
        plot.subtitle = element_text(size=24),
        axis.text.x = element_text(size=36), axis.title.x = element_text(size=36),
        axis.text.y = element_text(size=36), axis.title.y = element_text(size=36, angle =-0),
        legend.text=element_text(size=36)) +
  annotate("text", x = 8.5, y = .6, size=14,
           label = "BOS scores 1 run in the \n first half of each of the \n 6th, 7th, and 8th innings") +
  geom_segment(aes(x = 9, y = .5, xend = 6.5, yend = .29), col="#a50f15", arrow = arrow(length = unit(0.03, "npc"))) +
  geom_segment(aes(x = 9, y = .5, xend = 7.5, yend = .04), col="#a50f15", arrow = arrow(length = unit(0.03, "npc"))) +
  geom_segment(aes(x = 9, y = .5, xend = 8.5, yend = .01), col="#a50f15", arrow = arrow(length = unit(0.03, "npc"))) 

ggplot(pred2, aes(y=as.factor(Game), pch=Model)) +
  geom_point(aes(x=Mean), size=3) +
  scale_shape_manual(values=c(4, 1, 3))+
  geom_segment((aes(xend=Mean, yend=Game, x=1.96*LB, col=Game, alpha=Game)), size=1.2) +
  geom_segment((aes(xend=Mean, yend=Game, x=1.96*UB, col = Game, alpha=Game)), size=1.2) + 
  geom_point(aes(x=Mean), size=3) +
  scale_shape_manual(values=c(4, 1, 3))+
  theme_minimal() +
  labs(title="Partial Pooling with Multi-level Model") +
  xlab("Difference in Points (LAD - BOS)") + ylab("World \n Series \n Game") + xlim(-20, 20) + 
  guides(alpha=FALSE) + guides(col=FALSE) +
  theme(plot.title = element_text(size=48), 
        plot.subtitle = element_text(size=24),
        axis.text.x = element_text(size=36), axis.title.x = element_text(size=36),
        axis.text.y = element_text(size=36), axis.title.y = element_text(size=36, angle =-0),
        legend.text=element_text(size=30), legend.title = element_text(size=30), legend.position="top",
        strip.text = element_text(size = 36)) 


###Random simulation, just for fun

```{r}
hometeam = "ARI"
awayteam = "COL"
home <- mydata %>% filter(home == hometeam) %>% summarise_all(funs(mean))
away <- mydata %>% filter(home == awayteam) %>% summarise_all(funs(mean))
```
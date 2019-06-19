library(dplyr)
library(base)
library(stats)

matches1 <- read.csv("matches.csv")
deliveries <- read.csv("deliveries.csv")

df <- select(deliveries,batsman,match_id,batsman_runs)
df1 <- select(matches1,id,season)
ipl <- merge(df,df1, by.x = "match_id", by.y = "id")
table(ipl)

df2 <- select(matches1,id,season)
df3 <- select(deliveries,bowler,match_id,player_dismissed) %>% filter(player_dismissed!="")
ipl5 <- merge(df2,df3,by.x = "id",by.y = "match_id")
table(ipl5)

df4 <- select(matches1,id,season)
df5 <- select(deliveries,over,batsman_runs,match_id)
ipl2 <- merge(df5,df4,by.x = "match_id",by.y = "id")
table(ipl2)

choiceBatsman = unique(deliveries$batsman)
choiceBowler = unique(deliveries$bowler)
choiceSeason = unique(matches1$season)
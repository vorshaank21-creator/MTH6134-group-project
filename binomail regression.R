# binomial regression
#league players : 
league_players= read.csv("league_players.csv")
str(league_players)
summary(league_players)
table(league_players$prev_injuries)
plot(league_players$age,
     xlab="age",
     ylab="injured")
boxplot(minures_played~ injured, data= league.data,
        xlab="injured(0=No,1=Yes)",
        ylab="minutes played")

# ashford matches 
ashford_matches.csv= read.csv("ashford_matches.csv")
str(ashford_matches.csv)
summary(ashford_matches.csv)
                            
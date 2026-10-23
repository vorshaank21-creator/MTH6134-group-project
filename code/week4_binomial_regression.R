## WEEK 4 - BINOMIAL REGRESSION
# league players
league_players <- read.csv("data/league_players.csv")
str(league_players)
summary(league_players)
table(league_players$prev_injuries)

# explore the binary response against the explanatory variables
plot(league_players$age, league_players$injured,
     xlab = "age", ylab = "injured (0=No, 1=Yes)")
boxplot(minutes_played ~ injured, data = league_players,
        xlab = "injured (0=No, 1=Yes)", ylab = "minutes played")

# fit the binomial GLM (logistic regression)
injury_glm <- glm(injured ~ training_load + prev_injuries + age + bmi,
                  data = league_players, family = binomial)
summary(injury_glm)

# confidence intervals for the change in log odds (asymptotic/Wald, as on the slides)
confint.default(injury_glm)

# odds ratios
exp(coef(injury_glm))

# analysis of deviance, terms added sequentially: the order matters
anova(injury_glm, test = "Chisq")

# ashford matches
ashford_matches <- read.csv("data/ashford_matches.csv")
str(ashford_matches)
summary(ashford_matches)

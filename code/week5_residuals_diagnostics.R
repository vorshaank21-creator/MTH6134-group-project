## WEEK 5 - RESIDUALS AND DIAGNOSTICS

ashford_matches <- read.csv("data/ashford_matches.csv")
league_players  <- read.csv("data/league_players.csv")

# install.packages("arm")   # run once, needed for binnedplot()
library(arm)

############################################################
# (A) BINOMIAL GLM - PLAYER INJURY (binary response, like lungcap)
############################################################
injury_glm <- glm(injured ~ training_load + prev_injuries + age + bmi,
                  data = league_players, family = binomial)
summary(injury_glm)

# The response is 0/1 (Bernoulli), so the residual deviance does not follow a
# chi-squared distribution: the deviance goodness-of-fit test is not valid here.

# Pearson residuals vs fitted: for 0/1 data we get two curves, one for each
# outcome, which makes the standard residual plot hard to interpret
plot(fitted(injury_glm), resid(injury_glm, type = "pearson"),
     xlab = "Fitted", ylab = "Pearson Residual", main = "Injury model")

# Binned residual plot: average residual in each bin vs average fitted value.
# About 95% of points should fall within the grey +/- 2 SE bands if the model is true
binnedplot(x = fitted(injury_glm),
           y = resid(injury_glm, type = "response"),
           xlab = "Fitted probability")

# Binned residuals against each explanatory variable: checks the linear form
par(mfrow = c(2, 2))
binnedplot(x = league_players$training_load, y = resid(injury_glm, type = "response"),
           xlab = "Training load", main = "")
binnedplot(x = league_players$prev_injuries, y = resid(injury_glm, type = "response"),
           xlab = "Previous injuries", main = "")
binnedplot(x = league_players$age, y = resid(injury_glm, type = "response"),
           xlab = "Age", main = "")
binnedplot(x = league_players$bmi, y = resid(injury_glm, type = "response"),
           xlab = "BMI", main = "")
par(mfrow = c(1, 1))

# Leverages, their average is p/n
hatvalues(injury_glm)
mean(hatvalues(injury_glm))
5/620

# Cook's distances
plot(cooks.distance(injury_glm), type = "h",
     xlab = "Obs. number", ylab = "Cook's distance")
max(cooks.distance(injury_glm))

# Do we need extra terms? Add them and check the drop in deviance
injury_glm2 <- glm(injured ~ training_load + prev_injuries + age + bmi + position,
                   data = league_players, family = binomial)
anova(injury_glm, injury_glm2, test = "Chisq")

injury_glm3 <- glm(injured ~ training_load + prev_injuries + poly(age, 2) + bmi,
                   data = league_players, family = binomial)
anova(injury_glm, injury_glm3, test = "Chisq")

############################################################
# (B) POISSON GLM - ASHFORD GOALS SCORED
############################################################
poisson_model <- glm(goals_scored ~ opponent_rank, data = ashford_matches,
                     family = poisson(link = "log"))
summary(poisson_model)

# Does it fit? Compare to the saturated model: high p-value means no evidence of misfit
1 - pchisq(poisson_model$deviance, poisson_model$df.residual)

# Deviance and Pearson residuals vs fitted
par(mfrow = c(1, 2))
plot(fitted(poisson_model), resid(poisson_model, type = "deviance"),
     xlab = "Fitted", ylab = "Deviance Residual", main = "Poisson model")
plot(fitted(poisson_model), resid(poisson_model, type = "pearson"),
     xlab = "Fitted", ylab = "Pearson Residual", main = "Poisson model")
par(mfrow = c(1, 1))

# Standardised deviance residuals against the fitted values
# look for values beyond 2 or 3, curvature, or funneling
plot(fitted(poisson_model), rstandard(poisson_model),
     xlab = "Fitted values", ylab = "Standardised Deviance Residuals")
rstandard(poisson_model)

# QQ-plot
qqnorm(rstandard(poisson_model))
qqline(rstandard(poisson_model))

# Leverages, their average is p/n
hatvalues(poisson_model)
mean(hatvalues(poisson_model))
2/46

# Cook's distances
plot(cooks.distance(poisson_model), type = "h",
     xlab = "Obs. number", ylab = "Cook's distance")
ashford_matches[8, ]   # match 8 stands out: 8-0 home win against the bottom side

# How much does match 8 change the fit? Refit without it and compare
poisson_no8 <- glm(goals_scored ~ opponent_rank, data = ashford_matches[-8, ],
                   family = poisson(link = "log"))
summary(poisson_no8)

# Is home/away missing from the model? Check the drop in deviance
poisson_model2 <- glm(goals_scored ~ opponent_rank + home_away,
                      data = ashford_matches, family = poisson(link = "log"))
summary(poisson_model2)
anova(poisson_model, poisson_model2, test = "Chisq")
1 - pchisq(poisson_model2$deviance, poisson_model2$df.residual)

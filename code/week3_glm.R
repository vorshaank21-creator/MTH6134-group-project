##WEEK 3 - INTRO TO GLMs
#Read the data and see the first 6 rows for ashford matches and league players
ashford_matches<-read.csv("data/ashford_matches.csv")
head(ashford_matches)

league_players<-read.csv("data/league_players.csv")
head(league_players)


#Exploring GLM poisson model of the ashford matches dataset with canonical log-link
poisson_model <- glm(goals_scored ~ opponent_rank, data = ashford_matches, family = poisson(link = "log"))
summary(poisson_model)
  
#Binomial GLM (Logistic Regression) - Modelling injury probability
injury_glm <- glm(injured ~ training_load + prev_injuries + age + bmi, data = league_players, 
      family = binomial(link = "logit"))
summary(injury_glm)

#Model Diagnostics + Predicting Player injury
league_players$predicted_prob <- fitted(injury_glm)

#Boxplot to visualise the data
boxplot(predicted_prob ~ injured, data = league_players,
        main = "Predicted Probabilities vs Actual Injury Status",
        xlab = "Actual Injury Status (0 = Not injured, 1 = Injured)",
        ylab = "Predicted Probability")

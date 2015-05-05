setwd("Desktop/Analytics Edge/Unit3 Logistic Regression/data/")
baseball = read.csv("baseball.csv")
str(baseball)
table(baseball$Year)

baseball = subset(baseball, Playoffs == 1)
table(baseball$Year)

# Addint an important predictor 
playOffTable = table(baseball$Year)
names(playOffTable)
playOffTable[c("1990", "2001")]
baseball$NumCompetitors = playOffTable[as.character(baseball$Year)]
str(subset(baseball, NumCompetitors == 8))
table(baseball$NumCompetitors)

# Bivariate models for predicting world series winner
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)
mod1 = glm(WorldSeries ~ Year, data = baseball, family = binomial)
mod2 = glm(WorldSeries ~ RS, data = baseball, family = binomial)
mod3 = glm(WorldSeries ~ RA, data = baseball, family = binomial)
mod4 = glm(WorldSeries ~ W, data = baseball, family = binomial)
mod5 = glm(WorldSeries ~ OBP, data = baseball, family = binomial)
mod6 = glm(WorldSeries ~ SLG, data = baseball, family = binomial)
mod7 = glm(WorldSeries ~ BA, data = baseball, family = binomial)
mod8 = glm(WorldSeries ~ RankSeason, data = baseball, family = binomial)
mod9 = glm(WorldSeries ~ OOBP, data = baseball, family = binomial)
mod10 = glm(WorldSeries ~ OSLG, data = baseball, family = binomial)
mod11 = glm(WorldSeries ~ NumCompetitors, data = baseball, family = binomial)
mod12 = glm(WorldSeries ~ League, data = baseball, family = binomial)

# Multivariate models for predicting world series winner
mod13 = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball,
            family = binomial)
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])

nmod1 = glm(WorldSeries ~ Year + RA, data = baseball, family = binomial)
nmod2 = glm(WorldSeries ~ Year + RankSeason, data = baseball, family = binomial)
nmod3 = glm(WorldSeries ~ Year + NumCompetitors, data = baseball, family = binomial)
nmod4 = glm(WorldSeries ~ RA + RankSeason, data = baseball, family = binomial)
nmod5 = glm(WorldSeries ~ NumCompetitors + RA, data = baseball, family = binomial)
nmod6 = glm(WorldSeries ~ RankSeason + NumCompetitors, data = baseball, family = binomial)







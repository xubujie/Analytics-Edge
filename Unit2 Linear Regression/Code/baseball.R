baseball = read.csv("baseball.csv")
str(baseball)

moneyball = subset(baseball, Year < 2002)
str(moneyball)
moneyball$RD = moneyball$RS - moneyball$RA
plot(moneyball$RD, moneyball$W)
WinsReg = lm(W ~ RD, data=moneyball)

predict(WinsReg)

# Scoring Runs
Model1 = lm(RS ~ OBP + SLG, data=moneyball)
Model2 = lm(RA ~ OOBP + OSLG, data=moneyball)

Player = data.frame(c(0.338,0.391,0.369,0.313,0.361),
                    c(0.540,0.450,0.347,0.447,0.500))
names(Player) = c("OBP","SLG")

predict(Model1,Player)


# playoffs
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2012)


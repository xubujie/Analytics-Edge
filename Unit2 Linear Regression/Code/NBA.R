NBA = read.csv("NBA_train.csv")
table(NBA$W, NBA$Playoffs)

NBA$PTSdiff = NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)
WinsReg = lm(W ~ PTSdiff, data=NBA)

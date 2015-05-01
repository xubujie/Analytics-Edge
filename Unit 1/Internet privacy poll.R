poll = read.csv("AnonymityPoll.csv")
summary(poll)
str(poll)
head(poll)
table(poll$Smartphone)

SouthInterviewees = subset(poll, Region=="South")
sort(table(SouthInterviewees$State))

# Internet and smartphone uses
table(poll$Internet.Use,poll$Smartphone)
sum(is.na(poll$Internet.Use))
sum(is.na(poll$Smartphone))

limited = subset(poll, poll$Internet.Use==1 | poll$Smartphone==1)

# Summarize opinions about internet privacy
summary(limited)
table(limited$Info.On.Internet)
mean(limited$Worry.About.Info == 1, na.rm=TRUE)
table(limited$Worry.About.Info)
table(limited$Anonymity.Possible)
table(limited$Privacy.Laws.Effective)
mean(limited$Tried.Masking.Identity == 1, na.rm=TRUE)

# Relating Demographics to polling results
hist(limited$Age)
plot(limited$Age, limited$Info.On.Internet)
table(limited$Age, limited$Info.On.Internet)
jitter(c(1,2,3))
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))
tapply(limited$Info.On.Internet, limited$Smartphone,mean)
tapply(limited$Tried.Masking.Identity,limited$Smartphone,mean,na.rm=TRUE)

CPSData = read.csv("CPSData.csv")
CountryCodes = read.csv("CountryCodes.csv")
str(CPSData)
summary(CPSData)
table(CPSData$Industry)

# sort
sort(table(CPSData$State))

table(CPSData$Citizenship)
table(CPSData$Race,CPSData$Hispanic)

# Evaluating missing values
table(CPSData$Region, is.na(CPSData$Married))
table(CPSData$Sex, is.na(CPSData$Married))
table(CPSData$Age, is.na(CPSData$Married))
table(CPSData$Citizenship, is.na(CPSData$Married))

a = table(CPSData$State,is.na(CPSData$MetroAreaCode))
table(CPSData$Region,is.na(CPSData$MetroAreaCode))

tapply(is.na(CPSData$MetroAreaCode),CPSData$State,mean) 
sort(tapply(is.na(CPSData$MetroAreaCode),CPSData$State,mean))

# Intergrating metropolitan area data
MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")

# merge
CPS = merge(CPSData,MetroAreaMap,by.x="MetroAreaCode",by.y="Code",all.x=TRUE)
sort(table(CPS$MetroArea))
sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))

sort(tapply(CPS$Race=="Asian",CPS$MetroArea,mean))
sort(tapply(CPS$Education=="No high school diploma",CPS$MetroArea,mean,na.rm=TRUE))

# Intergrationg country of birth data
CPS = merge(CPS,CountryMap,by.x="CountryOfBirthCode",by.y="Code",all.x=TRUE)


mean(CPS$Country[which(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")] 
     != "United States",na.rm=TRUE)

table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA",
      CPS$Country != "United States")

sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))

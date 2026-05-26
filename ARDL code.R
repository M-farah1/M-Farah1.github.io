# ARDL Model Paper - Full Code
# Loading dataset
ricedata<-read.csv("~/rice.csv")
head(ricedata)
# Data Cleaning
# Missing
any(is.na(ricedata))
sum(is.na(ricedata))
# removing missing data 
na.remove(ricedata)

# Descriptive Statistics
mean(ricedata)
mean(ricedata$fertilizer)
mean(ricedata$rainfall)
mean(ricedata$fertilizer,na.rm = TRUE)

# Finalizing dataset
finaldata<-na.omit(ricedata)
tail(finaldata)

# Descriptive Analytics
rice<-finaldata$rice
describe(rice)
library(tseries)
jarque.bera.test(rice)

fertilizer<-finaldata$fertilizer
describe(fertilizer)

# Data Visualization
ricets<-ts(rice,start = 1970,frequency=1)
windows()
ts.plot(ricets,col=4,main= "Rice Production in Somalia", 
xlab="year",ylab="Rice in tonns")
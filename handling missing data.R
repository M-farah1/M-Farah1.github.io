# Data manipulation (missing data handling)
# Missing data manipulation means handling the empty, blank, or NA values in your dataset before analysis.
# These missing values occur when:
# A value was not recorded
# A sensor failed
# A person forgot to answer
# A measurement was lost
library(haven)
data1<-read_sav("adoledata.sav")
data1$place
table(data1$place)
View(data1)
# There are 4 common methods:
# 1️⃣remove missing data 
# Remove entire rows/observations with missing values
#Good when missing values are very few (<5%)
# Example in R:
sum(is.na(data1))
# Applies the sum(is.na()) logic to every column that is missing 
colSums(is.na(data1))[colSums(is.na(data1)) > 0]
# check the variable that has missing values
sapply(data1, function(x) sum(is.na(x)))
data1 <- na.omit(data1)
# 2️⃣ Mean/Median/Mode Imputation
#Replace missing values with:
  #Mean (for normal data)
 #Median (for skewed data) non normal data 
 #Mode (for categorical data)
# Example:
data$height[is.na(data$height)] <- mean(data$height, na.rm = TRUE)
data$height[is.na(data$height)] <- median(data$height, na.rm = TRUE)
data$Sex[is.na(data$Sex)] <- mode(data$Sex, na.rm = TRUE)
# 3️⃣ Regression / Predictive Imputation

#Use other variables to predict the missing value.
#Used in machine learning and advanced statistics)

# Examples:
  
#Regression imputation

#Random forest imputation

##KNN-based imputation
#4️⃣ Use “Special Category” for Categorical Variables
#If missing data is a category, create a new class:
  # “Unknown”
#“Not recorded”
#Example:
data$Type <- ifelse(is.na(data$Type), "Unknown", data$Type)
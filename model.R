source("bedcount.R")
source("model.R")

#Read City A and B
cA <- read.csv("cityA.csv")
cB <- read.csv("cityB.csv")


#Examples in each city dataset
nrow(cA)
nrow(cB)

#Basic summary of each city
summary(cA)
summary(cB)

#Boxplot of Age for both cities
boxplot(cA$AGE_ADM, cB$AGE_ADM, main = "Boxplot of patient ages in both cities",
        ylab = "Age", xlab = "city")
axis(side = 1, at = 1:2, labels = c("A", "B"))

#Examine LOS
summary(cA$LOS)
summary(cB$LOS)

#Histograms of LOS
hist(cA$LOS, main = "Histogram of City A LOS")
hist(cB$LOS, main = "Histogram of City B LOS")
#LOS is very skewed as many people stayed for 1 day at most
#Could normalise the variable and exclude some values of the most common value (1)

#Visualise and compare GENDER and LOS
cA$GENDER <- as.factor(cA$GENDER)
cB$GENDER <- as.factor(cB$GENDER)

#City A Gender
cA_female <- cA[which(cA$GENDER == "F"),]
cA_male <- cA[which(cA$GENDER == "M"),]

hist(cA_female$LOS, main = "Histogram of Female LOS for City A")
hist(cA_male$LOS, main = "Histogram of Male LOS for City A")

#Both are still skewed, the frequency for the hist of females LOS is higher than the males

#City B Gender
cB_female <- cB[which(cB$GENDER == "F"),]
cB_male <- cB[which(cB$GENDER == "M"),]

hist(cB_female$LOS, main = "Histogram of Female LOS for City B")
hist(cB_male$LOS, main = "Histogram of Male LOS for City B")

#EVENT_TYPE
cA$EVENT_TYPE <- as.factor(cA$EVENT_TYPE)
cB$EVENT_TYPE <- as.factor(cB$EVENT_TYPE)


#City A Event
cA_BT <- cA[which(cA$EVENT_TYPE == "BT"),]
cA_ID <- cA[which(cA$EVENT_TYPE == "ID"),]
cA_IP <- cA[which(cA$EVENT_TYPE == "IP"),]

hist(cA_BT$LOS, main = "Histogram of BT LOS for City A")
hist(cA_ID$LOS, main = "Histogram of ID LOS for City A")
hist(cA_IP$LOS, main = "Histogram of IP LOS for City A")


#City B Event
cB_BT <- cB[which(cB$EVENT_TYPE == "BT"),]
cB_ID <- cB[which(cB$EVENT_TYPE == "ID"),]
cB_IP <- cB[which(cB$EVENT_TYPE == "IP"),]

hist(cB_BT$LOS, main = "Histogram of BT LOS for City B")
hist(cB_ID$LOS, main = "Histogram of ID LOS for City B")
hist(cB_IP$LOS, main = "Histogram of IP LOS for City B")


#Use the bedcount function to create a time series showing bed counts over the year
#City A
cA.bc <- bedcount(cA)
plot.bc(cA.bc)
#The plot shows the occupied beds over a year. There is an increase in occupied beds
#after July and decrease towards the next July. There is also a slight decrease near the new year's
#time. Additionally the number of beds seem to decrease every weekend throughout the year.

plot.bc(cA.bc, r=150:200)
#The plot shows the occupied beds over the new year period. It does clearly
#show a decrease in beds occupied every weekend especially near Christmas day.

#Overall, there is less people in the hospital over the weekend

#City B
cB.bc <- bedcount(cB)
plot.bc(cB.bc)

plot.bc(cB.bc, r=150:200)

#The bedcount.R function counts the unique days of the dates in the data which would end up as 7.
#This would be useful for modelling LOS since there is a clear trend of lesser people occupying beds in the
#weekend.

#Function to classify weekday and weekend
classifyDay <- function(day){
  if(day %in% c("Saturday", "Sunday")){
    return("weekend")
  }else{
    return("weekday")
  }
}

#Function to add if the start and end day is a weekday or weekend
addMoreVariables <- function(data) {
  # Order data from time zero onwards...
  data$EVENDATE <- as.Date(data$EVENDATE)
  data$EVSTDATE <- as.Date(data$EVSTDATE)
  
  data <- data[order(data$EVSTDATE),]
  
  #Store the day of the week for EVENDATE and EVSTDATE
  evenday <- weekdays(data$EVENDATE)
  evstday <- weekdays(data$EVENDATE)
  
  #Add columns
  data$StartDayType <- sapply(evenday, classifyDay)
  data$EndDayType <- sapply(evstday, classifyDay)
  
  return(data)
}

#Set Dep06 to factors
cA$Dep06 <- as.factor(cA$Dep06)
cB$Dep06 <- as.factor(cB$Dep06)

#Update cA and cB
cA <- addMoreVariables(cA)
cB <- addMoreVariables(cB)

#Log transform LOS to minimise skewed variable
cA$LOS <- log(cA$LOS)
cB$LOS <- log(cB$LOS)

#Histograms of LOS
hist(cA$LOS, main = "Histogram of City A LOS")
hist(cB$LOS, main = "Histogram of City B LOS")

#Model cA and cB without diag01 and dates
S
#Hlthspec to factor?
cA$HLTHSPEC <- as.factor(cA$HLTHSPEC)
cB$HLTHSPEC <- as.factor(cB$HLTHSPEC)

#Add age group column
addAgeGroup <- function(df)
{
  df$Age_Group <- ifelse(df$AGE_ADM >= 0 & df$AGE_ADM <= 2, "Infants",
                  ifelse(df$AGE_ADM >= 3 & df$AGE_ADM <= 25, "Adolescents",
                  ifelse(df$AGE_ADM >= 26 & df$AGE_ADM <= 49, "Adults",
                  ifelse(df$AGE_ADM >= 50 & df$AGE_ADM <= 120, "Elderly", NA))))
  #make it a factor
  df$Age_Group <- as.factor(df$Age_Group)
  return(df)
}

#Modify cA and cB
cA <- addAgeGroup(cA)
cB <- addAgeGroup(cB)

#Test function
test_func2 <- test.lm2(cA[, -c(4,5,9)], LOS ~., perc.train=0.9)
#Run 100 times
cA_errs_all <- replicate(100, test.lm(cA[, -c(4,5,9)], LOS ~., perc.train=0.9))
cB_errs_all <- replicate(100, test.lm(cB[, -c(4,5,9)], LOS ~., perc.train=0.9))

#Boxplot showing distribution of rrse values
boxplot(cA_errs_all, cB_errs_all, main = "Boxplot of 100 RRSE of city A and B models",
        xlab = "City", ylab = "RRSE")
axis(side = 1, at = 1:2, labels = c("A", "B"))

#Randomforest
test_func2 <- test.rf(cA[, -c(4,5,9)], LOS ~., perc.train=0.9)
#Run 100 times
cA_errs_all <- replicate(10, test.rf(cA[, -c(4,5,9)], LOS ~., perc.train=0.9))
cB_errs_all <- replicate(10, test.rf(cB[, -c(4,5,9)], LOS ~., perc.train=0.9))

#Boxplot showing distribution of rrse values
boxplot(cA_errs_all, cB_errs_all, main = "Boxplot of RF RRSE of city A and B models",
        xlab = "City", ylab = "RRSE")
axis(side = 1, at = 1:2, labels = c("A", "B"))


#Common diagnoses:
head(sort(table(cA$diag01), decreasing = TRUE), 5)
head(sort(table(cB$diag01), decreasing = TRUE), 5)

#Z380 G4732  N390  J189

#Subset with just Z380
z380_cA <- cA[which(cA$diag01 == "Z380"),]
z380_cB <- cB[which(cB$diag01 == "Z380"),]

hist(z380_cA$LOS, main = "Histogram of LOS for z380_cA")
hist(z380_cB$LOS, main = "Histogram of LOS for z380_cB")

#Run 100 times
z380_cA_err <- replicate(100, test.lm(z380_cA[, -c(1,3,4,5,9,12)], LOS ~., perc.train=0.9))
z380_cB_err <- replicate(100, test.lm(z380_cB[, -c(1,3,4,5,9,12)], LOS ~., perc.train=0.9))

#Boxplot showing distribution of rrse values
boxplot(z380_cA_err, z380_cB_err, main = "Boxplot of 100 RRSE of city A and B z380 models",
        xlab = "City", ylab = "RRSE")
axis(side = 1, at = 1:2, labels = c("A", "B"))

#Run 100 times RF
z380_cA_err <- replicate(100, test.rf(z380_cA[, -c(1,3,4,5,9,12)], LOS ~., perc.train=0.9))
z380_cB_err <- replicate(100, test.rf(z380_cB[, -c(1,3,4,5,9,12)], LOS ~., perc.train=0.9))

#Boxplot showing distribution of rrse values
boxplot(z380_cA_err, z380_cB_err, main = "Boxplot of 100 RF RRSE of city A and B z380 models",
        xlab = "City", ylab = "RRSE")
axis(side = 1, at = 1:2, labels = c("A", "B"))

#Set LOS to factor due to distribution
z380_cA$LOS <- as.factor(z380_cA$LOS)
z380_cB$LOS <- as.factor(z380_cB$LOS)

#Run 100 times
z380_cA_err <- replicate(100, test.lm(z380_cA[, -c(1,3,4,5,9,12)], LOS ~., perc.train=0.9))
z380_cB_err <- replicate(100, test.lm(z380_cB[, -c(1,3,4,5,9,12)], LOS ~., perc.train=0.9))

#Boxplot showing distribution of rrse values
boxplot(z380_cA_err, z380_cB_err, main = "Boxplot of 100 RRSE of city A and B z380 models",
        xlab = "City", ylab = "RRSE")
axis(side = 1, at = 1:2, labels = c("A", "B"))

#Run 100 times RF
z380_cA_err <- replicate(100, test.rf(z380_cA[, -c(1,3,4,5,9,12)], LOS ~., perc.train=0.9))
z380_cB_err <- replicate(100, test.rf(z380_cB[, -c(1,3,4,5,9,12)], LOS ~., perc.train=0.9))

#Boxplot showing distribution of rrse values
boxplot(z380_cA_err, z380_cB_err, main = "Boxplot of 100 RF RRSE of city A and B z380 models",
        xlab = "City", ylab = "RRSE")
axis(side = 1, at = 1:2, labels = c("A", "B"))

#The subset data is less skewed
#It is still slightly skewed so it is not really possible to predict the LOS for this diagnosis



#3. TIME SERIES
rm(list=ls())

# 0 - LOADING LIBRARIES ----
library(lubridate)   #Manipulate date and time
library(tidyverse)   #Manipulate data and perform data science
library(tseries)     #Create functions and data for time series analysis
library(ggplot2)     #Create attractive/flexible Data Viz
library(forecast)    #Forecasting tools for time series
library(urca)        #Perform unit root and cointegration tests
library(car)         #Perform linear regression modeling
library(astsa)       #Perform functions for time series analysis
library(caret)       #Train and evaluate machine learning models

# 1 - IMPORT DATASET ----
airquality <- read.csv2("AirQualityUCI.csv")

# Converting column "Date" in format "year/month/day"
airquality$Date <- format(as.Date(airquality$Date, format = "%d/%m/%Y"), "%Y/%m/%d")

# Converting column "Time" in object type "POSIXlt"
airquality$Time <- gsub("\\.", ":", airquality$Time)

airquality$Date <- trimws(airquality$Date)
airquality$Time <- trimws(airquality$Time)

# Unify columns "Date" & "Time" in one column "DateTime"
airquality$DateTime <- paste(airquality$Date, airquality$Time, sep = " ")
airquality$DateTime <- ymd_hms(airquality$DateTime)

str(airquality$DateTime)
head(airquality$DateTime, 5)

unique_counts <- length(unique(airquality$DateTime))
print(unique_counts)

duplicated_counts <- sum(duplicated(airquality$DateTime))
print(duplicated_counts)

duplicate_indices <- which(duplicated(airquality$DateTime))
duplicate_values <- airquality$DateTime[duplicate_indices]
print(duplicate_values)

na_count <- sum(is.na(airquality$DateTime))
print(na_count)

# Creating a backup dataset
aq <- airquality

# 2 - EXPLORATORY DATA ANALYSIS ON FULL TIME SERIES ----
summary(aq)
missing_values <- aq$CO.GT. == -200
aq <- aq[!missing_values, ]  #Deleting NA (referred as -200)

#CO.GT. as a time series
CO.GT. <- ts(data = aq$CO.GT., frequency = 24) #freq = 24 (hourly intervals)
CO.GT.diff <- ts(data = diff(aq$CO.GT.), frequency = 24)

plot(stl(CO.GT., s.window = "periodic"))
plot(stl(CO.GT.diff, s.window = "periodic"))

#EDA
library(reshape2)
Sys.setlocale("LC_TIME", "English")

summary(aq)
ggplot(aq, aes(x = DateTime, y = CO.GT.)) +
  geom_line() +
  xlab("") +
  ylab("CO") +
  ggtitle("CO Time Series") +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

diff_CO.GT <- diff(CO.GT.)
aqdiff <- aq[2:7674,]
aqdiff$diff_CO.GT <- diff_CO.GT 

ggplot(aqdiff, aes(x = DateTime, y = diff_CO.GT)) +
  geom_line() +
  xlab("") +
  ylab("CO") +
  ggtitle("CO Time Series") +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

s_diff_CO.GT <- diff(CO.GT., lag = 24)
aqdiff.s <- aq[25:7674,]
aqdiff.s$s_diff_CO.GT <- s_diff_CO.GT

ggplot(aqdiff.s, aes(x = DateTime, y = s_diff_CO.GT)) +
  geom_line() +
  xlab("") +
  ylab("CO") +
  ggtitle("CO Time Series") +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

par(mfrow=c(1,2))
acf(aq$CO.GT., main="ACF CO.GT.")
pacf(aq$CO.GT., main="PACF CO.GT.")

acf(aq$CO.GT., main="ACF CO.GT.", lag=100)
pacf(aq$CO.GT., main="PACF CO.GT.", lag=100)

# Performs the KPSS test on the time series
aq$CO.GT. %>% ur.kpss() %>% summary()

# Interpretation: the value of the KPSS test statistic (1.465) is actually greater 
#  than the critical value (0.739) at the 1% significance level.
# The stationarity hypothesis can be rejected and the conclusion 
#  that can be drawn is that the time series is non-stationary.

# ACF and PACF of differences
Acf(aqdiff$diff_CO.GT, main="ACF CO.GT.", lag=30)
Pacf(aqdiff$diff_CO.GT, main="PACF CO.GT.", lag=30)

Acf(aqdiff$diff_CO.GT, main="ACF CO.GT.", lag=100)
Pacf(aqdiff$diff_CO.GT, main="PACF CO.GT.", lag=100)
# trend and seasonality are reduced

# Performs the KPSS test on the differences of the time series
aqdiff$diff_CO.GT %>% ur.kpss() %>% summary()

# Interpretation: the value of the KPSS test statistic (0.001) is actually lower 
#  than the critical value (0.739) at the 1% significance level.
# The stationarity hypothesis can't be rejected and the conclusion 
#  that can be drawn is that the differencing data are stationary.

#ACF and PACF of seasonal differences 
Acf(aqdiff.s$s_diff_CO.GT, main = "ACF CO.GT. (seasonal difference)", lag = 30)
Pacf(aqdiff.s$s_diff_CO.GT, main = "PACF CO.GT. (seasonal difference)", lag = 30)

Acf(aqdiff.s$s_diff_CO.GT, main="ACF CO.GT.", lag=100)
Pacf(aqdiff.s$s_diff_CO.GT, main="PACF CO.GT.", lag=100)

# Performs the KPSS test on the seasonal differences
aqdiff.s$s_diff_CO.GT %>% ur.kpss() %>% summary()

# Interpretation: the value of the KPSS test statistic (0.057) is actually lower 
#  than the critical value (0.739) at the 1% significance level.
# The stationarity hypothesis can't be rejected and the conclusion 
#  that can be drawn is that the seasonal differenced data are stationary.

# 3 - CREATION OF DUMMY VARIABLES ----
#Creation of the "hour" column
airquality$hour <- as.integer(format(strptime(airquality$Time, format = "%H:%M:%S"), "%H"))
aq$hour <- as.integer(format(strptime(aq$Time, format = "%H:%M:%S"), "%H"))

#Creation of the "day" column
#2004/03/10 was a Wednesday
#2005/04/04 was a Monday
#week is set up according to: Monday(1), Sunday (7)
days <- c(rep(3,6))
days <- c(days,rep(c(rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(1,24),rep(2,24),rep(3,24)),55))
days <- c(days,c(rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(1,15)))
airquality$Day <- days

#Creation of the "month" column
aq$month <- as.integer(strftime(aq$Date, format = "%m")) 
airquality$month <- as.integer(strftime(airquality$Date, format = "%m"))

# 3.1 Monthly dummies
#boxplot to look at the monthly statistical differences
par(mfrow=c(1,1))
boxplot(aq$CO.GT.~aq$month)
#table to look at the monthly statistical differences
stat_CO.GT. <- aq%>%
  group_by(month) %>%
  summarise(Mean = mean(CO.GT.),
            Median = median(CO.GT.),
            Max = max(CO.GT.),
            Min = min(CO.GT.),
            Var = var(CO.GT.))
stat_CO.GT.

#Plots of monthly statistics
par(mfrow=c(2,3))
plot(x=stat_CO.GT.$month, y = stat_CO.GT.$Mean, type="l", main = "Monthly Mean", xlab="",ylab="")
plot(x=stat_CO.GT.$month, y = stat_CO.GT.$Median, type="l", main = "Monthly Median", xlab="",ylab="")
plot(x=stat_CO.GT.$month, y = stat_CO.GT.$Max, type="l", main = "Monthly Maximum", xlab="",ylab="")
plot(x=stat_CO.GT.$month, y = stat_CO.GT.$Min, type="l", main = "Monthly Minimum", xlab="",ylab="")
plot(x=stat_CO.GT.$month, y = stat_CO.GT.$Var, type="l", main = "Monthly Variance", xlab="",ylab="")

# Interpreation: it is quite evident that CO levels are substantially decreasing
#  during summer period - while quite high during the remaining 9 months, in particular
#  during winter months

# Creation of monthly dummies
num_mesi <- 12
num_righe <- nrow(airquality)
mat1 <- matrix(0, nrow = num_righe, ncol = num_mesi)

for (i in 1:num_mesi) {
  indices <- which(airquality$month == i)
  mat1[indices, i] <- 1
}

colnames(mat1) <- c("January", "February", "March", "April", "May", "June",
                    "July", "August", "September", "October", "November", "December")

airquality.months <- cbind(airquality, mat1)
aq.months <- airquality.months
aq.months <- aq.months[!missing_values, ]

# 3.2  Trimestral Dummies
#Jan - Mar
#Apr - Jun
#Jul - Sep
#Oct - Dec

airquality.trim <- airquality.months
airquality.trim$TRIM1 <- airquality.trim$January  + airquality.trim$February  + airquality.trim$March
airquality.trim$TRIM2 <- airquality.trim$April    + airquality.trim$May       + airquality.trim$June
airquality.trim$TRIM3 <- airquality.trim$July     +  airquality.trim$August   + airquality.trim$September
airquality.trim$TRIM4 <- airquality.trim$October + airquality.trim$November  + airquality.trim$December

#remove months
airquality.trim <- airquality.trim[,-c(20:31)]
aq.trim <- airquality.trim
aq.trim <- aq.trim[!missing_values, ]

# 3.3 Quadrimestral dummies
#January - April
#May - August
#September - December

airquality.quad <- airquality.months
airquality.quad$QUAD1 <- airquality.quad$January + airquality.quad$February + airquality.quad$March + airquality.quad$April 
airquality.quad$QUAD2 <- airquality.quad$May + airquality.quad$June + airquality.quad$July + airquality.quad$August 
airquality.quad$QUAD3 <- airquality.quad$September + airquality.quad$October + airquality.quad$November + airquality.quad$December

#remove months
airquality.quad <- airquality.quad[,-c(20:31)]
aq.quad <- airquality.quad
aq.quad <- aq.quad[!missing_values, ]

# 3.4 Semestral dummies
#January - June
#July - December

airquality.sem <- airquality.months
airquality.sem$SEM1 <- airquality.sem$January + airquality.sem$February + airquality.sem$March + 
  airquality.sem$April+ airquality.sem$May + airquality.sem$June
airquality.sem$SEM2 <- airquality.sem$July + airquality.sem$August + airquality.sem$September +
  airquality.sem$October + airquality.sem$November + airquality.sem$December

#remove months
airquality.sem <- airquality.sem[,-c(20:31)]
aq.sem <- airquality.sem
aq.sem <- aq.sem[!missing_values, ]

# 3.5 Hourly dummies
#boxplot to look at the hourly statistical differences
par(mfrow=c(1,1))

boxplot(aq$CO.GT.~aq$hour)
stat_h_CO.GT. <- aq %>%
  group_by(hour) %>%
  summarise(Mean = mean(CO.GT.),
            Median = median(CO.GT.),
            Max = max(CO.GT.),
            Min = min(CO.GT.),
            Var = var(CO.GT.))
print(stat_h_CO.GT., n = Inf)

#Plots of hourly statistics
par(mfrow=c(2,3))
plot(x=stat_h_CO.GT.$hour, y = stat_h_CO.GT.$Mean, type="l", main = "Hourly Mean", xlab="",ylab="")
plot(x=stat_h_CO.GT.$hour, y = stat_h_CO.GT.$Median, type="l", main = "Hourly Median", xlab="",ylab="")
plot(x=stat_h_CO.GT.$hour, y = stat_h_CO.GT.$Max, type="l", main = "Hourly Maximum", xlab="",ylab="")
plot(x=stat_h_CO.GT.$hour, y = stat_h_CO.GT.$Min, type="l", main = "Hourly Minimum", xlab="",ylab="")
plot(x=stat_h_CO.GT.$hour, y = stat_h_CO.GT.$Var, type="l", main = "Hourly Variance", xlab="",ylab="")

# Interpreation: it is quite evident that CO levels are substantially decreasing
#  during night and early morning hours - while quite high during working hours; 
#  surprisingly also till 21.

# Creation of hourly dummies
num_hours <- 24
mat2 <- matrix(0, nrow = nrow(airquality), ncol = num_hours)

for (i in 1:num_hours) {
  if (i == 24) {
    mat2[, i] <- ifelse(airquality$hour == 0, 1, 0)
  } else {
    mat2[, i] <- ifelse(airquality$hour == i, 1, 0)
  }
}

colnames(mat2) <- paste0("Hour_", ifelse(1:24 == 24, 0, 1:24))

airquality.hours <- cbind(airquality, mat2)
aq.hours <- airquality.hours
aq.hours <- aq.hours[!missing_values, ]

# 3.6 Four-hour dummies 
#  2:00 -  5:00
#  6:00 -  9:00
# 10:00 - 13:00
# 14:00 - 17:00
# 18:00 - 21:00
# 22:00 -  1.00

airquality.4hours <- airquality.hours
airquality.4hours$FOURTH1 <- airquality.4hours$Hour_2 + airquality.4hours$Hour_3 + airquality.4hours$Hour_4 + airquality.4hours$Hour_5
airquality.4hours$FOURTH2 <- airquality.4hours$Hour_6 + airquality.4hours$Hour_7 + airquality.4hours$Hour_8 + airquality.4hours$Hour_9 
airquality.4hours$FOURTH3 <- airquality.4hours$Hour_10 + airquality.4hours$Hour_11 + airquality.4hours$Hour_12 + airquality.4hours$Hour_13
airquality.4hours$FOURTH4 <- airquality.4hours$Hour_14 + airquality.4hours$Hour_15 + airquality.4hours$Hour_16 + airquality.4hours$Hour_17
airquality.4hours$FOURTH5 <- airquality.4hours$Hour_18 + airquality.4hours$Hour_19 + airquality.4hours$Hour_20 + airquality.4hours$Hour_21
airquality.4hours$FOURTH6 <- airquality.4hours$Hour_22 + airquality.4hours$Hour_23 + airquality.4hours$Hour_0 + airquality.4hours$Hour_1

airquality.4hours <- airquality.4hours[,-c(20:43)]
aq.4hours <- airquality.4hours
aq.4hours <- aq.4hours[!missing_values, ]

# 3.7 Six-hour dummies
#  2:00 -  7:00
#  8:00 - 13:00
# 14:00 - 19:00
# 20:00 -  1:00

airquality.6hours <- airquality.hours
airquality.6hours$SIX1 <- airquality.6hours$Hour_2 + airquality.6hours$Hour_3 + airquality.6hours$Hour_4 + 
  airquality.6hours$Hour_5 + airquality.6hours$Hour_6 + airquality.6hours$Hour_7
airquality.6hours$SIX2 <- airquality.6hours$Hour_8 + airquality.6hours$Hour_9 + airquality.6hours$Hour_10 + 
  airquality.6hours$Hour_11 + airquality.6hours$Hour_12 + airquality.6hours$Hour_13
airquality.6hours$SIX3 <- airquality.6hours$Hour_14 + airquality.6hours$Hour_15 + airquality.6hours$Hour_16 + 
  airquality.6hours$Hour_17 + airquality.6hours$Hour_18 + airquality.6hours$Hour_19
airquality.6hours$SIX4 <- airquality.6hours$Hour_20 + airquality.6hours$Hour_21 + airquality.6hours$Hour_22 + 
  airquality.6hours$Hour_23 + airquality.6hours$Hour_0 + airquality.6hours$Hour_1

airquality.6hours <- airquality.6hours[,-c(20:43)]
aq.6hours <- airquality.6hours
aq.6hours <- aq.6hours[!missing_values, ]

# 3.8 Twelve-hour dummies
#  9:00 - 20:00
# 21:00 -  8:00

airquality.12hours <- airquality.hours
airquality.12hours$TWELVE1 <-  airquality.12hours$Hour_9 + airquality.12hours$Hour_10 + 
  airquality.12hours$Hour_11 + airquality.12hours$Hour_12 + airquality.12hours$Hour_13 +
  airquality.12hours$Hour_14 + + airquality.12hours$Hour_15 + + airquality.12hours$Hour_16 +
  airquality.12hours$Hour_17 + + airquality.12hours$Hour_18 + + airquality.12hours$Hour_19 + airquality.12hours$Hour_20

airquality.12hours$TWELVE2 <-  airquality.12hours$Hour_21 + airquality.12hours$Hour_22 + 
  airquality.12hours$Hour_23 + airquality.12hours$Hour_0 + airquality.12hours$Hour_1 +
  airquality.12hours$Hour_2 + + airquality.12hours$Hour_3 + + airquality.12hours$Hour_4 +
  airquality.12hours$Hour_5 + + airquality.12hours$Hour_6 + + airquality.12hours$Hour_7 + airquality.12hours$Hour_8

airquality.12hours <- airquality.12hours[,-c(20:43)]
aq.12hours <- airquality.12hours
aq.12hours <- aq.12hours[!missing_values, ]

# 3.9 Daily dummies

airquality.day <- airquality
airquality.day$Monday <- ifelse(airquality.day$Day == 1, 1, 0)
airquality.day$Tuesday <- ifelse(airquality.day$Day == 2, 1, 0)
airquality.day$Wednesday <- ifelse(airquality.day$Day == 3, 1, 0)
airquality.day$Thursday <- ifelse(airquality.day$Day == 4, 1, 0)
airquality.day$Friday <- ifelse(airquality.day$Day == 5, 1, 0)
airquality.day$Saturday <- ifelse(airquality.day$Day == 6, 1, 0)
airquality.day$Sunday <- ifelse(airquality.day$Day == 7, 1, 0)
aq.day <- airquality.day
aq.day <- aq.day[!missing_values, ]

par(mfrow=c(1,1))
boxplot(aq.day$CO.GT.~aq.day$Day)
#table to look at the monthly statistical differences
stat_CO.GT.DAY <- aq.day%>%
  group_by(Day) %>%
  summarise(Mean = mean(CO.GT.),
            Median = median(CO.GT.),
            Max = max(CO.GT.),
            Min = min(CO.GT.),
            Var = var(CO.GT.))
stat_CO.GT.DAY

#Plots of monthly statistics
par(mfrow=c(2,3))
plot(x=stat_CO.GT.DAY$Day, y = stat_CO.GT.DAY$Mean, type="l", main = "Monthly Mean", xlab="",ylab="")
plot(x=stat_CO.GT.DAY$Day, y = stat_CO.GT.DAY$Median, type="l", main = "Monthly Median", xlab="",ylab="")
plot(x=stat_CO.GT.DAY$Day, y = stat_CO.GT.DAY$Max, type="l", main = "Monthly Maximum", xlab="",ylab="")
plot(x=stat_CO.GT.DAY$Day, y = stat_CO.GT.DAY$Min, type="l", main = "Monthly Minimum", xlab="",ylab="")
plot(x=stat_CO.GT.DAY$Day, y = stat_CO.GT.DAY$Var, type="l", main = "Monthly Variance", xlab="",ylab="")

# Interpreation: it is quite evident that CO levels are decreasing during 
#  weekends - while quite high during week/working days.


# 4 - DATASET CLEAN AND REFINEMENT  ----
missing_values_column <- aq == -200
n_Missing_values_column <- colSums(missing_values_column)
n_Missing_values_column

#NMHC.GT. column presents too many missing values: it will be excluded.
#The remaining columns shows few missing values and can be all kept. Additionally,
#we also delete the columns related to "PT08.S(n)", since they refer
#to the actual records of the sensors and they are not properly transformed
#for air quality analysis. Instead, we opt for keeping the adjusted values. 

aq1 <- aq[,c(-4,-5,-7,-9,-11,-12)]
aq1 <- aq1[!apply(aq1 == -200, 1, any), ]

aq1diff <- aqdiff
aq1diff <- aqdiff[,c(-4,-5,-7,-9,-11,-12)]
aq1diff <- aq1diff[!apply(aq1diff == -200, 1, any), ]

aq1.months <- aq.months
aq1.months <- aq1.months[,c(-4,-5,-7,-9,-11,-12)]
aq1.months <- aq1.months[!apply(aq1.months == -200, 1, any), ]

aq1.trim <- aq.trim
aq1.trim <- aq1.trim[,c(-4,-5,-7,-9,-11,-12)]
aq1.trim <- aq1.trim[!apply(aq1.trim == -200, 1, any), ]

aq1.quad <- aq.quad
aq1.quad <- aq1.quad[,c(-4,-5,-7,-9,-11,-12)]
aq1.quad <- aq1.quad[!apply(aq1.quad == -200, 1, any), ]

aq1.sem <- aq.sem
aq1.sem <- aq1.sem[,c(-4,-5,-7,-9,-11,-12)]
aq1.sem <- aq1.sem[!apply(aq1.sem == -200, 1, any), ]

aq1.hours <- aq.hours
aq1.hours <- aq1.hours[,c(-4,-5,-7,-9,-11,-12)]
aq1.hours <- aq1.hours[!apply(aq1.hours == -200, 1, any), ]

aq1.4hours <- aq.4hours
aq1.4hours <- aq1.4hours[,c(-4,-5,-7,-9,-11,-12)]
aq1.4hours <- aq1.4hours[!apply(aq1.4hours== -200, 1,any),]

aq1.6hours <- aq.6hours
aq1.6hours <- aq1.6hours[,c(-4,-5,-7,-9,-11,-12)]
aq1.6hours <- aq1.6hours[!apply(aq1.6hours== -200, 1,any),]

aq1.12hours <- aq.12hours
aq1.12hours <- aq1.12hours[,c(-4,-5,-7,-9,-11,-12)]
aq1.12hours <- aq1.12hours[!apply(aq1.12hours== -200, 1,any),]

aq1.day <- aq.day
aq1.day <- aq1.day[,c(-4,-5,-7,-9,-11,-12)]
aq1.day <- aq1.day[!apply(aq1.day== -200, 1,any),]

ggplot(aq1, aes(x = DateTime, y = CO.GT.)) +
  geom_line() +
  xlab("") +
  ylab("CO") +
  ggtitle("CO Time Series") +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# 5 - TRAIN - TEST SPLIT ----
# Indices for the training set and test set
x.is <- 1:6245
x.oos <- 6246:6941

# Creation of the training set
aq1.is<- aq1[x.is, ]
aq1diff.is <- aq1diff[(x.is-1),]
CO.GT.is <- ts(aq1.is$CO.GT., frequency = 24)
CO.GT.diff.is <- ts(aq1diff.is$diff_CO.GT, frequency =24)
# Creation of the test set
aq1.oos <- aq1[x.oos, ]
aq1diff.oos <- aq1diff[(x.oos-1),]
CO.GT.oos <- ts(aq1.oos$CO.GT., frequency = 24)
CO.GT.diff.oos <- ts(aq1diff.oos$diff_CO.GT,frequency =24)

CO.GT <- ts(aq1$CO.GT., frequency = 24)

# 6 - DICKEY FULLER: STATIONARITY CHECK ----
#Daily dummies
fit.1 <- lm(CO.GT[2:6941] ~ CO.GT[1:6940] + as.matrix(aq1.day[2:6941, c(15:20)]))
summary(fit.1)
#Adjusted R-squared:  0.6820 

#Twelve-hour dummies
fit.2 <- lm(CO.GT[2:6941] ~ CO.GT[1:6940] + as.matrix(aq1.12hours[2:6941, 15]))
summary(fit.2)
#Adjusted R-squared:  0.6894 

#Six-hour dummies
fit.3 <- lm(CO.GT[2:6941] ~ CO.GT[1:6940] + as.matrix(aq1.6hours[2:6941, c(15:17)]))
summary(fit.3)
#Adjuster R-squared:  0.7078

#Four-hour dummies
fit.4 <- lm(CO.GT[2:6941] ~ CO.GT[1:6940] + as.matrix(aq1.4hours[2:6941, c(15:19)]))
summary(fit.4)
#Adjuster R-squared:  0.7248

#Hourly dummies
fit.5 <- lm(CO.GT[2:6941] ~ CO.GT[1:6940] + as.matrix(aq1.hours[2:6941, c(15:37)]))
summary(fit.5)
#Adjusted R-squared:  0.7720

#Monthly dummies
fit.6 <- lm(CO.GT[2:6941] ~ CO.GT[1:6940] + as.matrix(aq1.months[2:6941, c(15:25)]))
summary(fit.6)
#Adjusted R-squared:  0.6809

#Trimestral dummies
fit.7 <- lm(CO.GT[2:6941] ~ CO.GT[1:6940] + as.matrix(aq1.trim[2:6941, c(15:17)]))
summary(fit.7)
#Adjusted R-squared:  0.6806

#Quadrimestral dummies
fit.8 <- lm(CO.GT[2:6941] ~ CO.GT[1:6940] + as.matrix(aq1.quad[2:6941, c(15:16)]))
summary(fit.8)
#Adjusted R-squared:  0.6808

#Semestral dummies
fit.9 <- lm(CO.GT[2:6941] ~ CO.GT[1:6940] + as.matrix(aq1.sem[2:6941, 15]))
summary(fit.9)
#Adjusted R-squared:  0.6794

#Hourly and daily dummies
fit.10 <- lm(CO.GT[2:6941] ~ CO.GT[1:6940] + as.matrix(aq1.hours[2:6941, c(15:37)]) + as.matrix(aq1.day[2:6941, c(15:20)])) 
summary(fit.10)
#Adjusted R-squared:  0.7752

#Hourly, daily and quadrimestral dummies
fit.11 <- lm(CO.GT[2:6941] ~ CO.GT[1:6940] + as.matrix(aq1.hours[2:6941, c(15:37)]) + as.matrix(aq1.day[2:6941, c(15:20)])+ as.matrix(aq1.quad[2:6941, c(15:16)])) 
summary(fit.11)
#Adjusted R-squared:  0.7774 

par(mfrow=c(1,1))
plot(fit.5$residuals, type="l")
plot(fit.10$residuals, type="l")
par(mfrow=c(1,2))
#Residual variance seems to vary over time

acf(fit.5$residuals,100)
pacf(fit.5$residuals,100)
acf(fit.10$residuals,100)
pacf(fit.10$residuals,100)
DFUL_value <- (fit.5$coefficients[2] - 1) /0.007605
#We use punitroot to compute the p-value of the test
punitroot(DFUL_value,N=6940, trend="c")
#p-value is: 9.999287e-51 - we reject the null hypothesis of stationarity

# 7 - ARIMAX WITH HOURLY SEASONALITY ----
fitA <- lm(CO.GT.is ~ as.matrix(aq1.hours[1:6245, c(15:37)]))
summary(fitA)
par(mfrow=c(1,2))
acf(fitA$residuals,40)
pacf(fitA$residuals,40)
#ACF: slowly decreasing
#PACF tends to become null after lag (3-5). Although seasonality is present
#ARIMA(3,0,0)

reg <- as.matrix(aq1.hours[1:6245, c(15:37)])

fit.ar1 <- Arima(CO.GT.is, order=c(3,0,0), 
                 xreg =  reg)
summary(fit.ar1)
#BIC=13089.56
#AICc=12901.12
#AIC=12900.85

par(mfrow=c(1,1))
plot(fit.ar1$residuals, type="l")
mean(fit.ar1$residuals)
gghistogram(fit.ar1$residuals) + ggtitle("Histogram of residuals")
par(mfrow=c(1,2))
Acf(fit.ar1$residuals, lag=30)
Pacf(fit.ar1$residuals, lag=30)
checkresiduals(fit.ar1$residuals) #reject H0: residuals are autocorrelated


train_1 <- CO.GT.is
pred8.ahead <- c()
lb8.ahead <- c()
ub8.ahead <- c()

regA <- as.matrix(aq1.hours[1:6941, c(15:37)])

for (i in 1:87) {
  
  regressors_subset <- regA[1:(6245 + (i-1)*8),]
  model <- Arima(train_1, order = c(3, 0, 0),
                 xreg = regressors_subset)
  
  regressors_subset_forecast <- regA[(6246 + (i-1)*8):(6253 + (i-1)*8),]
  forecast <- forecast(model, h = 8, 
                       xreg = regressors_subset_forecast)
  
  values <- forecast$mean
  lb_values <- forecast$lower[,"95%"]
  ub_values <- forecast$upper[,"95%"]
  
  pred8.ahead <- c(pred8.ahead, values)
  lb8.ahead <- c(lb8.ahead, lb_values)
  ub8.ahead <- c(ub8.ahead, ub_values)
  
  train_1 <- c(train_1, CO.GT.oos[(i*8-7):(i*8)])
}
length(CO.GT.oos)
length(pred8.ahead)
mse.8ahead <- mean((pred8.ahead-CO.GT.oos)^2)
mse.8ahead #0.9585411

# 8 - ARIMAX WITH HOURLY AND DAILY SEASONALITY ---- 
reg2 <- cbind(as.matrix(aq1.hours[1:6245, c(15:37)]),as.matrix(aq1.day[1:6245, c(15:20)]))

fitB <- lm(CO.GT.is ~ reg2)
summary(fitB)
par(mfrow=c(1,2))
acf(fitB$residuals,40)
pacf(fitB$residuals,40)
#ACF: slowly decreasing
#PACF tends to become null after lag 3. Although seasonality is present
#ARIMA(3,0,0)            


fit.ar2 <- Arima(CO.GT.is, order=c(3,0,0), 
                 xreg =  reg2)
summary(fit.ar2)
#BIC=13122.88
#AICc=12894.12
#AIC=12893.73

par(mfrow=c(1,1))
plot(fit.ar2$residuals, type="l")

mean(fit.ar2$residuals)
gghistogram(fit.ar2$residuals) + ggtitle("Histogram of residuals")
par(mfrow=c(1,2))
Acf(fit.ar2$residuals, lag=30)
Pacf(fit.ar2$residuals, lag=30)
checkresiduals(fit.ar2$residuals) #reject H0: residuals are autocorrelated

train_2 <- CO.GT.is
pred8.ahead2 <- c()
lb8.ahead2 <- c()
ub8.ahead2 <- c()

regB <- cbind(as.matrix(aq1.hours[1:6941, c(15:37)]),as.matrix(aq1.day[1:6941, c(15:20)]))

for (i in 1:87) {
  
  regressors_subset <- regB[1:(6245 + (i-1)*8),]
  model <- Arima(train_2, order = c(3, 0, 0),
                 xreg = regressors_subset)
  
  regressors_subset_forecast <- regB[(6246 + (i-1)*8):(6253 + (i-1)*8),]
  forecast <- forecast(model, h = 8, 
                       xreg = regressors_subset_forecast)
  
  values <- forecast$mean
  lb_values <- forecast$lower[,"95%"]
  ub_values <- forecast$upper[,"95%"]
  
  pred8.ahead2 <- c(pred8.ahead2, values)
  lb8.ahead2 <- c(lb8.ahead2, lb_values)
  ub8.ahead2 <- c(ub8.ahead2, ub_values)
  
  train_2 <- c(train_2, CO.GT.oos[(i*8-7):(i*8)])
}
mse.8ahead2 <- mean((pred8.ahead2-CO.GT.oos)^2)
mse.8ahead2 #0.9203567


# 9 - ARIMAX WITH HOURLY, DAILY AND MONTHLY SEASONALITY ---- 

reg3 <- cbind(as.matrix(aq1.hours[1:6245, c(15:37)]),as.matrix(aq1.day[1:6245, c(15:20)]),as.matrix(aq1.months[1:6245, c(15:25)]))

fitC <- lm(CO.GT.is ~ reg3)
summary(fitC)
par(mfrow=c(1,2))
acf(fitC$residuals,40)
pacf(fitC$residuals,40)
#ACF: slowly decreasing
#PACF tends to become null after lag 3. Although seasonality is present
#ARIMA(3,0,0)            

fit.ar3 <- Arima(CO.GT.is, order=c(3,0,0), 
                 xreg =  reg3)
summary(fit.ar3)
#BIC=13135.68
#AICc=12833.07
#AIC=12832.41

par(mfrow=c(1,1))
plot(fit.ar3$residuals, type="l")

mean(fit.ar3$residuals)
gghistogram(fit.ar3$residuals) + ggtitle("Histogram of residuals")
par(mfrow=c(1,2))
Acf(fit.ar3$residuals, lag=30)
Pacf(fit.ar3$residuals, lag=30)
checkresiduals(fit.ar3$residuals) #reject H0: residuals are autocorrelated


train_3 <- CO.GT.is
pred8.ahead3 <- c()
lb8.ahead3 <- c()
ub8.ahead3 <- c()

regC <- cbind(as.matrix(aq1.hours[1:6941, c(15:37)]),as.matrix(aq1.day[1:6941, c(15:20)]),as.matrix(aq1.months[1:6941, c(15:25)]))

for (i in 1:87) {
  
  regressors_subset <- regC[1:(6245 + (i-1)*8),]
  model <- Arima(train_3, order = c(3, 0, 0),
                 xreg = regressors_subset)
  
  regressors_subset_forecast <- regC[(6246 + (i-1)*8):(6253 + (i-1)*8),]
  forecast <- forecast(model, h = 8, 
                       xreg = regressors_subset_forecast)
  
  values <- forecast$mean
  lb_values <- forecast$lower[,"95%"]
  ub_values <- forecast$upper[,"95%"]
  
  pred8.ahead3 <- c(pred8.ahead3, values)
  lb8.ahead3 <- c(lb8.ahead3, lb_values)
  ub8.ahead3 <- c(ub8.ahead3, ub_values)
  
  train_3 <- c(train_3, CO.GT.oos[(i*8-7):(i*8)])
}
mse.8ahead3 <- mean((pred8.ahead3-CO.GT.oos)^2)
mse.8ahead3 # 0.9153743

# 10 - SARIMA ---- 
fit.sar1 <- Arima(CO.GT.is, order = c(3, 0, 0), 
                  seasonal = list(order=c(1, 0, 0),period=24))
summary(fit.sar1)   

#BIC=14633.89
#AICc=14593.47
#AIC= 14593.46

par(mfrow=c(1,1))
plot(fit.sar1$residuals, type="l")

mean(fit.sar1$residuals)
gghistogram(fit.sar1$residuals) + ggtitle("Histogram of residuals")
par(mfrow=c(1,2))
Acf(fit.sar1$residuals, lag=30)
Pacf(fit.sar1$residuals, lag=30)
checkresiduals(fit.sar1$residuals) #reject H0: residuals are autocorrelated

train_4 <- CO.GT.is
pred8.ahead4 <- c()
lb8.ahead4 <- c()
ub8.ahead4 <- c()

for (i in 1:87) {
  
  model <- Arima(train_4, order = c(3, 0, 0),
                 seasonal = list(order=c(1, 0, 0),period=24))
  
  forecast <- forecast(model, h = 8)
  
  values <- forecast$mean
  lb_values <- forecast$lower[,"95%"]
  ub_values <- forecast$upper[,"95%"]
  
  pred8.ahead4 <- c(pred8.ahead4, values)
  lb8.ahead4 <- c(lb8.ahead4, lb_values)
  ub8.ahead4 <- c(ub8.ahead4, ub_values)
  
  train_4 <- c(train_4, CO.GT.oos[(i*8-7):(i*8)])
}
mse.8ahead4 <- mean((pred8.ahead4-CO.GT.oos)^2)
mse.8ahead4 #1.485605

# 11 - SARIMAX WITH DAILY SEASONALITY ---- 
reg4 <- as.matrix(aq1.day[1:6245, c(15:20)])
fitD <- lm(CO.GT.is ~ reg4)
summary(fitD)
par(mfrow=c(1,2))
acf(fitD$residuals,40)
pacf(fitD$residuals,40)

fit.sar2 <- Arima(CO.GT.is, order = c(3, 0, 0), 
                  seasonal = list(order=c(1, 0, 0),period=24), xreg = reg4)
summary(fit.sar2)
#BIC=14654.59
#AICc=14573.76
#AIC=14573.71

par(mfrow=c(1,1))
plot(fit.sar2$residuals, type="l")

mean(fit.sar2$residuals)
gghistogram(fit.sar2$residuals) + ggtitle("Histogram of residuals")
par(mfrow=c(1,2))
Acf(fit.sar2$residuals, lag=30)
Pacf(fit.sar2$residuals, lag=30)
Box.test(fit.sar2$residuals, lag=48) #reject H0: residuals are autocorrelated

train_5 <- CO.GT.is
pred8.ahead5 <- c()
lb8.ahead5 <- c()
ub8.ahead5 <- c()

regD <- as.matrix(aq1.day[1:6941, c(15:20)])
for (i in 1:87) {
  
  regressors_subset <- regD[1:(6245 + (i-1)*8),]
  model <- Arima(train_5, order = c(3, 0, 0),
                 seasonal = list(order=c(1, 0, 0),period=24),
                 xreg=regressors_subset)
  
  regressors_subset_forecast <- regD[(6246 + (i-1)*8):(6253 + (i-1)*8),]
  forecast <- forecast(model, h = 8, xreg=regressors_subset_forecast)
  
  values <- forecast$mean
  lb_values <- forecast$lower[,"95%"]
  ub_values <- forecast$upper[,"95%"]
  
  pred8.ahead5 <- c(pred8.ahead5, values)
  lb8.ahead5 <- c(lb8.ahead5, lb_values)
  ub8.ahead5 <- c(ub8.ahead5, ub_values)
  
  train_5 <- c(train_5, CO.GT.oos[(i*8-7):(i*8)])
}
mse.8ahead5 <- mean((pred8.ahead5-CO.GT.oos)^2)
mse.8ahead5 # 1.434247

# 12 - SARIMAX WITH DAILY AND MONTHLY SEASONALITY ----
reg5 <- cbind(as.matrix(aq1.day[1:6245, c(15:20)]),as.matrix(aq1.months[1:6245, c(15:25)]))
fitE <- lm(CO.GT.is ~ reg5)
summary(fitE)
par(mfrow=c(1,2))
acf(fitE$residuals,40)
pacf(fitE$residuals,40)

fit.sar3 <- Arima(CO.GT.is, order = c(2, 0, 0), 
                  seasonal = list(order=c(1, 0, 0),period=24), xreg = reg5)
summary(fit.sar3)                 
#BIC=14660.88
#AICc=14512.77
#AIC=14512.61

par(mfrow=c(1,1))
plot(fit.sar3$residuals, type="l")

mean(fit.sar3$residuals)
gghistogram(fit.sar3$residuals) + ggtitle("Histogram of residuals")
par(mfrow=c(1,2))
Acf(fit.sar3$residuals, lag=30)
Pacf(fit.sar3$residuals, lag=30)
Box.test(fit.sar3$residuals, lag=48) #reject H0: residuals are autocorrelated

train_6 <- CO.GT.is
pred8.ahead6 <- c()
lb8.ahead6 <- c()
ub8.ahead6 <- c()

regE <- cbind(as.matrix(aq1.day[1:6941, c(15:20)]),as.matrix(aq1.months[1:6941, c(15:25)]))
for (i in 1:87) {
  
  regressors_subset <- regE[1:(6245 + (i-1)*8),]
  model <- Arima(train_6, order = c(2, 0, 0),
                 seasonal = list(order=c(1, 0, 0),period=24),
                 xreg=regressors_subset)
  
  regressors_subset_forecast <- regE[(6246 + (i-1)*8):(6253 + (i-1)*8),]
  forecast <- forecast(model, h = 8, xreg=regressors_subset_forecast)
  
  values <- forecast$mean
  lb_values <- forecast$lower[,"95%"]
  ub_values <- forecast$upper[,"95%"]
  
  pred8.ahead6 <- c(pred8.ahead6, values)
  lb8.ahead6 <- c(lb8.ahead6, lb_values)
  ub8.ahead6 <- c(ub8.ahead6, ub_values)
  
  train_6 <- c(train_6, CO.GT.oos[(i*8-7):(i*8)])
}
mse.8ahead6 <- mean((pred8.ahead6-CO.GT.oos)^2)
mse.8ahead6 #1.449607

# 13 - ARIMAX WITH HOURLY AND DAILY SEASONALITY. Lag 1: C6H6.GT.,NOx.GT. ----
cor(aq1.is[,c(3:9)])[,1]

# Create regressor lag at lag 1
VifLag <- lag(aq1.is[, c(4:9)], 1)
aq1.is.VifLag <- cbind(aq1.is$CO.GT, VifLag)[2:6245,]
cor(aq1.is.VifLag)[,1]

# Calculate VIF values for regressors (lag 1)
# VIF: considering all regressors
modelV <- lm(aq1.is$CO.GT[2:6245] ~ ., data = aq1.is.VifLag[,c(2:7)])
vif_values <- vif(modelV)
ggplot(data = data.frame(Variables = names(vif_values), VIF = vif_values),
       aes(x = Variables, y = VIF)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Regressors") +
  ylab("VIF Values") +
  ggtitle("Variable Importance Plot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
vif_values

# VIF: Considering all variables except for T
modelV2 <- lm(aq1.is$CO.GT[2:6245] ~ ., data = aq1.is.VifLag[,c(2:4,6:7)])
vif_values2 <- vif(modelV2)
ggplot(data = data.frame(Variables = names(vif_values2), VIF = vif_values2),
       aes(x = Variables, y = VIF)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Regressors") +
  ylab("VIF Values") +
  ggtitle("Variable Importance Plot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
vif_values2

cor(aq1.is.VifLag[,c(1:4,6:7)])[,1]

rlag1 <- aq1[,c(4,5)]
lag1 <- lag(rlag1, 1)
colnames(lag1) <- paste(colnames(lag1), "lag1", sep = "_")

#C6H6.GT.,NOx.GT.3
reg6 <- cbind(as.matrix(aq1.hours[2:6245, c(15:37)]),as.matrix(aq1.day[2:6245, c(15:20)]),as.matrix(lag1[2:6245,]))
reg6

fitF <- lm(CO.GT.is[2:6245] ~ reg6)
summary(fitF)
par(mfrow=c(1,2))
acf(fitF$residuals,40)
pacf(fitF$residuals,40)
#ACF: slowly decreasing, Although seasonality is present.
#PACF: Also the PACF tends to decrease
#ARIMA(3,0,1)            

fit.ar4 <- Arima(CO.GT.is[2:6245], order=c(3,0,1), 
                 xreg =  reg6)
summary(fit.ar4)

#BIC=12635.88
#AICc=12386.98
#AIC=12386.53

par(mfrow=c(1,1))
plot(fit.ar4$residuals, type="l")

mean(fit.ar4$residuals)
gghistogram(fit.ar4$residuals) + ggtitle("Histogram of residuals")
par(mfrow=c(1,2))
Acf(fit.ar4$residuals, lag=30)
Pacf(fit.ar4$residuals, lag=30)
checkresiduals(fit.ar4$residuals) #accept H0: residuals are not autocorrelated

train_7 <- CO.GT.is[2:6245]
pred8.ahead7 <- c()
lb8.ahead7 <- c()
ub8.ahead7 <- c()

regF <- cbind(as.matrix(aq1.hours[2:6941, c(15:37)]),as.matrix(aq1.day[2:6941, c(15:20)]),as.matrix(lag1[2:6941,]))

for (i in 1:87) {
  
  regressors_subset <- regF[1:(6244 + (i-1)*8),]
  model <- Arima(train_7, order = c(3, 0, 1),
                 xreg = regressors_subset)
  
  regressors_subset_forecast <- regF[(6245 + (i-1)*8):(6252 + (i-1)*8),]
  forecast <- forecast(model, h = 8, 
                       xreg = regressors_subset_forecast)
  
  values <- forecast$mean
  lb_values <- forecast$lower[,"95%"]
  ub_values <- forecast$upper[,"95%"]
  
  pred8.ahead7 <- c(pred8.ahead7, values)
  lb8.ahead7 <- c(lb8.ahead7, lb_values)
  ub8.ahead7 <- c(ub8.ahead7, ub_values)
  
  train_7 <- c(train_7, CO.GT.oos[(i*8-7):(i*8)])
}
length(pred8.ahead7)
mse.8ahead7 <- mean((pred8.ahead7-CO.GT.oos)^2)
mse.8ahead7 #0.4299916

par(mfrow=c(1,1))
plot(aq1$DateTime, aq1$CO.GT., type="l", ylab="CO.GT.", xlab="Date", main="ARIMAX(3,0,1) model")
lines(aq1.oos$DateTime, pred8.ahead7, col=2, lwd=1)  
abline(v=aq1.is$DateTime[6245], lty=2)

par(mfrow=c(1,1))
plot(aq1$DateTime, aq1$CO.GT., type="l", ylab="CO.GT.", xlab="Date", main="ARIMAX(3,0,1) model")
lines(aq1.oos$DateTime, lb8.ahead7, col="blue", lwd=0.5, type="l") 
lines(aq1.oos$DateTime, ub8.ahead7, col="blue", lwd=0.5, type="l")  
abline(v=aq1.is$DateTime[6245], lty=2)


# 14 - SARIMAX WITH 4-HOURS SEASONALITY. Lag 1: C6H6.GT.,NOx.GT. ---- 
reg7 <- cbind(as.matrix(aq1.4hours[2:6245, c(15:19)]),as.matrix(lag1[2:6245,]))
reg7
fitG <- lm(CO.GT.is[2:6245] ~ reg7)
summary(fitG)
par(mfrow=c(1,2))
acf(fitG$residuals,40)
pacf(fitG$residuals,40)
#ACF: slowly decreasing, Although seasonality is present.

fit.sar4 <- Arima(CO.GT.is[2:6245], order = c(3, 0, 1), 
                  seasonal = list(order=c(1, 0, 0),period=24), xreg = reg7)
summary(fit.sar4)                 
#BIC=13574.31
#AICc=13480.03
#AIC=13479.96

par(mfrow=c(1,1))
plot(fit.sar4$residuals, type="l")

mean(fit.sar4$residuals)
gghistogram(fit.sar4$residuals) + ggtitle("Histogram of residuals")
par(mfrow=c(1,2))
Acf(fit.sar4$residuals, lag=30)
Pacf(fit.sar4$residuals, lag=30)
checkresiduals(fit.sar4$residuals) #accept H0: residuals are not autocorrelated

train_8 <- CO.GT.is[2:6245]
pred8.ahead8 <- c()
lb8.ahead8 <- c()
ub8.ahead8 <- c()

regG <- cbind(as.matrix(aq1.4hours[2:6941, c(15:19)]),as.matrix(lag1[2:6941,]))
for (i in 1:87) {
  
  regressors_subset <- regG[1:(6244 + (i-1)*8),]
  model <- Arima(train_8, order = c(3, 0, 1),
                 seasonal = list(order=c(1, 0, 0),period=24),
                 xreg = regressors_subset)
  
  regressors_subset_forecast <- regG[(6245 + (i-1)*8):(6252 + (i-1)*8),]
  forecast <- forecast(model, h = 8, 
                       xreg = regressors_subset_forecast)
  
  values <- forecast$mean
  lb_values <- forecast$lower[,"95%"]
  ub_values <- forecast$upper[,"95%"]
  
  pred8.ahead8 <- c(pred8.ahead8, values)
  lb8.ahead8 <- c(lb8.ahead8, lb_values)
  ub8.ahead8 <- c(ub8.ahead8, ub_values)
  
  train_8 <- c(train_8, CO.GT.oos[(i*8-7):(i*8)])
}
length(pred8.ahead8)
mse.8ahead8 <- mean((pred8.ahead8-CO.GT.oos)^2)
mse.8ahead8 # 0.4590436 (with four-hours dummies)

par(mfrow=c(1,1))
plot(aq1$DateTime, aq1$CO.GT., type="l", ylab="CO.GT.", xlab="Date", main="SARIMAX(3,0,1)(1,0,0) model")
lines(aq1.oos$DateTime, pred8.ahead8, col=2, lwd=1)  
abline(v=aq1.is$DateTime[6245], lty=2)

par(mfrow=c(1,1))
plot(aq1$DateTime, aq1$CO.GT., type="l", ylab="CO.GT.", xlab="Date", main="SARIMAX(3,0,1)(1,0,0) model")
lines(aq1.oos$DateTime, lb8.ahead8, col="blue", lwd=0.5, type="l") 
lines(aq1.oos$DateTime, ub8.ahead8, col="blue", lwd=0.5, type="l")  
abline(v=aq1.is$DateTime[6245], lty=2)

# 15 - ARIMAX WITH HOURLY AND DAILY DUMMIES. Lag 1: C6H6.GT.,NOx.GT.,Lag2: NO2.GT. ----
VifLag2 <- lag(aq1.is[, c(4:9)], 2)
aq1.is.VifLag2 <- cbind(aq1.is$CO.GT,lag1[1:6245,], VifLag2)[3:6245,]
cor(aq1.is.VifLag2)[,1]

modelVA <- lm(aq1.is$CO.GT[3:6245] ~ ., data = aq1.is.VifLag2[,c(2:9)])
vif_valuesA <- vif(modelVA)
ggplot(data = data.frame(Variables = names(vif_valuesA), VIF = vif_valuesA),
       aes(x = Variables, y = VIF)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Regressors") +
  ylab("VIF Values") +
  ggtitle("Variable Importance Plot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
vif_valuesA

# VIF: Considering all variables except for Lag2-T
modelVA2 <- lm(aq1.is$CO.GT[3:6245] ~ ., data = aq1.is.VifLag2[,c(2:6,8:9)])
vif_valuesA2 <- vif(modelVA2)
ggplot(data = data.frame(Variables = names(vif_valuesA2), VIF = vif_valuesA2),
       aes(x = Variables, y = VIF)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Regressors") +
  ylab("VIF Values") +
  ggtitle("Variable Importance Plot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
vif_valuesA2

# VIF: Considering all variables except for Lag2-T and Lag2_NOx.GT.
modelVA3 <- lm(aq1.is$CO.GT[3:6245] ~ ., data = aq1.is.VifLag2[,c(2:4,6,8:9)])
vif_valuesA3 <- vif(modelVA3)
ggplot(data = data.frame(Variables = names(vif_valuesA3), VIF = vif_valuesA3),
       aes(x = Variables, y = VIF)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Regressors") +
  ylab("VIF Values") +
  ggtitle("Variable Importance Plot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
vif_valuesA3

cor(aq1.is.VifLag2[,c(1:4,6,8:9)])[,1]

rlag2 <- aq1[,c(6)]
lag2 <- lag(rlag2, 2)
names(lag2) <- " NO2.GT._lag2"
#C6H6.GT._lag1,NOx.GT._lag1,NO2.GT._lag2
reg8 <- cbind(as.matrix(aq1.hours[3:6245, c(15:37)]),as.matrix(aq1.day[3:6245, c(15:20)]),as.matrix(lag1[3:6245,]),lag2[3:6245])
reg8


fitH <- lm(CO.GT.is[3:6245] ~ reg8)
summary(fitH)
par(mfrow=c(1,2))
acf(fitH$residuals,40)
pacf(fitH$residuals,40)
#ACF: slowly decreasing, Although seasonality is present.
#PACF: Also the PACF tends to decrease
#ARIMA(3,0,1)   

fit.ar5 <- Arima(CO.GT.is[3:6245], order = c(3, 0, 1), xreg = reg8)
summary(fit.ar5)                 
#BIC=12636.82
#AICc=12381.21   
#AIC=12380.73 

par(mfrow=c(1,1))
plot(fit.ar5$residuals, type="l")

mean(fit.ar5$residuals)
gghistogram(fit.ar5$residuals) + ggtitle("Histogram of residuals")
par(mfrow=c(1,2))
Acf(fit.ar5$residuals, lag=30)
Pacf(fit.ar5$residuals, lag=30)
checkresiduals(fit.ar5$residuals) #reject H0: residuals are autocorrelated

train_9 <- CO.GT.is[3:6245]
pred8.ahead9 <- c()
lb8.ahead9 <- c()
ub8.ahead9 <- c()

regH <- cbind(as.matrix(aq1.hours[3:6941, c(15:37)]),as.matrix(aq1.day[3:6941, c(15:20)]),as.matrix(lag1[3:6941,]),as.matrix(lag2[3:6941]))
for (i in 1:87) {
  
  regressors_subset <- regH[1:(6243 + (i-1)*8),]
  model <- Arima(train_9, order = c(3, 0, 1),
                 xreg = regressors_subset)
  
  regressors_subset_forecast <- regH[(6244 + (i-1)*8):(6251 + (i-1)*8),]
  forecast <- forecast(model, h = 8, 
                       xreg = regressors_subset_forecast)
  
  values <- forecast$mean
  lb_values <- forecast$lower[,"95%"]
  ub_values <- forecast$upper[,"95%"]
  
  pred8.ahead9 <- c(pred8.ahead9, values)
  lb8.ahead9 <- c(lb8.ahead9, lb_values)
  ub8.ahead9 <- c(ub8.ahead9, ub_values)
  
  train_9 <- c(train_9, CO.GT.oos[(i*8-7):(i*8)])
}
length(pred8.ahead9)
mse.8ahead9 <- mean((pred8.ahead9-CO.GT.oos)^2)
mse.8ahead9 # 0.4359131

par(mfrow=c(1,1))
plot(aq1$DateTime, aq1$CO.GT., type="l", ylab="CO.GT.", xlab="Date", main="ARIMAX(3,0,1)")
lines(aq1.oos$DateTime, pred8.ahead9, col=2, lwd=1)  
abline(v=aq1.is$DateTime[6245], lty=2)

par(mfrow=c(1,1))
plot(aq1$DateTime, aq1$CO.GT., type="l", ylab="CO.GT.", xlab="Date")
lines(aq1.oos$DateTime, lb8.ahead9, col="blue", lwd=0.5, type="l")
lines(aq1.oos$DateTime, ub8.ahead9, col="blue", lwd=0.5, type="l")  
abline(v=aq1.is$DateTime[6245], lty=2)

# 16 - SARIMAX WITH 4-HOURS DUMMIES. Lag 1: C6H6.GT.,NOx.GT., Lag2: NO2.GT. ----
reg9 <- cbind(as.matrix(aq1.4hours[3:6245, c(15:19)]),as.matrix(lag1[3:6245,]),lag2[3:6245])
reg9

fitI <- lm(CO.GT.is[3:6245] ~ reg9)
summary(fitI)
par(mfrow=c(1,2))
acf(fitI$residuals,40)
pacf(fitI$residuals,40)

fit.sar5 <- Arima(CO.GT.is[3:6245], order = c(3, 0, 1), 
                  seasonal = list(order=c(1, 0, 0),period=24), xreg = reg9)
summary(fit.sar5)                
#BIC=13579.77
#AICc=13478.76
#AIC=13478.68

par(mfrow=c(1,1))
plot(fit.sar5$residuals, type="l")

mean(fit.sar5$residuals)
gghistogram(fit.sar5$residuals) + ggtitle("Histogram of residuals")
par(mfrow=c(1,2))
Acf(fit.sar5$residuals, lag=30)
Pacf(fit.sar5$residuals, lag=30)
checkresiduals(fit.sar5$residuals) #reject H0: residuals are autocorrelated

train_10 <- CO.GT.is[3:6245]
pred8.ahead10 <- c()
lb8.ahead10 <- c()
ub8.ahead10 <- c()

regI <- cbind(as.matrix(aq1.4hours[3:6941, c(15:19)]),as.matrix(lag1[3:6941,]),lag2[3:6941])
for (i in 1:87) {
  
  regressors_subset <- regI[1:(6243 + (i-1)*8),]
  model <- Arima(train_10, order = c(3, 0, 1),
                 seasonal = list(order=c(1, 0, 0),period=24),
                 xreg = regressors_subset)
  
  regressors_subset_forecast <- regI[(6244 + (i-1)*8):(6251 + (i-1)*8),]
  forecast <- forecast(model, h = 8, 
                       xreg = regressors_subset_forecast)
  
  values <- forecast$mean
  lb_values <- forecast$lower[,"95%"]
  ub_values <- forecast$upper[,"95%"]
  
  pred8.ahead10 <- c(pred8.ahead10, values)
  lb8.ahead10 <- c(lb8.ahead10, lb_values)
  ub8.ahead10 <- c(ub8.ahead10, ub_values)
  
  train_10 <- c(train_10, CO.GT.oos[(i*8-7):(i*8)])
}
length(pred8.ahead10)
mse.8ahead10 <- mean((pred8.ahead10-CO.GT.oos)^2)
mse.8ahead10 #0.4533986

par(mfrow=c(1,1))
plot(aq1$DateTime, aq1$CO.GT., type="l", ylab="CO.GT.", xlab="Date", main="SARIMAX(3,0,1)(1,0,0) model")
lines(aq1.oos$DateTime, pred8.ahead10, col=2, lwd=1)  
abline(v=aq1.is$DateTime[6245], lty=2)

par(mfrow=c(1,1))
plot(aq1$DateTime, aq1$CO.GT., type="l", ylab="CO.GT.", xlab="Date", main="SARIMAX(3,0,1)(1,0,0) model")
lines(aq1.oos$DateTime, lb8.ahead10, col="blue", lwd=0.5, type="l") 
lines(aq1.oos$DateTime, ub8.ahead10, col="blue", lwd=0.5, type="l")  
abline(v=aq1.is$DateTime[6245], lty=2)
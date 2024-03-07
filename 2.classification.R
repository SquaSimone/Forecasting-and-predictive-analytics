#2. CLASSIFICATION
rm(list = ls())

# 0 - Loading Libraries ----
library(readxl)          #Import and read data from Excel files
library(ggplot2)         #Create attractive/flexible Data Viz
library(nnet)            #Building and training neural networks
library(recipes)         #Define data preprocessing recipes and pipelines
library(caret)           #Train and evaluate machine learning models
library(boot)            #Perform bootstrapping for statistical analysis
library(ROSE)            #Balance imbalanced datasets with oversampling
library(grDevices)       #Control and manage graphics devices
library(corrplot)        #Visualize correlation matrices
library(pROC)            #Analyze ROC curves and classification performance
library(car)             #Perform linear regression modeling
library(randomForest)    #Build regression trees and random forests
library(glmnet)          #Fitting GLM with regularization, specifically Lasso - Ridge

# 1 - Dataset Cleaning ----
mdiabetes <- read_excel("diabetes.xlsx")
colnames(mdiabetes)[c(1,8,13,22)] <- c("Diabetes","HeartDisease","Healthcarecov","Income")
mdiabetes <- mdiabetes[,-23]

# Dichotomization of the dependent variable "Diabetes"
mdiabetes$Diabetes <- ifelse(mdiabetes$Diabetes %in% c(0, 1), 0, 1)

list(mdiabetes)
mdiabetes <- as.data.frame(lapply(mdiabetes, as.integer))
diabetes_df <- mdiabetes

# Dichotomization of the independent variable "GenHlth"
mdiabetes$GenHlth_1 <- ifelse(mdiabetes$GenHlth == 1, 1, 0)
mdiabetes$GenHlth_2 <- ifelse(mdiabetes$GenHlth == 2, 1, 0)
mdiabetes$GenHlth_3 <- ifelse(mdiabetes$GenHlth == 3, 1, 0)
mdiabetes$GenHlth_4 <- ifelse(mdiabetes$GenHlth == 4, 1, 0)
mdiabetes$GenHlth_5 <- ifelse(mdiabetes$GenHlth == 5, 1, 0)

# Dichotomization of the independent variable "Education"
mdiabetes$Education_1 <- ifelse(mdiabetes$Education %in% c(1,2,3), 1, 0)
mdiabetes$Education_2 <- ifelse(mdiabetes$Education == 4, 1, 0)
mdiabetes$Education_3 <- ifelse(mdiabetes$Education == 5, 1, 0)
mdiabetes$Education_4 <- ifelse(mdiabetes$Education == 6, 1, 0)

#Dichotomization of the independent variable "Income"
mdiabetes$Income_1 <- ifelse(mdiabetes$Income == 1, 1, 0)
mdiabetes$Income_2 <- ifelse(mdiabetes$Income == 2, 1, 0)
mdiabetes$Income_3 <- ifelse(mdiabetes$Income == 3, 1, 0)
mdiabetes$Income_4 <- ifelse(mdiabetes$Income == 4, 1, 0)
mdiabetes$Income_5 <- ifelse(mdiabetes$Income == 5, 1, 0)
mdiabetes$Income_6 <- ifelse(mdiabetes$Income == 6, 1, 0)
mdiabetes$Income_7 <- ifelse(mdiabetes$Income == 7, 1, 0)
mdiabetes$Income_8 <- ifelse(mdiabetes$Income == 8, 1, 0)

mdiabetes2 <- mdiabetes

# 2 - EDA - exploratory data analysis ----

# Diabetes #
mdiabetes$Diabetes <- factor(mdiabetes$Diabetes)
summary(mdiabetes$Diabetes)
ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = Diabetes), fill="blue")

# HighBP #
mdiabetes$HighBP <- factor(mdiabetes$HighBP)
summary(mdiabetes$HighBP)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = HighBP), fill="blue")

HighBP_Diabetes <- table(mdiabetes$HighBP,mdiabetes$Diabetes)
chisq.test(HighBP_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = HighBP)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, HighBP))) +
  labs(title = "Diabetes frequency by HighBP",
       x = "Diabetes",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Low pressure", "High pressure")) +
  theme_minimal()

# HighChol #
mdiabetes$HighChol <- factor(mdiabetes$HighChol)
summary(mdiabetes$HighChol)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = HighChol), fill="blue")

HighChol_Diabetes <- table(mdiabetes$HighChol,mdiabetes$Diabetes)
chisq.test(HighChol_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = HighChol)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, HighChol))) +
  labs(title = "Diabetes frequency by HighChol",
       x = "Diabetes",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Low cholesterol", "High cholesterol")) +
  theme_minimal()

# CholCheck #
mdiabetes$CholCheck <- factor(mdiabetes$CholCheck)
summary(mdiabetes$CholCheck)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = CholCheck), fill="blue")

CholCheck_Diabetes <- table(mdiabetes$CholCheck,mdiabetes$Diabetes)
chisq.test(CholCheck_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = CholCheck)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, CholCheck))) +
  labs(title = "Diabetes frequency by CholCheck",
       x = "Diabetes",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("No check", "Yes check")) +
  theme_minimal()

# BMI #
summary(mdiabetes$BMI)

ggplot(mdiabetes, aes(x = BMI)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "BMI distribution",
       x = "BMI",
       y = "Frequency") +
  theme_minimal()

ProvvData <- mdiabetes2
# Creare una variabile di classe BMI secondo la classificazione OMS
ProvvData$BMI_Class <- cut(mdiabetes$BMI, 
                           breaks = c(0, 18.5, 25, 30, 100),
                           labels = c("Underweight", "Healthy", "Overweight", "Obese"))

ggplot(ProvvData, aes(x = BMI_Class)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "BMI distribution",
       x = "BMI Class",
       y = "Frequency") +
  scale_x_discrete(labels = c("Underweight", "Healthy", "Overweight", "Obese")) +
  theme_minimal()

ggplot(data = ProvvData, aes(x = Diabetes, fill = BMI_Class)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, BMI_Class))) +
  labs(title = "Diabetes frequency by BMI_Class",
       x = "BMI_Class",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "green", "yellow", "orange"),
                    labels = c("Underweight", "Healthy", "Overweight", "Obese")) +
  theme_minimal()

# SMOKER #
mdiabetes$Smoker <- factor(mdiabetes$Smoker)
summary(mdiabetes$Smoker)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = Smoker), fill="blue")

Smoker_Diabetes <- table(mdiabetes$Smoker,mdiabetes$Diabetes)
chisq.test(Smoker_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = Smoker)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, Smoker))) +
  labs(title = "Diabetes frequency by Smoker",
       x = "Smoker",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("No smoker", "Smoker")) +
  theme_minimal()

# STROKE #
mdiabetes$Stroke <- factor(mdiabetes$Stroke)
summary(mdiabetes$Stroke)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = Stroke), fill="blue")

Stroke_Diabetes <- table(mdiabetes$Stroke,mdiabetes$Diabetes)
chisq.test(Stroke_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = Stroke)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, Stroke))) +
  labs(title = "Diabetes frequency by Stroke",
       x = "Stroke",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("No stroke", "Stroke")) +
  theme_minimal()

# HEART DISEASE #
mdiabetes$HeartDisease <- factor(mdiabetes$HeartDisease)
summary(mdiabetes$HeartDisease)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = HeartDisease), fill="blue")

HeartDisease_Diabetes <- table(mdiabetes$HeartDisease,mdiabetes$Diabetes)
chisq.test(HeartDisease_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = HeartDisease)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, HeartDisease))) +
  labs(title = "Diabetes frequency by HeartDisease",
       x = "HeartDisease",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("No HeartDisease", "HeartDisease")) +
  theme_minimal()


# PHYSACTIVITY #
mdiabetes$PhysActivity <- factor(mdiabetes$PhysActivity)
summary(mdiabetes$PhysActivity)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = PhysActivity), fill="blue")

PhysActivity_Diabetes <- table(mdiabetes$PhysActivity,mdiabetes$Diabetes)
chisq.test(PhysActivity_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = PhysActivity)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, PhysActivity))) +
  labs(title = "Diabetes frequency by PhysActivity",
       x = "PhysActivity",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
  theme_minimal()

# FRUITS #
mdiabetes$Fruits <- factor(mdiabetes$Fruits)
summary(mdiabetes$Fruits)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = Fruits), fill="blue")

Fruits_Diabetes <- table(mdiabetes$Fruits,mdiabetes$Diabetes)
chisq.test(Fruits_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = Fruits)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, Fruits))) +
  labs(title = "Diabetes frequency by Fruits",
       x = "Fruits",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
  theme_minimal()

# VEGGIES #
mdiabetes$Veggies <- factor(mdiabetes$Veggies)
summary(mdiabetes$Veggies)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = Veggies), fill="blue")

Fruits_Diabetes <- table(mdiabetes$Veggies,mdiabetes$Diabetes)
chisq.test(Fruits_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = Veggies)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, Veggies))) +
  labs(title = "Diabetes frequency by Veggies",
       x = "Veggies",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
  theme_minimal()

# HVYALCOHOLCONSUMP #
mdiabetes$HvyAlcoholConsump <- factor(mdiabetes$HvyAlcoholConsump)
summary(mdiabetes$HvyAlcoholConsump)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = HvyAlcoholConsump), fill="blue")

HvyAlcoholConsump_Diabetes <- table(mdiabetes$HvyAlcoholConsump,mdiabetes$Diabetes)
chisq.test(HvyAlcoholConsump_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = HvyAlcoholConsump)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, HvyAlcoholConsump))) +
  labs(title = "Diabetes frequency by HvyAlcoholConsump",
       x = "HvyAlcoholConsump",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
  theme_minimal()

# HEALTHCARECOV #
mdiabetes$Healthcarecov <- factor(mdiabetes$Healthcarecov)
summary(mdiabetes$Healthcarecov)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = Healthcarecov), fill="blue")

HealthcarecovConsump_Diabetes <- table(mdiabetes$Healthcarecov,mdiabetes$Diabetes)
chisq.test(HealthcarecovConsump_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = Healthcarecov)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, Healthcarecov))) +
  labs(title = "Diabetes frequency by Healthcarecov",
       x = "Healthcarecov",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
  theme_minimal()

# NODOCBCCOST #
mdiabetes$NoDocbcCost <- factor(mdiabetes$NoDocbcCost)
summary(mdiabetes$NoDocbcCost)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = NoDocbcCost), fill="blue")

NoDocbcCost_Diabetes <- table(mdiabetes$NoDocbcCost,mdiabetes$Diabetes)
chisq.test(HealthcarecovConsump_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = NoDocbcCost)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, NoDocbcCost))) +
  labs(title = "Diabetes frequency by NoDocbcCost",
       x = "NoDocbcCost",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Yes Doc", "No Doc")) +
  theme_minimal()

# GENHLTH #
mdiabetes$GenHlth <- factor(mdiabetes$GenHlth)
summary(mdiabetes$GenHlth)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = GenHlth), fill="blue")

GenHlth_Diabetes <- table(mdiabetes$GenHlth,mdiabetes$Diabetes)
chisq.test(GenHlth_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = GenHlth)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, GenHlth))) +
  labs(title = "Diabetes frequency by GenHlth",
       x = "GenHlth",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "green", "yellow", "orange", "red"),
                    labels = c("1", "2", "3", "4", "5")) +
  theme_minimal()

# MENTHLTH #
summary(mdiabetes$MentHlth)

ggplot(mdiabetes, aes(x = MentHlth)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "PhysHlth distribution",
       x = "MentHlth",
       y = "Frequenzy") +
  theme_minimal()

# PHYSHLTH #
summary(mdiabetes$PhysHlth)

ggplot(mdiabetes, aes(x = PhysHlth)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "PhysHlth distribution",
       x = "PhysHlth",
       y = "Frequenzy") +
  theme_minimal()

# DIFFWALK #
mdiabetes$DiffWalk <- factor(mdiabetes$DiffWalk)
summary(mdiabetes$DiffWalk)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = DiffWalk), fill="blue")

DiffWalk_Diabetes <- table(mdiabetes$DiffWalk,mdiabetes$Diabetes)
chisq.test(DiffWalk_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = DiffWalk)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, DiffWalk))) +
  labs(title = "Diabetes frequency by DiffWalk",
       x = "DiffWalk",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("0", "1")) +
  theme_minimal()

# SEX #
mdiabetes$Sex <- factor(mdiabetes$Sex)
summary(mdiabetes$Sex)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = Sex), fill="blue")

Sex_Diabetes <- table(mdiabetes$Sex,mdiabetes$Diabetes)
chisq.test(Sex_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = Sex)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, Sex))) +
  labs(title = "Diabetes frequency by Sex",
       x = "Sex",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Males", "Females")) +
  theme_minimal()

# AGE #
summary(mdiabetes$Age)

ggplot(mdiabetes, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Age distribution",
       x = "Age",
       y = "Frequency") +
  theme_minimal()

# EDUCATION #
mdiabetes$Education <- factor(mdiabetes$Education)
summary(mdiabetes$Education)

EducatDiab <- mdiabetes
EducatDiab$Aggregated_Education <- ifelse(EducatDiab$Education %in% c(1, 2, 3), "Aggregated", as.character(mdiabetes$Education))
EducatDiab$Aggregated_Education <- factor(EducatDiab$Aggregated_Education,
                                         levels = c("Aggregated", "4", "5", "6"),
                                         labels = c("1 & 2 & 3", "4", "5", "6"))
EducatDiab$Aggregated_Education <- factor(EducatDiab$Aggregated_Education)
summary(EducatDiab$Aggregated_Education)
ggplot(data = EducatDiab) +
  geom_bar(mapping = aes(x = Aggregated_Education), fill="blue")

Education_Diabetes <- table(EducatDiab$Aggregated_Education,EducatDiab$Diabetes)
chisq.test(GenHlth_Diabetes) #<2.2e^-16

ggplot(data = EducatDiab, aes(x = Diabetes, fill = Aggregated_Education)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, Aggregated_Education))) +
  labs(title = "Diabetes frequency by Education",
       x = "Education",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "green", "yellow", "orange"),
                    labels = c("1 & 2 & 3", "2", "3", "4")) +
  theme_minimal()

# INCOME #
mdiabetes$Income <- factor(mdiabetes$Income)
summary(mdiabetes$Income)

ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = Income), fill="blue")

income_Diabetes <- table(mdiabetes$Income,mdiabetes$Diabetes)
chisq.test(income_Diabetes) #<2.2e^-16

ggplot(data = mdiabetes, aes(x = Diabetes, fill = Income)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, Income))) +
  labs(title = "Diabetes frequency by Income",
       x = "Income",
       y = "Frequency") +
  scale_fill_manual(values = c("red","orange","yellow","cornsilk4","cadetblue1","cadetblue3","cadetblue4","blue"),
                    labels = c("1", "2", "3", "4", "5","6","7","8")) +
  theme_minimal()

## CORRELATION ##
par(mfrow=c(1,1))
corrplot(cor(diabetes_df), type = "upper")
cor(diabetes_df)

## EDA FOR REPORT ##
# Diabetes #
diabetes <- ggplot(data = mdiabetes) +
  geom_bar(mapping = aes(x = Diabetes), fill="darkgrey")
diabetes <- diabetes + theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20),  # Increase x-axis label font size
    axis.text.y = element_text(size = 20),  # Increase y-axis label font size
    axis.title.x = element_text(size = 30))  # Increase x-axis title font size
print(diabetes)
ggsave("diabetes_plot.pdf", diabetes, width = 8, height = 6)

# Education vs Diabetes #
education_diabetes <- ggplot(data = EducatDiab, aes(x = Diabetes, fill = Aggregated_Education)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, Aggregated_Education))) +
  labs(title = "",
       x = "Diabetes vs Education",
       y = "") +
  scale_fill_manual(values = c("grey90", "grey70", "grey50", "grey30"),
                    labels = c("1 & 2 & 3", "2", "3", "4"))

education_diabetes <- education_diabetes + theme_minimal() +
  theme(legend.position = "none") +
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20),  # Increase x-axis label font size
    axis.text.y = element_text(size = 20),  # Increase y-axis label font size
    axis.title.x = element_text(size = 30))  # Increase x-axis title font size
print(education_diabetes)
ggsave("education_diabetes_plot.pdf", education_diabetes, width = 8, height = 6)

# BMI vs Diabetes #
ProvvData$BMI_Class <- cut(mdiabetes$BMI, 
                           breaks = c(0, 18.5, 25, 30, 100),
                           labels = c("Underweight", "Healthy", "Overweight", "Obese"))

bmi_diabetes <- ggplot(data = ProvvData, aes(x = factor(Diabetes), fill = BMI_Class)) +
  geom_bar(position = "dodge", aes(group = interaction(factor(Diabetes), BMI_Class))) +
  labs(title = "",
       x = "Diabetes vs BMI",
       y = "") +
  scale_fill_manual(values = c("grey90", "grey70", "grey50", "grey30"),
                    labels = c("Underweight", "Healthy", "Overweight", "Obese"))

bmi_diabetes <- bmi_diabetes + theme_minimal() +
  theme(legend.position = "none") +
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20),  # Increase x-axis label font size
    axis.text.y = element_text(size = 20),  # Increase y-axis label font size
    axis.title.x = element_text(size = 30))  # Increase x-axis title font size
print(bmi_diabetes)
ggsave("bmi_diabetes_plot.pdf", bmi_diabetes, width = 8, height = 6)

# HvyAlcoholConsump vs Diabetes #
alcohol_diabetes <- ggplot(data = mdiabetes, aes(x = Diabetes, fill = HvyAlcoholConsump)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, HvyAlcoholConsump))) +
  labs(title = "",
       x = "Diabetes vs HvyAlcoholConsump",
       y = "") +
  scale_fill_manual(values = c("grey90", "grey30"), labels = c("No", "Yes"))

alcohol_diabetes <- alcohol_diabetes + theme_minimal() +
  theme(legend.position = "none") +
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20),  # Increase x-axis label font size
    axis.text.y = element_text(size = 20),  # Increase y-axis label font size
    axis.title.x = element_text(size = 30))  # Increase x-axis title font size
print(alcohol_diabetes)
ggsave("alcohol_diabetes_plot.pdf", alcohol_diabetes, width = 8, height = 6)

# Smoker vs Diabetes #
smoker_diabetes <- ggplot(data = mdiabetes, aes(x = Diabetes, fill = Smoker)) +
  geom_bar(position = "dodge", aes(group = interaction(Diabetes, Smoker))) +
  labs(title = "",
       x = "Diabetes vs Smoker",
       y = "") +
  scale_fill_manual(values = c("grey90", "grey30"), labels = c("No smoker", "Smoker"))

smoker_diabetes <- smoker_diabetes + theme_minimal() +
  theme(legend.position = "none") +
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20),  # Increase x-axis label font size
    axis.text.y = element_text(size = 20),  # Increase y-axis label font size
    axis.title.x = element_text(size = 30))  # Increase x-axis title font size
print(smoker_diabetes)
ggsave("smoker_diabetes_plot.pdf", smoker_diabetes, width = 8, height = 6)

# PhysActivity vs Diabetes #
physact_diabetes <- ggplot(data = mdiabetes, aes(x = factor(Diabetes), fill = PhysActivity)) +
  geom_bar(position = "dodge", aes(group = interaction(factor(Diabetes), PhysActivity))) +
  labs(title = "",
       x = "Diabetes vs PhysActivity",
       y = "") +
  scale_fill_manual(values = c("grey90", "grey30"), labels = c("No", "Yes")) +
  theme_minimal()

physact_diabetes <- physact_diabetes + theme_minimal() +
  theme(legend.position = "none") +
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20),  # Increase x-axis label font size
    axis.text.y = element_text(size = 20),  # Increase y-axis label font size
    axis.title.x = element_text(size = 30))  # Increase x-axis title font size
print(physact_diabetes)
ggsave("physact_diabetes_plot.pdf", physact_diabetes, width = 8, height = 6)

# 3 - Train Set and Test Set ----
set.seed(1)
n <- nrow(mdiabetes)
indexes <- sample(1:n, n*0.7)
dati.is <- mdiabetes[indexes,]
dati.oos <- mdiabetes[-indexes,]
dati.is2 <- mdiabetes2[indexes,]
dati.oos2 <- mdiabetes2[-indexes,]
# 4 - Model Estimation: Logit and Probit ----
# Full Linear Model and VIF computation
modelVif <- lm(Diabetes ~ . -GenHlth -GenHlth_5 -Education -Education_4 -Income -Income_8, data = dati.is2)
vif_values <- vif(modelVif)
vif_values

ggplot(data = data.frame(Variables = names(vif_values), VIF = vif_values),
       aes(x = Variables, y = VIF)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Regressors") +
  ylab("VIF Values") +
  ggtitle("Variable Importance Plot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
vif_values

# Logit Model
modelA <- glm(data = dati.is, Diabetes ~ HighBP + HighChol + BMI +
                Smoker + HeartDisease + PhysActivity + Fruits + Veggies +
                HvyAlcoholConsump + GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + 
                DiffWalk + Sex + Age + Education_1 + Education_2 + Education_3 + 
                 + Income_1 + Income_2 + Income_3 + Income_4 + Income_5 + 
                Income_6 + Income_7 ,  family = "binomial")
summary(modelA) #AIC: 113423

#  we have eliminated the "Smoker" variable from the model
modelB <- glm(data = dati.is, Diabetes ~ HighBP + HighChol + BMI +
                HeartDisease + PhysActivity + Fruits + Veggies +
                HvyAlcoholConsump + GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + 
                DiffWalk + Sex + Age + Education_1 + Education_2 + Education_3  + 
                Income_1 + Income_2 + Income_3 +  Income_4 + Income_5 + Income_6 + Income_7 ,  family = "binomial")
summary(modelB) #AIC: 113422

#  we have eliminated the "PhysActivity" variable from the model
modelC <- glm(data = dati.is, Diabetes ~ HighBP + HighChol + BMI +
                HeartDisease + Fruits + Veggies +HvyAlcoholConsump + 
                GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + DiffWalk + 
                Sex + Age +  Education_1 + Education_2 + Education_3 +  Income_1 +
               Income_2 + Income_3 +  Income_4 + Income_5 + Income_6 + Income_7 ,  family = "binomial")
summary(modelC) #AIC: 113422

#  we tried to add other variables in the model: "CholCheck"
modelD <- glm(data = dati.is, Diabetes ~ HighBP + HighChol + CholCheck +
                BMI + HeartDisease + Fruits + Veggies +HvyAlcoholConsump + 
                GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + DiffWalk + 
                Sex + Age +  Education_1 + Education_2 + Education_3 + 
                Income_1 + Income_2 + Income_3 + Income_4 + Income_5 + Income_6 + 
                Income_7 , family = "binomial")
summary(modelD) #AIC: 113111

#  we tried to add other variables in the model: MentHlth and PhysHlth
modelE <- glm(data = dati.is, Diabetes ~ HighBP + HighChol + CholCheck +
                BMI  + HeartDisease + Fruits + Veggies +HvyAlcoholConsump + 
                GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + 
                PhysHlth + DiffWalk + Sex + Age +  Education_1 + Education_2 + 
                Education_3 + Income_1 + Income_2 + Income_3 + Income_4 + Income_5 + 
                Income_6 + Income_7 , family = "binomial")
summary(modelE) #AIC: 113094

#  we tried adding the variable "Smoker" again
modelF <- glm(data = dati.is, Diabetes ~ HighBP + HighChol + CholCheck +
                BMI  + Smoker + HeartDisease + Fruits + Veggies +HvyAlcoholConsump + 
                GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + 
                DiffWalk + Sex + Age +  Education_1 + Education_2 + Education_3 + Income_1 + 
                Income_2 + Income_3 + Income_4 + Income_5 + Income_6 + Income_7 , family = "binomial")
summary(modelF) #AIC: 113096

#  we tried adding the variable "PhysActivity" again and we remove the variable "Smoker"
modelG <- glm(data = dati.is, Diabetes ~ HighBP + HighChol + CholCheck +
                BMI  + HeartDisease + PhysActivity +Fruits + Veggies +HvyAlcoholConsump + 
                GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + 
                DiffWalk + Sex + Age + Education_1 + Education_2 + Education_3 + 
                Income_1 + Income_2 + Income_3 + Income_4 + Income_5 + Income_6 + Income_7 , family = "binomial")
summary(modelG) #AIC: 113092

# Full Model with both "PhysActivity" and "Smoker"
modelH <- glm(data = dati.is, Diabetes ~ HighBP + HighChol + CholCheck +
                BMI  + Smoker + HeartDisease + PhysActivity +Fruits + Veggies + HvyAlcoholConsump + 
                GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + 
                DiffWalk + Sex + Age + Education_1 + Education_2 + Education_3 + 
                Income_1 + Income_2 + Income_3 + Income_4 + Income_5 + Income_6 + Income_7 , family = "binomial")
summary(modelH) #AIC: 113094

# Probit Model
modelprF <- glm(data = dati.is, Diabetes ~ HighBP + HighChol + CholCheck +
                  BMI  + Smoker + HeartDisease + Fruits + Veggies + HvyAlcoholConsump + 
                  GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + 
                  DiffWalk + Sex + Age + Education_1 + Education_2 + Education_3 + 
                  Income_1 + Income_2 + Income_3 + Income_4 + Income_5 +
                  Income_6 + Income_7, family = binomial(link = "probit"))
summary(modelprF) #AIC: 112747

modelprG <- glm(data = dati.is, Diabetes ~ HighBP + HighChol + CholCheck +
                  BMI  + HeartDisease + PhysActivity + Fruits + Veggies + HvyAlcoholConsump + 
                  GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + 
                  DiffWalk +  Sex + Age + Education_1 + Education_2 + Education_3 + 
                  Income_1 + Income_2 + Income_3 + Income_4 + Income_5 +
                  Income_6 + Income_7, family = binomial(link = "probit"))
summary(modelprG) #AIC: 112743

modelprH <- glm(data = dati.is, Diabetes ~ HighBP + HighChol + CholCheck +
                  BMI  + Smoker + HeartDisease + PhysActivity +Fruits + Veggies + HvyAlcoholConsump + 
                  GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + 
                  DiffWalk + Sex + Age + Education_1 + Education_2 + Education_3 + 
                  Income_1 + Income_2 + Income_3 + Income_4 + Income_5 + Income_6 + Income_7 , family = binomial(link = "probit"))
summary(modelprH) #AIC: 112744

# 5 - Model Prediction: Logit ----
#MODEL-F
predictF <- predict(modelF, newdata=dati.oos, type="response")
#response set the scale of the prediction on the same scale of the y(0,1)

thresholds <- seq(0.01, 1, by = 0.01)
max_accuracyF <- 0
optimal_thresholdF <- 0

# Computing accuracy for each threshold, to find the optimal
for (threshold in thresholds) {
  d_predictF <- ifelse(predictF > threshold, 1, 0)
  cmF <- table(dati.oos$Diabetes, d_predictF)
  accuracyF <- sum(diag(cmF)) / sum(cmF)
  
  if (accuracyF > max_accuracyF) {
    max_accuracyF <- accuracyF
    optimal_thresholdF <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdF))
print(paste("Accuracy:", round(accuracyF, 4)))

# Computing classification using optimal threshold
d_predictF <- ifelse(predictF > optimal_thresholdF, 1, 0)

# Confusion matrix
cm.predictF <- table(dati.oos$Diabetes, d_predictF)
cm.predictF
accuracyF <- sum(diag(cm.predictF)) / sum(cm.predictF)
precisionF <- cm.predictF[2, 2] / sum(cm.predictF[, 2])
recallF <- cm.predictF[2, 2] / sum(cm.predictF[2, ])

M_F_1 <- as.matrix(cbind(accuracyF,precisionF,recallF))
M_F_1

# Plot creation of accuracy at each threshold
accuraciesF <- numeric(length = length(thresholds))

#  Computing accuracy at each threshold
for (i in 1:length(thresholds)) {
  d_predictF <- ifelse(predictF > thresholds[i], 1, 0)
  cmF <- table(dati.oos$Diabetes, d_predictF)
  accuraciesF[i] <- sum(diag(cmF)) / sum(cmF)
}

#  Find the threshold which maximizes accuracy
max_accuracyF <- max(accuraciesF)
max_thresholdF <- thresholds[which.max(accuraciesF)]

#  Create the plot
plot(thresholds, accuraciesF, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdF, max_accuracyF, col = "red", pch = 16)


#MODEL-G
predictG <- predict(modelG, newdata=dati.oos, type="response")

max_accuracyG <- 0
optimal_thresholdG <- 0

for (threshold in thresholds) {
  d_predictG <- ifelse(predictG > threshold, 1, 0)
  cmG <- table(dati.oos$Diabetes, d_predictG)
  accuracyG <- sum(diag(cmG)) / sum(cmG)
  
  if (accuracyG > max_accuracyG) {
    max_accuracyG <- accuracyG
    optimal_thresholdG <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdG))
print(paste("Accuracy:", round(accuracyG, 4)))

d_predictG <- ifelse(predictG > optimal_thresholdG, 1, 0)
cm.predictG <- table(dati.oos$Diabetes, d_predictG)
cm.predictG
accuracyG <- sum(diag(cm.predictG)) / sum(cm.predictG)
precisionG <- cm.predictG[2, 2] / sum(cm.predictG[, 2])
recallG <- cm.predictG[2, 2] / sum(cm.predictG[2, ])

M_G_1 <- as.matrix(cbind(max_accuracyG,precisionG,recallG))
M_G_1

accuraciesG <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictG <- ifelse(predictG > thresholds[i], 1, 0)
  cmG <- table(dati.oos$Diabetes, d_predictG)
  accuraciesG[i] <- sum(diag(cmG)) / sum(cmG)
}

max_accuracyG <- max(accuraciesG)
max_thresholdG <- thresholds[which.max(accuraciesG)]

plot(thresholds, accuraciesG, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdG, max_accuracyG, col = "red", pch = 16)


#MODEL-H
predictH <- predict(modelH, newdata=dati.oos, type="response")

max_accuracyH <- 0
optimal_thresholdH <- 0

for (threshold in thresholds) {
  d_predictH <- ifelse(predictH > threshold, 1, 0)
  cmH <- table(dati.oos$Diabetes, d_predictH)
  accuracyH <- sum(diag(cmH)) / sum(cmH)
  
  if (accuracyH > max_accuracyH) {
    max_accuracyH <- accuracyH
    optimal_thresholdH <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdH))
print(paste("Accuracy:", round(accuracyH, 4)))

d_predictH <- ifelse(predictH > optimal_thresholdH, 1, 0)
cm.predictH <- table(dati.oos$Diabetes, d_predictH)
cm.predictH
accuracyH <- sum(diag(cm.predictH)) / sum(cm.predictH)
precisionH <- cm.predictH[2, 2] / sum(cm.predictH[, 2])
recallH <- cm.predictH[2, 2] / sum(cm.predictH[2, ])

M_H_1 <- as.matrix(cbind(max_accuracyH,precisionH,recallH))
M_H_1

accuraciesH <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictH <- ifelse(predictH > thresholds[i], 1, 0)
  cmH <- table(dati.oos$Diabetes, d_predictH)
  accuraciesH[i] <- sum(diag(cmH)) / sum(cmH)
}

max_accuracyH <- max(accuraciesH)
max_thresholdH <- thresholds[which.max(accuraciesH)]

plot(thresholds, accuraciesH, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")

points(max_thresholdH, max_accuracyH, col = "red", pch = 16)


M_1 <- as.matrix(rbind(M_F_1,M_G_1,M_H_1))
M_1

# 6 - Model Prediction: Probit ----
# MODEL F
predictprF <- predict(modelprF, newdata = dati.oos, type = "response")

max_accuracyprF <- 0
optimal_thresholdprF <- 0

for (threshold in thresholds) {
  d_predictprF <- ifelse(predictprF > threshold, 1, 0)
  cmprF <- table(dati.oos$Diabetes, d_predictprF)
  accuracyprF <- sum(diag(cmprF)) / sum(cmprF)
  
  if (accuracyprF > max_accuracyprF) {
    max_accuracyprF <- accuracyprF
    optimal_thresholdprF <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdprF))
print(paste("Accuracy:", round(accuracyprF, 4)))

d_predictprF <- ifelse(predictprF > optimal_thresholdprF, 1, 0)
cm.predictprF <- table(dati.oos$Diabetes, d_predictprF)
cm.predictprF
accuracyprF <- sum(diag(cm.predictprF)) / sum(cm.predictprF)
precisionprF <- cm.predictprF[2, 2] / sum(cm.predictprF[, 2])
recallprF <- cm.predictprF[2, 2] / sum(cm.predictprF[2, ])

M_PRF <- as.matrix(cbind(max_accuracyprF, precisionprF, recallprF))
M_PRF

accuraciesprF <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictprF <- ifelse(predictprF > thresholds[i], 1, 0)
  cmprF <- table(dati.oos$Diabetes, d_predictprF)
  accuraciesprF[i] <- sum(diag(cmprF)) / sum(cmprF)
}

max_accuracyprF <- max(accuraciesprF)
max_thresholdprF <- thresholds[which.max(accuraciesprF)]

plot(thresholds, accuraciesprF, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdprF, max_accuracyprF, col = "red", pch = 16)


# MODEL G
predictprG <- predict(modelprG, newdata = dati.oos, type = "response")

max_accuracyprG <- 0
optimal_thresholdprG <- 0

for (threshold in thresholds) {
  d_predictprG <- ifelse(predictprG > threshold, 1, 0)
  cmprG <- table(dati.oos$Diabetes, d_predictprG)
  accuracyprG <- sum(diag(cmprG)) / sum(cmprG)
  
  if (accuracyprG > max_accuracyprG) {
    max_accuracyprG <- accuracyprG
    optimal_thresholdprG <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdprG))
print(paste("Accuracy:", round(accuracyprG, 4)))

d_predictprG <- ifelse(predictprG > optimal_thresholdprG, 1, 0)
cm.predictprG <- table(dati.oos$Diabetes, d_predictprG)
cm.predictprG
accuracyprG <- sum(diag(cm.predictprG)) / sum(cm.predictprG)
precisionprG <- cm.predictprG[2, 2] / sum(cm.predictprG[, 2])
recallprG <- cm.predictprG[2, 2] / sum(cm.predictprG[2, ])

M_PRG <- as.matrix(cbind(max_accuracyprG, precisionprG, recallprG))
M_PRG

accuraciesprG <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictprG <- ifelse(predictprG > thresholds[i], 1, 0)
  cmprG <- table(dati.oos$Diabetes, d_predictprG)
  accuraciesprG[i] <- sum(diag(cmprG)) / sum(cmprG)
}

max_accuracyprG <- max(accuraciesprG)
max_thresholdprG <- thresholds[which.max(accuraciesprG)]

plot(thresholds, accuraciesprG, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdprG, max_accuracyprG, col = "red", pch = 16)


# MODEL H
predictprH <- predict(modelprH, newdata = dati.oos, type = "response")

max_accuracyprH <- 0
optimal_thresholdprH <- 0

for (threshold in thresholds) {
  d_predictprH <- ifelse(predictprH > threshold, 1, 0)
  cmprH <- table(dati.oos$Diabetes, d_predictprH)
  accuracyprH <- sum(diag(cmprH)) / sum(cmprH)
  
  if (accuracyprH > max_accuracyprH) {
    max_accuracyprH <- accuracyprH
    optimal_thresholdprH <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdprH))
print(paste("Accuracy:", round(accuracyprH, 4)))

d_predictprH <- ifelse(predictprH > optimal_thresholdprH, 1, 0)
cm.predictprH <- table(dati.oos$Diabetes, d_predictprH)
cm.predictprH
accuracyprH <- sum(diag(cm.predictprH)) / sum(cm.predictprH)
precisionprH <- cm.predictprH[2, 2] / sum(cm.predictprH[, 2])
recallprH <- cm.predictprH[2, 2] / sum(cm.predictprH[2, ])

M_PRH <- as.matrix(cbind(max_accuracyprH, precisionprH, recallprH))
M_PRH

accuraciesprH <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictprH <- ifelse(predictprH > thresholds[i], 1, 0)
  cmprH <- table(dati.oos$Diabetes, d_predictprH)
  accuraciesprH[i] <- sum(diag(cmprH)) / sum(cmprH)
}

max_accuracyprH <- max(accuraciesprH)
max_thresholdprH <- thresholds[which.max(accuraciesprH)]

plot(thresholds, accuraciesprH, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdprH, max_accuracyprH, col = "red", pch = 16)


M_PR1 <- as.matrix(rbind(M_PRF,M_PRG,M_PRH))
M_PR1

# 7 - Oversampling ----
# First, we calculate freq and %
diabetes_bin_freq <- table(dati.is$Diabetes)
diabetes_bin_percent <- diabetes_bin_freq / sum(diabetes_bin_freq) * 100
diabetes_bin_percent

# We count the freq of each class
class_freq <- table(dati.is$Diabetes)
class_freq

# We determine the minority class and majority class
minority_class <- names(class_freq)[which.min(class_freq)]
majority_class <- names(class_freq)[which.max(class_freq)]

# We compute the oversampling factor
oversample_factor <- class_freq[majority_class] / class_freq[minority_class]

# We oversample the minority class
minority_data <- dati.is %>%
  filter(Diabetes == minority_class)

balanced_data <- minority_data %>%
  sample_n(size = round(nrow(minority_data) * oversample_factor), replace = TRUE) %>%
  bind_rows(dati.is)

# We verify the class balance after oversampling
table(balanced_data$Diabetes)

ggplot(data = balanced_data, aes(x = factor(Diabetes))) +
  geom_bar(fill = "lightblue", color = "#444444") +
  xlab("Diabetes") +
  ylab("Frequency") +
  ggtitle("Frequency of Diabetes") +
  theme_minimal()

claim_freq <- table(balanced_data$Diabetes)
claim_percent <- claim_freq / sum(claim_freq) * 100
claim_percent

# 8 - Logit estimations and predictions, with oversampling ----
#Model F
modelOF <- glm(data = balanced_data, Diabetes ~ HighBP + HighChol + CholCheck +
                 BMI  + Smoker + HeartDisease + Fruits + Veggies +HvyAlcoholConsump + 
                 GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + 
                 DiffWalk + Sex + Age +  Education_1 + Education_2 + Education_3 + 
                 Income_1 + Income_2 + Income_3 + Income_4 + Income_5 + Income_6 + Income_7 , family = "binomial")
summary(modelOF) #337142

predictOF <- predict(modelOF, newdata = dati.oos, type = "response")

max_accuracyOF <- 0
optimal_thresholdOF <- 0

for (threshold in thresholds) {
  d_predictOF <- ifelse(predictOF > threshold, 1, 0)
  cmOF <- table(dati.oos$Diabetes, d_predictOF)
  accuracyOF <- sum(diag(cmOF)) / sum(cmOF)
  
  if (accuracyOF > max_accuracyOF) {
    max_accuracyOF <- accuracyOF
    optimal_thresholdOF <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdOF))
print(paste("Accuracy:", round(accuracyOF, 4)))

d_predictOF <- ifelse(predictOF > optimal_thresholdOF, 1, 0)
cm.predictOF <- table(dati.oos$Diabetes, d_predictOF)
cm.predictOF
accuracyOF <- sum(diag(cm.predictOF)) / sum(cm.predictOF)
precisionOF <- cm.predictOF[2, 2] / sum(cm.predictOF[, 2])
recallOF <- cm.predictOF[2, 2] / sum(cm.predictOF[2, ])

M_OF <- as.matrix(cbind(max_accuracyOF, precisionOF, recallOF))
M_OF

accuraciesOF <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictOF <- ifelse(predictOF > thresholds[i], 1, 0)
  cmOF <- table(dati.oos$Diabetes, d_predictOF)
  accuraciesOF[i] <- sum(diag(cmOF)) / sum(cmOF)
}

max_accuracyOF <- max(accuraciesOF)
max_thresholdOF <- thresholds[which.max(accuraciesOF)]

plot(thresholds, accuraciesOF, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdOF, max_accuracyOF, col = "red", pch = 16)


#Model G
modelOG <- glm(data = balanced_data , Diabetes ~ HighBP + HighChol + CholCheck +
                 BMI  + HeartDisease + PhysActivity +Fruits + Veggies +HvyAlcoholConsump + 
                 GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + DiffWalk +
                 Sex + Age +  Education_1 + Education_2 + Education_3 + Income_1 + Income_2 + Income_3 + 
                 Income_4 + Income_5 + Income_6 + Income_7, family = "binomial")
summary(modelOG) #AIC: 337137

predictOG <- predict(modelOG, newdata=dati.oos, type="response")

max_accuracyOG <- 0
optimal_thresholdOG <- 0

for (threshold in thresholds) {
  d_predictOG <- ifelse(predictOG > threshold, 1, 0)
  cmOG <- table(dati.oos$Diabetes, d_predictOG)
  accuracyOG <- sum(diag(cmOG)) / sum(cmOG)
  
  if (accuracyOG > max_accuracyOG) {
    max_accuracyOG <- accuracyOG
    optimal_thresholdOG <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdOG))
print(paste("Accuracy:", round(accuracyOG, 4)))

d_predictOG <- ifelse(predictOG > optimal_thresholdOG, 1, 0)
cm.predictOG <- table(dati.oos$Diabetes, d_predictOG)
cm.predictOG
accuracyOG <- sum(diag(cm.predictOG)) / sum(cm.predictOG)
precisionOG <- cm.predictOG[2, 2] / sum(cm.predictOG[, 2])
recallOG <- cm.predictOG[2, 2] / sum(cm.predictOG[2, ])

M_OG <- as.matrix(cbind(max_accuracyOG,precisionOG,recallOG))
M_OG

accuraciesOG <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictOG <- ifelse(predictOG > thresholds[i], 1, 0)
  cmOG <- table(dati.oos$Diabetes, d_predictOG)
  accuraciesOG[i] <- sum(diag(cmOG)) / sum(cmOG)
}

max_accuracyOG <- max(accuraciesOG)
max_thresholdOG <- thresholds[which.max(accuraciesOG)]

plot(thresholds, accuraciesOG, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdOG, max_accuracyOG, col = "red", pch = 16)


#Model H
modelOH <- glm(data = balanced_data, Diabetes ~ HighBP + HighChol + CholCheck +
                 BMI  + Smoker + HeartDisease + PhysActivity +Fruits + Veggies + HvyAlcoholConsump + 
                 GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + 
                 DiffWalk + Sex + Age + Education_1 + Education_2 + Education_3 + 
                 Income_1 + Income_2 + Income_3 + Income_4 + Income_5 + Income_6 + Income_7 , family = "binomial")
summary(modelOH) #337139

predictOH <- predict(modelOH, newdata = dati.oos, type = "response")

max_accuracyOH <- 0
optimal_thresholdOH <- 0

for (threshold in thresholds) {
  d_predictOH <- ifelse(predictOH > threshold, 1, 0)
  cmOH <- table(dati.oos$Diabetes, d_predictOH)
  accuracyOH <- sum(diag(cmOH)) / sum(cmOH)
  
  if (accuracyOH > max_accuracyOH) {
    max_accuracyOH <- accuracyOH
    optimal_thresholdOH <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdOH))
print(paste("Accuracy:", round(accuracyOH, 4)))

d_predictOH <- ifelse(predictOH > optimal_thresholdOH, 1, 0)
cm.predictOH <- table(dati.oos$Diabetes, d_predictOH)
cm.predictOH
accuracyOH <- sum(diag(cm.predictOH)) / sum(cm.predictOH)
precisionOH <- cm.predictOH[2, 2] / sum(cm.predictOH[, 2])
recallOH <- cm.predictOH[2, 2] / sum(cm.predictOH[2, ])

M_OH <- as.matrix(cbind(max_accuracyOH, precisionOH, recallOH))
M_OH

accuraciesOH <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictOH <- ifelse(predictOH > thresholds[i], 1, 0)
  cmOH <- table(dati.oos$Diabetes, d_predictOH)
  accuraciesOH[i] <- sum(diag(cmOH)) / sum(cmOH)
}

max_accuracyOH <- max(accuraciesOH)
max_thresholdOH <- thresholds[which.max(accuraciesOH)]

plot(thresholds, accuraciesOH, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdOH, max_accuracyOH, col = "red", pch = 16)


M_O1 <- as.matrix(rbind(M_OF, M_OG, M_OH))
M_O1

# 9 - Probit estimations and predictions, with oversampling ----
# MODEL F
modelprOF <- glm(data = balanced_data, Diabetes ~ HighBP + HighChol + CholCheck +
                   BMI  + Smoker + HeartDisease + Fruits + Veggies +HvyAlcoholConsump + 
                   GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth +
                   DiffWalk + Sex + Age +   Education_1 + Education_2 + Education_3 + 
                   Income_1 + Income_2 + Income_3 + Income_4 + Income_5 + Income_6 + Income_7 , family = binomial(link = "probit"))
summary(modelprOF) #AIC: 337501

predictprOF <- predict(modelprOF, newdata=dati.oos, type="response")

max_accuracyprOF <- 0
optimal_thresholdprOF <- 0

for (threshold in thresholds) {
  d_predictprOF <- ifelse(predictprOF > threshold, 1, 0)
  cmprOF <- table(dati.oos$Diabetes, d_predictprOF)
  accuracyprOF <- sum(diag(cmprOF)) / sum(cmprOF)
  
  if (accuracyprOF > max_accuracyprOF) {
    max_accuracyprOF <- accuracyprOF
    optimal_thresholdprOF <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdprOF))
print(paste("Accuracy:", round(accuracyprOF, 4)))

d_predictprOF <- ifelse(predictprOF > optimal_thresholdprOF, 1, 0)
cm.predictprOF <- table(dati.oos$Diabetes, d_predictprOF)
cm.predictprOF
accuracyprOF <- sum(diag(cm.predictprOF)) / sum(cm.predictprOF)
precisionprOF <- cm.predictprOF[2, 2] / sum(cm.predictprOF[, 2])
recallprOF <- cm.predictprOF[2, 2] / sum(cm.predictprOF[2, ])

M_PROF <- as.matrix(cbind(max_accuracyprOF, precisionprOF, recallprOF))
M_PROF

accuraciesprOF <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictprOF <- ifelse(predictprOF > thresholds[i], 1, 0)
  cmprOF <- table(dati.oos$Diabetes, d_predictprOF)
  accuraciesprOF[i] <- sum(diag(cmprOF)) / sum(cmprOF)
}

max_accuracyprOF <- max(accuraciesprOF)
max_thresholdprOF <- thresholds[which.max(accuraciesprOF)]

plot(thresholds, accuraciesprOF, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdprOF, max_accuracyprOF, col = "red", pch = 16)


#MODEL G
modelprOG <- glm(data = balanced_data, Diabetes ~ HighBP + HighChol + CholCheck +
                  BMI  + HeartDisease + PhysActivity + Fruits + Veggies + HvyAlcoholConsump + 
                  GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + DiffWalk + 
                   Sex + Age +  Education_1 + Education_2 + Education_3 + Income_1 + Income_2 + Income_3 + 
                   Income_4 + Income_5 + Income_6 + Income_7, family = binomial(link = "probit"))
summary(modelprOG) #AIC: 337493

predictprOG <- predict(modelprOG, newdata=dati.oos, type="response")

max_accuracyprOG <- 0
optimal_thresholdprOG <- 0

for (threshold in thresholds) {
  d_predictprOG <- ifelse(predictprOG > threshold, 1, 0)
  cmprOG <- table(dati.oos$Diabetes, d_predictprOG)
  accuracyprOG <- sum(diag(cmprOG)) / sum(cmprOG)
  
  if (accuracyprOG > max_accuracyprOG) {
    max_accuracyprOG <- accuracyprOG
    optimal_thresholdprOG <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdprOG))
print(paste("Accuracy:", round(accuracyprOG, 4)))

d_predictprOG <- ifelse(predictprOG > optimal_thresholdprOG, 1, 0)
cm.predictprOG <- table(dati.oos$Diabetes, d_predictprOG)
cm.predictprOG
accuracyprOG <- sum(diag(cm.predictprOG)) / sum(cm.predictprOG)
precisionprOG <- cm.predictprOG[2, 2] / sum(cm.predictprOG[, 2])
recallprOG <- cm.predictprOG[2, 2] / sum(cm.predictprOG[2, ])

M_PROG <- as.matrix(cbind(max_accuracyprOG,precisionprOG,recallprOG))
M_PROG

accuraciesprOG <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictprOG <- ifelse(predictprOG > thresholds[i], 1, 0)
  cmprOG <- table(dati.oos$Diabetes, d_predictprOG)
  accuraciesprOG[i] <- sum(diag(cmprOG)) / sum(cmprOG)
}

max_accuracyprOG <- max(accuraciesprOG)
max_thresholdprOG <- thresholds[which.max(accuraciesprOG)]

plot(thresholds, accuraciesprOG, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdprOG, max_accuracyprOG, col = "red", pch = 16)


# MODEL H
modelprOH <- glm(data = balanced_data, Diabetes ~ HighBP + HighChol + CholCheck +
                   BMI  + Smoker + HeartDisease + PhysActivity +Fruits + Veggies + HvyAlcoholConsump + 
                   GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + 
                   DiffWalk + Sex + Age + Education_1 + Education_2 + Education_3 + 
                   Income_1 + Income_2 + Income_3 + Income_4 + Income_5 + Income_6 + Income_7 , family = binomial(link = "probit"))
summary(modelprOH) #AIC: 337495

predictprOH <- predict(modelprOH, newdata=dati.oos, type="response")

max_accuracyprOH <- 0
optimal_thresholdprOH <- 0

for (threshold in thresholds) {
  d_predictprOH <- ifelse(predictprOH > threshold, 1, 0)
  cmprOH <- table(dati.oos$Diabetes, d_predictprOH)
  accuracyprOH <- sum(diag(cmprOH)) / sum(cmprOH)
  
  if (accuracyprOH > max_accuracyprOH) {
    max_accuracyprOH <- accuracyprOH
    optimal_thresholdprOH <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdprOH))
print(paste("Accuracy:", round(accuracyprOH, 4)))

d_predictprOH <- ifelse(predictprOH > optimal_thresholdprOH, 1, 0)
cm.predictprOH <- table(dati.oos$Diabetes, d_predictprOH)
cm.predictprOH
accuracyprOH <- sum(diag(cm.predictprOH)) / sum(cm.predictprOH)
precisionprOH <- cm.predictprOH[2, 2] / sum(cm.predictprOH[, 2])
recallprOH <- cm.predictprOH[2, 2] / sum(cm.predictprOH[2, ])

M_PROH <- as.matrix(cbind(max_accuracyprOH, precisionprOH, recallprOH))
M_PROH

accuraciesprOH <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictprOH <- ifelse(predictprOH > thresholds[i], 1, 0)
  cmprOH <- table(dati.oos$Diabetes, d_predictprOH)
  accuraciesprOH[i] <- sum(diag(cmprOH)) / sum(cmprOH)
}

max_accuracyprOH <- max(accuraciesprOH)
max_thresholdprOH <- thresholds[which.max(accuraciesprOH)]

plot(thresholds, accuraciesprOH, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdprOH, max_accuracyprOH, col = "red", pch = 16)


M_PRO1 <- as.matrix(rbind(M_PROF,M_PROG,M_PROH))
M_PRO1

# 10 - Undersampling -----
# First, we count the freq of each class
class_freq2 <- table(dati.is$Diabetes)
class_freq2

# We determine the minority class and majority class
minority_class2 <- names(class_freq2)[which.min(class_freq2)]
majority_class2 <- names(class_freq2)[which.max(class_freq2)]

# We calculate the undersampling factor (assuming you want to match the minority class size)
undersample_factor2 <- class_freq[minority_class2] / class_freq[majority_class2]

# We undersample the majority class
majority_data2 <- dati.is %>%
  filter(Diabetes == majority_class2)

# Then, we get the desired n of samples from the majority class
desired_majority_samples2 <- round(nrow(majority_data2) * undersample_factor2)

# We randomly sample the desired n of majority class samples
majority_data_undersampled2 <- majority_data2 %>%
  sample_n(size = desired_majority_samples2)

# We combine the undersampled majority class data with the minority class data
balanced_data2 <- bind_rows(majority_data_undersampled2, dati.is %>% filter(Diabetes == minority_class2))

# Then, we verify the class balance
table(balanced_data2$Diabetes)

ggplot(data = balanced_data2, aes(x = factor(Diabetes))) +
  geom_bar(fill = "lightblue", color = "#444444") +
  xlab("Diabetes") +
  ylab("Frequency") +
  ggtitle("Frequency of Diabetes") +
  theme_minimal()

claim_freq2 <- table(balanced_data2$Diabetes)
claim_percent2 <- claim_freq2 / sum(claim_freq2) * 100
claim_percent2

# 11 - Logit estimations and predictions, with undersampling ----
#Model F
modelUF <- glm(data = balanced_data2, Diabetes ~ HighBP + HighChol + CholCheck +
                 BMI  + Smoker + HeartDisease + Fruits + Veggies +HvyAlcoholConsump + 
                 GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + 
                 DiffWalk + Sex + Age +   Education_1 + Education_2 + Education_3 + Income_1 + 
                 Income_2 + Income_3 + Income_4 + Income_5 + Income_6 + Income_7 , family = "binomial")
summary(modelUF) #AIC: 50557

predictUF <- predict(modelUF, newdata=dati.oos, type="response")

max_accuracyUF <- 0
optimal_thresholdUF <- 0

for (threshold in thresholds) {
  d_predictUF <- ifelse(predictUF > threshold, 1, 0)
  cmUF <- table(dati.oos$Diabetes, d_predictUF)
  accuracyUF <- sum(diag(cmUF)) / sum(cmUF)
  
  if (accuracyUF > max_accuracyUF) {
    max_accuracyUF <- accuracyUF
    optimal_thresholdUF <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdUF))
print(paste("Accuracy:", round(accuracyUF, 4)))

d_predictUF <- ifelse(predictUF > optimal_thresholdUF, 1, 0)
cm.predictUF <- table(dati.oos$Diabetes, d_predictUF)
cm.predictUF
accuracyUF <- sum(diag(cm.predictUF)) / sum(cm.predictUF)
precisionUF <- cm.predictUF[2, 2] / sum(cm.predictUF[, 2])
recallUF <- cm.predictUF[2, 2] / sum(cm.predictUF[2, ])

M_UF <- as.matrix(cbind(max_accuracyUF,precisionUF,recallUF))
M_UF

accuraciesUF <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictUF <- ifelse(predictUF > thresholds[i], 1, 0)
  cmUF <- table(dati.oos$Diabetes, d_predictUF)
  accuraciesUF[i] <- sum(diag(cmUF)) / sum(cmUF)
}

max_accuracyUF <- max(accuraciesUF)
max_thresholdUF <- thresholds[which.max(accuraciesUF)]

plot(thresholds, accuraciesUF, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdUF, max_accuracyUF, col = "red", pch = 16)


#Model G
modelUG <- glm(data = balanced_data2, Diabetes ~ HighBP + HighChol + CholCheck +
                 BMI  + HeartDisease + PhysActivity +Fruits + Veggies +HvyAlcoholConsump + 
                 GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + DiffWalk + 
                 Sex + Age + Education_1 + Education_2 + Education_3 + Income_1 + Income_2 + Income_3 + 
                 Income_4 + Income_5 + Income_6 + Income_7 , family = "binomial")
summary(modelUG) #AIC: 50555

predictUG <- predict(modelUG, newdata=dati.oos, type="response")

max_accuracyUG <- 0
optimal_thresholdUG <- 0

for (threshold in thresholds) {
  d_predictUG <- ifelse(predictUG > threshold, 1, 0)
  cmUG <- table(dati.oos$Diabetes, d_predictUG)
  accuracyUG <- sum(diag(cmUG)) / sum(cmUG)
  
  if (accuracyUG > max_accuracyUG) {
    max_accuracyUG <- accuracyUG
    optimal_thresholdUG <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdUG))
print(paste("Accuracy:", round(accuracyUG, 4)))

d_predictUG <- ifelse(predictUG > optimal_thresholdUG, 1, 0)
cm.predictUG <- table(dati.oos$Diabetes, d_predictUG)
cm.predictUG
accuracyUG <- sum(diag(cm.predictUG)) / sum(cm.predictUG)
precisionUG <- cm.predictUG[2, 2] / sum(cm.predictUG[, 2])
recallUG <- cm.predictUG[2, 2] / sum(cm.predictUG[2, ])

M_UG <- as.matrix(cbind(max_accuracyUG,precisionUG,recallUG))
M_UG

accuraciesUG <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictUG <- ifelse(predictUG > thresholds[i], 1, 0)
  cmUG <- table(dati.oos$Diabetes, d_predictUG)
  accuraciesUG[i] <- sum(diag(cmUG)) / sum(cmUG)
}

max_accuracyUG <- max(accuraciesUG)
max_thresholdUG <- thresholds[which.max(accuraciesUG)]

plot(thresholds, accuraciesUG, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdUG, max_accuracyUG, col = "red", pch = 16)


#Model H
modelUH <- glm(data = balanced_data2, Diabetes ~ HighBP + HighChol + CholCheck +
                 BMI  + Smoker + HeartDisease + PhysActivity +Fruits + Veggies +HvyAlcoholConsump + 
                 GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + DiffWalk + 
                 Sex + Age + Education_1 + Education_2 + Education_3 + Income_1 + Income_2 + Income_3 + 
                 Income_4 + Income_5 + Income_6 + Income_7 , family = "binomial")
summary(modelUH) #AIC: 50557

predictUH <- predict(modelUH, newdata=dati.oos, type="response")

max_accuracyUH <- 0
optimal_thresholdUH <- 0

for (threshold in thresholds) {
  d_predictUH <- ifelse(predictUH > threshold, 1, 0)
  cmUH <- table(dati.oos$Diabetes, d_predictUH)
  accuracyUH <- sum(diag(cmUH)) / sum(cmUH)
  
  if (accuracyUH > max_accuracyUH) {
    max_accuracyUH <- accuracyUH
    optimal_thresholdUH <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdUH))
print(paste("Accuracy:", round(accuracyUH, 4)))

d_predictUH <- ifelse(predictUH > optimal_thresholdUH, 1, 0)
cm.predictUH <- table(dati.oos$Diabetes, d_predictUH)
cm.predictUH
accuracyUH <- sum(diag(cm.predictUH)) / sum(cm.predictUH)
precisionUH <- cm.predictUH[2, 2] / sum(cm.predictUH[, 2])
recallUH <- cm.predictUH[2, 2] / sum(cm.predictUH[2, ])

M_UH <- as.matrix(cbind(max_accuracyUH, precisionUH, recallUH))
M_UH

accuraciesUH <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictUH <- ifelse(predictUH > thresholds[i], 1, 0)
  cmUH <- table(dati.oos$Diabetes, d_predictUH)
  accuraciesUH[i] <- sum(diag(cmUH)) / sum(cmUH)
}

max_accuracyUH <- max(accuraciesUH)
max_thresholdUH <- thresholds[which.max(accuraciesUH)]

plot(thresholds, accuraciesUH, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdUH, max_accuracyUH, col = "red", pch = 16)


M_U <- rbind(M_UF, M_UG, M_UH)
M_U

# 12 - Probit estimations and predictions, with undersampling ----
#MODEL F
modelprUF <- glm(data = balanced_data2, Diabetes ~ HighBP + HighChol + CholCheck +
                   BMI  + Smoker + HeartDisease + Fruits + Veggies +HvyAlcoholConsump + 
                   GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + DiffWalk +
                   Sex + Age +  Education_1 + Education_2 + Education_3 + Income_1 + Income_2 + 
                   Income_3 + Income_4 + Income_5 + Income_6 + Income_7 , family = binomial(link = "probit"))
summary(modelprUF) #AIC: 50593

predictprUF <- predict(modelprUF, newdata=dati.oos, type="response")

max_accuracyprUF <- 0
optimal_thresholdprUF <- 0

for (threshold in thresholds) {
  d_predictprUF <- ifelse(predictprUF > threshold, 1, 0)
  cmprUF <- table(dati.oos$Diabetes, d_predictprUF)
  accuracyprUF <- sum(diag(cmprUF)) / sum(cmprUF)
  
  if (accuracyprUF > max_accuracyprUF) {
    max_accuracyprUF <- accuracyprUF
    optimal_thresholdprUF <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdprUF))
print(paste("Accuracy:", round(accuracyprUF, 4)))

d_predictprUF <- ifelse(predictprUF > optimal_thresholdprUF, 1, 0)
cm.predictprUF <- table(dati.oos$Diabetes, d_predictprUF)
cm.predictprUF
accuracyprUF <- sum(diag(cm.predictprUF)) / sum(cm.predictprUF)
precisionprUF <- cm.predictprUF[2, 2] / sum(cm.predictprUF[, 2])
recallprUF <- cm.predictprUF[2, 2] / sum(cm.predictprUF[2, ])

M_PRUF <- as.matrix(cbind(max_accuracyprUF, precisionprUF, recallprUF))
M_PRUF

accuraciesprUF <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictprUF <- ifelse(predictprUF > thresholds[i], 1, 0)
  cmprUF <- table(dati.oos$Diabetes, d_predictprUF)
  accuraciesprUF[i] <- sum(diag(cmprUF)) / sum(cmprUF)
}

max_accuracyprUF <- max(accuraciesprUF)
max_thresholdprUF <- thresholds[which.max(accuraciesprUF)]

plot(thresholds, accuraciesprUF, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdprUF, max_accuracyprUF, col = "red", pch = 16)


#MODEL G
modelprUG <- glm(data = balanced_data2, Diabetes ~ HighBP + HighChol + CholCheck +
                   BMI  + HeartDisease + PhysActivity + Fruits + Veggies + HvyAlcoholConsump + 
                   GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + DiffWalk + 
                   Sex + Age + Education_1 + Education_2 + Education_3 + Income_1 + Income_2 + Income_3 + 
                   Income_4 + Income_5 + Income_6 + Income_7,  family = binomial(link = "probit"))
summary(modelprUG) #AIC: 50591

predictprUG <- predict(modelprUG, newdata=dati.oos, type="response")

max_accuracyprUG <- 0
optimal_thresholdprUG <- 0

for (threshold in thresholds) {
  d_predictprUG <- ifelse(predictprUG > threshold, 1, 0)
  cmprUG <- table(dati.oos$Diabetes, d_predictprUG)
  accuracyprUG <- sum(diag(cmprUG)) / sum(cmprUG)
  
  if (accuracyprUG > max_accuracyprUG) {
    max_accuracyprUG <- accuracyprUG
    optimal_thresholdprUG <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdprUG))
print(paste("Accuracy:", round(accuracyprUG, 4)))

d_predictprUG <- ifelse(predictprUG > optimal_thresholdprUG, 1, 0)
cm.predictprUG <- table(dati.oos$Diabetes, d_predictprUG)
cm.predictprUG
accuracyprUG <- sum(diag(cm.predictprUG)) / sum(cm.predictprUG)
precisionprUG <- cm.predictprUG[2, 2] / sum(cm.predictprUG[, 2])
recallprUG <- cm.predictprUG[2, 2] / sum(cm.predictprUG[2, ])

M_PRUG <- as.matrix(cbind(max_accuracyprUG,precisionprUG,recallprUG))
M_PRUG

accuraciesprUG <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictprUG <- ifelse(predictprUG > thresholds[i], 1, 0)
  cmprUG <- table(dati.oos$Diabetes, d_predictprUG)
  accuraciesprUG[i] <- sum(diag(cmprUG)) / sum(cmprUG)
}

max_accuracyprUG <- max(accuraciesprUG)
max_thresholdprUG <- thresholds[which.max(accuraciesprUG)]

plot(thresholds, accuraciesprUG, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdprUG, max_accuracyprUG, col = "red", pch = 16)


# MODEL H
modelprUH <- glm(data = balanced_data2, Diabetes ~ HighBP + HighChol + CholCheck +
                   BMI  + Smoker + HeartDisease + PhysActivity +Fruits + Veggies + HvyAlcoholConsump + 
                   GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + MentHlth + PhysHlth + 
                   DiffWalk + Sex + Age + Education_1 + Education_2 + Education_3 + 
                   Income_1 + Income_2 + Income_3 + Income_4 + Income_5 + Income_6 + Income_7 , family = binomial(link = "probit"))
summary(modelprUH) #AIC: 50593

predictprUH <- predict(modelprUH, newdata=dati.oos, type="response")

max_accuracyprUH <- 0
optimal_thresholdprUH <- 0

for (threshold in thresholds) {
  d_predictprUH <- ifelse(predictprUH > threshold, 1, 0)
  cmprUH <- table(dati.oos$Diabetes, d_predictprUH)
  accuracyprUH <- sum(diag(cmprUH)) / sum(cmprUH)
  
  if (accuracyprUH > max_accuracyprUH) {
    max_accuracyprUH <- accuracyprUH
    optimal_thresholdprUH <- threshold
  }
}

print(paste("Optimal Threshold:", optimal_thresholdprUH))
print(paste("Accuracy:", round(accuracyprUH, 4)))

d_predictprUH <- ifelse(predictprUH > optimal_thresholdprUH, 1, 0)
cm.predictprUH <- table(dati.oos$Diabetes, d_predictprUH)
cm.predictprUH
accuracyprUH <- sum(diag(cm.predictprUH)) / sum(cm.predictprUH)
precisionprUH <- cm.predictprUH[2, 2] / sum(cm.predictprUH[, 2])
recallprUH <- cm.predictprUH[2, 2] / sum(cm.predictprUH[2, ])

M_PRUH <- as.matrix(cbind(max_accuracyprUH, precisionprUH, recallprUH))
M_PRUH

accuraciesprUH <- numeric(length = length(thresholds))

for (i in 1:length(thresholds)) {
  d_predictprUH <- ifelse(predictprUH > thresholds[i], 1, 0)
  cmprUH <- table(dati.oos$Diabetes, d_predictprUH)
  accuraciesprUH[i] <- sum(diag(cmprUH)) / sum(cmprUH)
}

max_accuracyprUH <- max(accuraciesprUH)
max_thresholdprUH <- thresholds[which.max(accuraciesprUH)]

plot(thresholds, accuraciesprUH, type = "l", xlab = "Threshold", ylab = "Accuracy",
     main = "Accuracy at each Threshold")
points(max_thresholdprUH, max_accuracyprUH, col = "red", pch = 16)


M_PRU <- rbind(M_PRUF, M_PRUG, M_PRUH)
M_PRU

# 13 - Lasso estimations and predictions ----
##Logit I
lasso_model.logit <- cv.glmnet(as.matrix(dati.is2[, c(-1,-7,-13,-14,-15,-21,-22,-27,-31,-39)]), dati.is2$Diabetes, family="binomial", weights = ifelse(dati.is2$Diabetes == 0, 1, 2), alpha = 1)
summary(lasso_model.logit)
lasso_coefficients_logit <- coef(lasso_model.logit)
print(lasso_coefficients_logit)
pred_lasso.logit <- predict(lasso_model.logit, newx = as.matrix(dati.oos2[, c(-1,-7,-13,-14,-15,-21,-22,-27, -31, -39)]), type="response")
  
d.lasso.logit.oos <- 1*(pred_lasso.logit>0.5)
  
cm.lasso.logit <- table(d.lasso.logit.oos, dati.oos$Diabetes)
cm.lasso.logit
  
acc.lasso.logit <- sum(diag(cm.lasso.logit))/sum(cm.lasso.logit)
acc.lasso.logit
#accuracy 0.8469
precision.lasso.logit <- (cm.lasso.logit [2,2])/colSums(cm.lasso.logit)[2]
precision.lasso.logit
#precision 0.3666
recall.lasso.logit <- cm.lasso.logit[2, 2] / sum(cm.lasso.logit[2, ])
recall.lasso.logit
#recall 0.4429
  
plot(lasso_model.logit)
  
LASSO_LOGIT <- cbind(acc.lasso.logit,precision.lasso.logit,recall.lasso.logit)
LASSO_LOGIT

##Probit III
lasso_model.probit <- cv.glmnet(as.matrix(dati.is2[, c(-1,-7,-13,-14,-15,-21,-22,-27, -31, -39)]), dati.is2$Diabetes, family=binomial(link="probit"), weights = ifelse(dati.is2$Diabetes == 0, 1, 2), alpha = 1)
summary(lasso_model.probit)
lasso_coefficients_probit <- coef(lasso_model.logit)
print(lasso_coefficients_probit)
pred_lasso.probit <- predict(lasso_model.probit, newx = as.matrix(dati.oos2[, c(-1,-7,-13,-14,-15,-21,-22,-27, -31, -39)]), type="response")
  
d.lasso.probit.oos <- 1*(pred_lasso.probit>0.5)

cm.lasso.probit <- table(d.lasso.probit.oos, dati.oos$Diabetes)
cm.lasso.probit
  
acc.lasso.probit <- sum(diag(cm.lasso.probit))/sum(cm.lasso.probit)
acc.lasso.probit
#accuracy 0.8485231
precision.lasso.probit <- (cm.lasso.probit [2,2])/colSums(cm.lasso.probit)[2]
precision.lasso.probit
#precision 0.3570355
recall.lasso.probit <- cm.lasso.probit[2, 2] / sum(cm.lasso.probit[2, ])
recall.lasso.probit
#recall 0.4480198
  
plot(lasso_model.probit)
  
LASSO_PROBIT <- cbind(acc.lasso.probit,precision.lasso.probit,recall.lasso.probit)
LASSO_PROBIT
  
  
rbind(LASSO_LOGIT,LASSO_PROBIT)
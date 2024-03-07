#1. REGRESSION
rm(list = ls())

# 0 - LOADING LIBRARIES ----
#library(readxl)       #Import and read data from Excel files
library(ggplot2)      #Create attractive/flexible Data Viz
library(gridExtra)    #Data Viz export: arrange & display multiple plots on 1 page
#library(nnet)         #Work with neural networks for data analysis
library(dplyr)        #Perform efficient data manipulation tasks
library(recipes)      #Define data preprocessing recipes and pipelines
library(caret)        #Train and evaluate machine learning models
library(boot)         #Perform bootstrapping for statistical analysis
library(glmnet)       #Fitting GLM with regularization, specifically Lasso - Ridge
#library(ROSE)         #Balance imbalanced datasets with oversampling
library(grDevices)    #Control and manage graphics devices
library(corrplot)     #Visualize correlation matrices
library(psych)        #Visualize descriptive statistics, factor analysis
#library(pROC)         #Analyze ROC curves and classification performance
library(car)          #Perform linear regression modeling
library(stats)        #Performs essential functions for statistical analysis
library(leaps)        #Perform subset selection for linear regression
library(lmtest)       #Test assumptions of linear regression
library(tree)         #Build regression trees and random forests
library(ISLR)         #Build regression trees and random forests
library(MASS)         #Build regression trees and random forests
library(randomForest) #Build regression trees and random forests
library(gbm)          #Implement gradient boosting for predictive modeling
library(quantreg)     #Perform quantile regression modeling

# 1 - IMPORT DATASET ----
#insurance <- read_excel("insurance.xlsx")
insurance <- read.csv("insurance.csv", header = TRUE)

# 2 - DATASET DESCRIPTION ----
# *Age*: insurance contractor age, in years
# *Sex*: insurance contractor gender [female, male]
# *BMI*: body mass index (kg / m ^ 2), ideally 18.5 to 24.9
# *Children*: n of children/dependent covered by health insurance
# *Smoker*: smoking [yes, no]
# *Region*: beneficiary's area in the US [northeast, southeast, southwest, northwest]
# *Charges*: individual medical costs billed by health insurance, in $

list(insurance)
sum(is.na(insurance)) # NO missing values
head(insurance)

# 3 - DATASET CLEANING & PREP ----
#Transforming into factor variables
insurance$sex <- as.factor(insurance$sex)
insurance$children <- as.factor(insurance$children)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

insurance_df <- as.data.frame(insurance)
insurance_eda <- insurance

insurancenum <- insurance
insurancenum <- as.data.frame(lapply(insurance, as.integer))

# 4 - EDA (exploratory data analysis) ----
head(insurance)
summary(insurance)

# Converting variable bmi into classes
age_breaks <- c(18, 20, 30, 40, 50, 60, 64)
age_labels <- c("Under 20", "21-30", "31-40", "41-50", "51-60", "60+")
insurance_eda$age <- cut(insurance_eda$age, breaks = age_breaks, labels = age_labels, include.lowest = TRUE)

# Converting variable bmi into classes according to WHO classification
bmi_breaks <- c(0, 18.5, 25, 30, 60)
bmi_labels <- c("underweight", "healthy", "overweight", "obese")
insurance_eda$bmi <- cut(insurance_eda$bmi, breaks = bmi_breaks, labels = bmi_labels, include.lowest = TRUE)

# EDA on single variable
age <- ggplot(data = insurance_eda, aes(x = age)) +
  geom_bar(mapping = aes(x = age), fill = "darkgrey", color = "white") +
  scale_y_continuous(labels = NULL)
age <- age + geom_text(
  aes(label = ..count..),
  stat = "count",
  vjust = -0.1,
  show.legend = FALSE,
  size = 8,
  fontface = "bold")
age <- age + theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20),  # Increase x-axis label font size
    #axis.text.y = element_text(size = 14),  # Increase y-axis label font size
    axis.title.x = element_text(size = 30))  # Increase x-axis title font size
print(age)
#ggsave("age_plot.pdf", age, width = 8, height = 6)

sex <- ggplot(data = insurance_eda, aes(x = sex)) +
  geom_bar(mapping = aes(x = sex), fill="darkgrey",  color = "white") +
  scale_y_continuous(labels = NULL)
sex <- sex + geom_text(
  aes(label = ..count..),
  stat = "count",
  vjust = -0.1,
  show.legend = FALSE,
  size = 3.5,
  fontface = "bold")
sex <- sex + theme_minimal() +
  theme(
    axis.title.y = element_blank())
print(sex)

bmi <- ggplot(data = insurance_eda, aes(x = bmi)) +
  geom_bar(mapping = aes(x = bmi), fill = "darkgrey", color = "white") +
  scale_y_continuous(labels = NULL)
bmi <- bmi + geom_text(
  aes(label = ..count..),
  stat = "count",
  vjust = -0.1,
  show.legend = FALSE,
  size = 8,
  fontface = "bold")
bmi <- bmi + theme_minimal() +
  theme(
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 20),  # Increase x-axis label font size
      #axis.text.y = element_text(size = 14),  # Increase y-axis label font size
      axis.title.x = element_text(size = 30))  # Increase x-axis title font size
print(bmi)
#ggsave("bmi_plot.pdf", bmi, width = 8, height = 6)

children <- ggplot(data = insurance_eda, aes(x = children)) +
  geom_bar(mapping = aes(x = children), fill="darkgrey", color = "white") +
  scale_y_continuous(labels = NULL)
children <- children + geom_text(
  aes(label = ..count..),
  stat = "count",
  vjust = -0.1,
  show.legend = FALSE,
  size = 3.5,
  fontface = "bold")
children <- children + theme_minimal() +
  theme(
    axis.title.y = element_blank())
print(children)

smoker <- ggplot(data = insurance_eda, aes(x = smoker)) +
  geom_bar(mapping = aes(x = smoker), fill="darkgrey", color = "white") +
  scale_y_continuous(labels = NULL)

smoker <- smoker + geom_text(
  aes(label = ..count..),
  stat = "count",
  vjust = -0.1,
  show.legend = FALSE,
  size = 8,
  fontface = "bold")
smoker <- smoker + theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20),  # Increase x-axis label font size
    #axis.text.y = element_text(size = 14),  # Increase y-axis label font size
    axis.title.x = element_text(size = 30))  # Increase x-axis title font size
print(smoker)
#ggsave("smoker_plot.pdf", smoker, width = 8, height = 6)

region <- ggplot(data = insurance_eda, aes(x = region)) +
  geom_bar(mapping = aes(x = region), fill="darkgrey", color = "white") +
  scale_y_continuous(labels = NULL)

region <- region + geom_text(
  aes(label = ..count..),
  stat = "count",
  vjust = -0.1,
  show.legend = FALSE,
  size = 3.5,
  fontface = "bold")
region <- region + theme_minimal() +
  theme(
    axis.title.y = element_blank())
print(region)

#EDA <- grid.arrange(age, sex, bmi, children, smoker, region, ncol = 3)
#print(EDA)
#ggsave("EDA_plots.pdf", EDA, width = 14, height = 8)

ggplot(data = insurance_eda, aes(x = charges)) +
  geom_histogram(binwidth = 1000, fill = "darkgrey", color = "white") +
  labs(title = "Histogram of charges", x = "charges") +
  theme_minimal()

mean_charges <- mean(insurance_eda$charges)
median_charges <- median(insurance_eda$charges)
charges <- ggplot(data = insurance_eda, aes(x = charges)) +
  geom_density(fill = "darkgrey", alpha = 0.5) +
  geom_vline(aes(xintercept = mean_charges), color = "red", linetype = "dashed", linewidth = 0.5) +
  geom_vline(aes(xintercept = median_charges), color = "blue", linetype = "dashed", linewidth = 0.5) +
  geom_text(aes(x = mean_charges, y = 0.000001, label = "Mean"), color = "red", size = 2, vjust = 0) +
  geom_text(aes(x = median_charges, y = 0.000001, label = "Median"), color = "blue", size = 2, vjust = 0) +
  labs(title = "Density Plot of Charges", x = "", y = "") +
  theme_minimal()
print(charges)

ggplot(data = insurance_eda, aes(x="", y=charges)) +
  geom_boxplot(color="black", fill="darkgrey") +
  ggtitle("Boxplot of Charges") +
  stat_summary(fun=mean, geom="point", shape=3, size=2, fill="white", color="red", stroke=1) +
  stat_summary(fun=median, geom="point", shape=3, size=2, fill="white", color="blue", stroke=1) +
  geom_text(aes(x = 1.5, y = mean_charges, label = "Mean"), color = "red", size = 3, vjust = 0) +
  geom_text(aes(x = 1.5, y = median_charges, label = "Median"), color = "blue", size = 3, vjust = 0) +
  theme_minimal()

# at a first glance, it is possible to state a couple of assumptions:
# - "age" is quite well uniformly distributed, apart from the categories at the beginning
#   and at the ending, namely "18-19" and "60+"
# - "sex" is fairly distributed across the two genders
# - "bmi" resembles a Gaussian Normal distribution
# - "children" is quite well heterogenenous, nearly an half of insurees does not 
#     have children; then the proportion of insurees for each category is 
#     inversely correlated with the n of children
# - "smoker" is quite unbalanced, with a large majority (nearly 80%) who is no-smoker
# - "region" seems to be evenly distributed among the 4 US areas (NE,SW,SE,NW), with
#   a slight majority in the SE part
# - charges, our dependent variable, shows non-normality, and a distribution which
#  has a positive skew on the right

# EDA on cross-variables, with respect to charges
ggplot(data = insurance_eda, aes(age, charges)) +
  geom_jitter(aes(color = age), alpha = 0.5) +
  theme_light()

ggplot(data = insurance_eda, aes(age, charges)) +
  geom_boxplot(aes(color = age), alpha = 0.5) +
  theme_light()

ggplot(data = insurance_eda, aes(bmi, charges)) +
  geom_boxplot(aes(color = children), alpha = 0.7) +
  theme_light()

ggplot(data = insurance_eda, aes(bmi, charges)) +
  geom_jitter(color = "darkgrey", alpha = 0.5) +
  theme_light()

bmi_charges <- ggplot(insurance_eda, aes(x = bmi, y = charges)) +
  geom_boxplot() +
  labs(
    x = "",
    y = "",
    title = "charges vs bmi"
  ) +
  theme_light() +
  theme(
    #axis.title = element_text(size = 18),      # Increase axis titles font size
    axis.text = element_text(size = 20),       # Increase axis labels font size
    plot.title = element_text(size = 30, hjust = 0.5)) # Increase plot title font size and center it
print(bmi_charges)
#ggsave("bmi_charges_plot.pdf", bmi_charges, width = 8, height = 6)

age_charges <- ggplot(insurance_eda, aes(x = age, y = charges)) +
  geom_boxplot() +
  labs(
    x = "",
    y = "",
    title = "charges vs age"
  ) +
  theme_light() +
  theme(
    #axis.title = element_text(size = 18),      # Increase axis titles font size
    axis.text = element_text(size = 20),       # Increase axis labels font size
    plot.title = element_text(size = 30, hjust = 0.5)) # Increase plot title font size and center it
print(age_charges)
#ggsave("age_charges_plot.pdf", age_charges, width = 8, height = 6)

smoker_charges <- ggplot(insurance_eda, aes(x = smoker, y = charges)) +
  geom_boxplot() +
  labs(
    x = "",
    y = "",
    title = "charges vs smoker"
  ) +
  theme_light() +
  theme(
    #axis.title = element_text(size = 18),      # Increase axis titles font size
    axis.text = element_text(size = 20),       # Increase axis labels font size
    plot.title = element_text(size = 30, hjust = 0.5)) # Increase plot title font size and center it
print(smoker_charges)
#ggsave("smoker_charges_plot.pdf", smoker_charges, width = 8, height = 6)

ggplot(data = insurance_eda, aes(sex, charges)) +
  geom_jitter(aes(color = sex), alpha = 0.7) +
  theme_light()

ggplot(insurance_eda, aes(x = charges, fill = sex)) + 
  geom_density(alpha = 0.5)

ggplot(data = insurance_eda, aes(children, charges)) +
  geom_jitter(aes(color = children), alpha = 0.7) +
  theme_light()

ggplot(data = insurance_eda, aes(children, charges)) +
  geom_boxplot(aes(color = children), alpha = 0.7) +
  theme_light()

ggplot(data = insurance_eda, aes(smoker, charges)) +
  geom_jitter(aes(color = smoker), alpha = 0.5) +
  theme_light()

ggplot(data = insurance_eda, aes(region, charges)) +
  geom_jitter(aes(color = region), alpha = 0.7) +
  theme_light()

ggplot(data = insurance_eda, aes(region, charges)) +
  geom_boxplot(aes(color = region), alpha = 0.7) +
  theme_light()

###

plot_data <- data.frame(age_class = factor(insurance_eda$age, levels = c("Under 20", "21-30", "31-40", "41-50", "51-60", "60+")),
                        charges = insurance_eda$charges)

class_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33")

ggplot(plot_data, aes(x = charges, fill = age_class)) +
  geom_density(alpha = 0.5) +
  labs(title = "Conditional Distribution of Charges by Age Class",
       x = "Charges",
       y = "Density") +
  facet_wrap(~ age_class, ncol = 2) +
  scale_fill_manual(values = class_colors)

# COMMENT:
#  -there is a positive and increasing trend between age and charges
#  -there is an avg positive and increasing trend between children and charges
#  -there is a sparse relationship between bmi and charges
#  -there is a quite different relationship between smokers/nonsmokers and charges, as expected
#  -there is a uniform relationship between sex/region and charges

# Having performed EDA on variables, it seems quite convenient to try transforming
#  the dependent variable "charges" in logarithm of "charges". The transformation
#  could help making the relationship more linear, by addressing skewness and 
#  stabilizing variances.

# TRANSFORMING THE DEPENDENT (LOG)
insurance_eda$charges <- log(insurance_eda$charges)

ggplot(data = insurance_eda, aes(x = charges)) +
  geom_histogram(binwidth = 0.05, fill = "darkgrey", color = "black") +
  labs(title = "Histogram of log(charges)", x = "Log(charges)") +
  theme_minimal()

ggplot(data = insurance_eda, aes(x = charges)) +
  geom_density(fill = "darkgrey", alpha = 0.50) +
  labs(title = "Density Plot of log(charges)", x = "Log(charges)") +
  theme_minimal()

# the transformation does not help so much: the dependent variable maintains a 
#  non-normal distribution. however, we will try it in OLS to check how it performs.

# EDA on correlation
par(mfrow=c(1,1))
corrplot(cor(insurancenum), type = "upper")
cor(insurancenum)
pairs.panels(insurancenum)

# CORRELATION PLOT points out the positive correlation of charges with age, bmi, smoker.
#  It also highlights the slight positive correlation of charges with sex and children.

# 5 - TRAIN SET & TEST SET ----
set.seed(1)
n <- nrow(insurance_df)
train <- sample(1:n, n*0.7)
insurance.is <- insurance_df[train,]
insurance.oos <- insurance_df[-train,]
n.is <- nrow(insurance.is)
n.oos <- nrow(insurance.oos)

# 6a - LINEAR REGRESSION: IN SAMPLE & DIAGNOSTICS ----
#MODEL0 - OLS - baseline model with all variables
model0 <- lm(charges ~ ., data = insurance.is)
summary(model0)
vif(model0)
plot(model0, which=1) #Residuals vs Fitted values plot
plot(model0, which=2) #Q-Q plot: non-normality
plot(model0, which=3) #Scale-Location plot
plot(density(model0$residuals))

# The baseline model is not quite well performing: residuals are not normally
#  distributed and this is clearly confirmed by diagnostic plots. 
# Let's try to create a model where we use variables which shows the highest and
#  most significant correlation.


#MODEL1 - OLS - model with significant variables + interaction term
model1 <- lm(charges ~ age+bmi+smoker:bmi, data = insurance.is)
summary(model1)
vif(model1, type='predictor')
plot(model1, which=1) #Residuals vs Fitted values plot
plot(model1, which=2) #Q-Q plot: non-normality
plot(model1, which=3) #Scale-Location plot
plot(density(model1$residuals))

# This model seems to lead to an improvement. The VIF shows multicollinearity if we
#  include both the interaction term smoker:bmi and smoker. Therefore, we only 
#  include the variable smoker through the interaction term. However, the QQ-Plot
#  still shows non-normality. The residuals' distribution is improved but still
#  does not resemble a normal distribution.

# To start: generating simulations of normality
residual_range <- range(model1$residuals)
sim_values <- seq(residual_range[1], residual_range[2], by = 0.1)
lines(sim_values, dnorm(sim_values, mean = mean(model1$residuals), sd = sd(model1$residuals)), col = 2, lwd = 2)

# Try: generating simulations of student-t
est.t <- fitdistr(model1$residuals, densfun = "t")
est.t

# Location-scale distribution
my.dt <- function(x, a, b, df){
  out <- (1/b)*dt((x-a)/b, df = df)
  return(out)
}

plot(density(model1$residuals))
lines(seq(-20000,30000,by=1), my.dt(x = seq(-20000,30000,by=1), 
                                    a = est.t$estimate[1],
                                    b= est.t$estimate[2],
                                    df = est.t$estimate[3]), col=4, lwd=2)

# Via simulations, as expected, we can again confirm that residuals are not normally 
#  distributed; the student t seemed at first a good approximation but the resulting
#  df ≈ 1 shows that using student t is meaningless.

# Let's try to apply the log-transformation to the dependent variable 'charges'.

#MODEL2 - OLS - baseline model with log(charges)
model2 <- lm(log(charges) ~ age+bmi+smoker:bmi, data = insurance.is)
summary(model2)
vif(model2, type='predictor')
plot(model2, which=1) #Residuals vs Fitted values plot
plot(model2, which=2) #Q-Q plot: non-normality
plot(model2, which=3) #Scale-Location plot
plot(density(model2$residuals))

# It does not seem to lead to a great improvement. The residuals' distribution
#  seems worsened and it is not normal.

# Let's have a look at outliers to model1:

# OUTLIER detection
plot(predict(model1), rstudent(model1)) #studentized res
abline(h=0, col="red")
which(rstudent(model1)>3)
which(rstudent(model1)<(-3))

# We can state that there are several outliers affecting the relationship,
#  instead of removing them, we decided to try using robust linear regression

#MODEL3 - ROBUST linear regression
model3 <- rlm(charges ~ age+bmi+smoker:bmi, data = insurance.is, method = "M", maxit = 100)
summary(model3)
plot(model3, which=1) #Residuals vs Fitted values plot
plot(model3, which=2) #Q-Q plot: non-normality in the upper tails
plot(model3, which=3) #Scale-Location plot
plot(density(model3$residuals))

# It seems no great improvement by comparing RLM and LM.

# MODEL EVALUATION: in-sample of the best models
mse.is.0 <- mean(residuals(model0)^2)
ser.is.0 <- sqrt((n.is/(n.is-length(coef(model0))))*mse.is.0)
aic.is.0 <- exp(2*length(coef(model0))/n.is)*mse.is.0
bic.is.0 <- n.is^(length(coef(model0)/n.is))*mse.is.0

mse.is.1 <- mean(residuals(model1)^2)
ser.is.1 <- sqrt((n.is/(n.is-length(coef(model1))))*mse.is.1)
aic.is.1 <- exp(2*length(coef(model1))/n.is)*mse.is.1
bic.is.1 <- n.is^(length(coef(model1)/n.is))*mse.is.1

# 6b - LINEAR REGRESSION: PREDICTION ----
# MODEL PREDICTIONS: out-of-sample of the best models
predict0 <- predict(model0, newdata=insurance.oos, type="response")

mse.oos.0 <- mean((insurance.oos$charges-predict(model0,insurance.oos))^2)
ser.oos.0 <- sqrt((n.oos/(n.oos-length(coef(model0))))*mse.oos.0)
aic.oos.0 <- exp(2*length(coef(model0))/n.oos)*mse.oos.0
bic.oos.0 <- n.oos^(length(coef(model0)/n.oos))*mse.oos.0

predict1 <- predict(model1, newdata=insurance.oos, type="response")

mse.oos.1 <- mean((insurance.oos$charges-(predict(model1,insurance.oos)))^2)
ser.oos.1 <- sqrt((n.oos/(n.oos-length(coef(model1))))*mse.oos.1)
aic.oos.1 <- exp(2*length(coef(model1))/n.oos)*mse.oos.1
bic.oos.1 <- n.oos^(length(coef(model1)/n.oos))*mse.oos.1

#predictive accuracy evaluation
predictive_accuracy <- cbind(rbind(c(mse.is.0,mse.is.1),
                                   c(ser.is.0,ser.is.1),
                                   c(aic.is.0,aic.is.1),
                                   c(bic.is.0,bic.is.1)),
                             rbind(c(mse.oos.0,mse.oos.1),
                                   c(ser.oos.0,ser.oos.1),
                                   c(aic.oos.0,aic.oos.1),
                                   c(bic.oos.0,bic.oos.1)))
print(predictive_accuracy)

#MSE MODEL 0: 44636607
#MSE MODEL 1: 34440438

# By comparing those metrics, OLS with variable selection provide the best MSE.
# However, it might not be optimal. This situation may be due to the non-normality 
#  distribution of response variable and the violation of assumptions on residuals. 
# We then decided to implement some other techniques beyond OLS, to improve estimation
#  and prediction accuracy:

# BEST SUBSET SELECTION
# RIDGE REGRESSION
# REGRESSION TREES AND RANDOM FORESTS

# 7 - LINEAR REGRESSION: BEST SUBSET SELECTION ----
# BEST SUBSET SELECTION performs subset selection process for linear regression models, 
#  by calculating validation errors for different model sizes, and identifying the 
#  model size with the lowest validation error.

regfit.full<-regsubsets(charges~.,insurance.is)
summary(regfit.full)
regfit.full<-regsubsets(charges~.,data=insurance.is,nvmax=12)
reg.summary=summary(regfit.full)
names(reg.summary) #we have: rsq, rss, adjr2, cp, bic

par(mfrow=c(1,1))
plot(reg.summary$rsq,xlab="Number of Variables",ylab="RSS",type="l")
which.max(reg.summary$rsq)
points(12,reg.summary$rsq[12], col="red",cex=2,pch=20)

plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
which.min(reg.summary$rss)
points(12,reg.summary$rss[12], col="red",cex=2,pch=20)

plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(7,reg.summary$adjr2[7], col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(6,reg.summary$cp[6],col="red",cex=2,pch=20)

which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(3,reg.summary$bic[3],col="red",cex=2,pch=20)

plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,12)
coef(regfit.full,7)
coef(regfit.full,6)
coef(regfit.full,3)

# the BIC shows that the most relevant variables should be age, bmi, smoker 
#  (hence, confirming our model estimation approach)
# then, the CP adds also the importance of children2-4 
# then, the ADJR2 highlights the relevance of regionSE

# Model selection

set.seed(1)
regfit.best=regsubsets(charges~.,data=insurance.is,nvmax=19)
test.mat=model.matrix(charges~.,data=insurance.oos)
val.errors=rep(NA,12)
for(i in 1:12){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((insurance.oos$charges-pred)^2)
}
val.errors
which.min(val.errors) #MSE: 44596490
coef(regfit.best,9)

# BSS method reports that the best model is with 9 variables, showing that 
#  sex, children1, children5 are not so relevant.
# However, even though MSE slightly improved with respect to OLS with all variables,
#  it is not a relevant improvement with respect to our model1.

# 8 - LINEAR REGRESSION: RIDGE REGRESSION ----
set.seed(1)

n <- nrow(insurancenum)
train <- sample(1:n, n*0.7)
insurancenum.is <- insurancenum[train,]
insurancenum.oos <- insurancenum[-train,]
n.is <- nrow(insurancenum.is)
n.oos <- nrow(insurancenum.oos)

X_train <- as.matrix(insurancenum.is[,])
X_test <- as.matrix(insurancenum.oos[,])

y_train <- insurancenum.is$charges
y_test <- insurancenum.oos$charges

class(X_train)
class(y_train)

cv.out=cv.glmnet(X_train, y_train, alpha=0)
plot(cv.out)
cv.out$lambda.min
bestlam=cv.out$lambda.min
bestlam
#lambda.min = best lambda, which results in lowest CV error: 1190.984

ridge_model <- cv.glmnet(X_train, y_train, alpha = 0)  # alpha = 0 for ridge
predictions <- predict(ridge_model, s = "lambda.min", newx = X_test)

mse <- mean((predictions - y_test)^2)
cat("Mean Squared Error (MSE):", mse, "\n") #MSE: 2844860
mae <- sqrt(mse)
cat("Mean Absolute Error (MAE):", mae, "\n") #MAE: 1686.671

# RIDGE regression provides the best MSE till now.

# 9 - REGRESSION TREES AND RANDOM FORESTS ----
# A regression tree is a non-parametric predictive modeling technique used for 
#  solving regression problems. Regression trees predict continuous numerical values.
# Random Forest is primarily a machine learning algorithm used for classification 
#  and regression tasks. It's a powerful ensemble method that combines multiple 
#  decision trees to create a more robust and accurate model. Its strength lies 
#  in handling non-linear relationships and complex interactions in the data.

# Regression Trees
set.seed(1)
tree.ins=tree(charges~.,insurancenum.is)
summary(tree.ins) #"smoker", "age", "bmi" are the 3 core variables
plot(tree.ins)
text(tree.ins,pretty=0)

cv.ins=cv.tree(tree.ins) #check for CV to see whether we need to prune
plot(cv.ins$size,cv.ins$dev,type='b') 
#confirming 5 as the best n of terminal nodes: no need for pruning

#prune.ins <- prune.tree(tree.ins, best=5)
#plot(prune.ins)
#text(prune.ins,pretty=0)

yhat=predict(tree.ins,newdata=insurancenum.oos) #using RT to make predictions
ins.test=insurancenum.oos[,"charges"]
plot(yhat,ins.test)
abline(0,1)
mean((yhat-ins.test)^2) #MSE=32421321

# RT MSE is only slightly better than OLS, but worse than ridge.
# Let's try to implement random forests.

# Random Forests: Bagging
set.seed(1)
bag.ins=randomForest(charges~.,data=insurancenum.is,mtry=6,importance=TRUE)
bag.ins
#Bagging is a special case of random forests, when m=p; In order to perform bagging 
# we must specify mtry=6, so that all the predictors should be considered for 
# each split of the tree.

yhat.bag = predict(bag.ins,newdata=insurancenum.oos)
plot(yhat.bag, ins.test)
abline(0,1)
mean((yhat.bag-ins.test)^2) #MSE=29939582 (almost equal to RT)

# Random Forests
# usually mtry = √p or p/3
# however, since our dataset is quite small and we only have 6 predictors,
#  we went by trial and error; we found out that the n of predictors that
#  yield to the best performance is mtry = 3
set.seed(1)
rf.ins=randomForest(charges~.,data=insurancenum.is,mtry=3)
yhat.rf = predict(rf.ins,newdata=insurancenum.oos)
mean((yhat.rf-ins.test)^2) #MSE=28884742 (slight improvement)

rf.ins=randomForest(charges~.,data=insurancenum.is,mtry=3,importance=TRUE)
yhat.rf = predict(rf.ins,newdata=insurancenum.oos)
mean((yhat.rf-ins.test)^2) #MSE=28775287
importance(rf.ins)
varImpPlot(rf.ins) #smoker, age, bmi are confirmed the most important variables

# Boosting
set.seed(1)
boost.ins=gbm(charges~.,data=insurancenum.is,distribution="gaussian")
yhat.boost=predict(boost.ins,newdata=insurancenum.oos,n.trees = 100)
mean((yhat.boost-ins.test)^2) #MSE=43916750

# GridSearch for Boosting
set.seed(1)
ctrl <- trainControl(method = "cv", number = 10)

parameter_grid <- expand.grid(
  n.trees = c(100, 200, 500),     # N trees
  interaction.depth = c(3, 4, 5), # Interaction depth: maximum depth for a tree
  shrinkage = c(0.01, 0.1, 0.2),  # Shrinkage parameter λ
  n.minobsinnode = c(10, 20, 30)  # Min n of obs in a terminal node
)

boost_grid <- train(charges ~ ., data = insurancenum.is, method = "gbm", trControl = ctrl,
  tuneGrid = parameter_grid, verbose = FALSE)

best_bmodel <- boost_grid$finalModel
best_param <- boost_grid$bestTune
#n.trees: 500; interaction.depth: 5; shrinkage: 0.01; n.minobsinnode: 20;

boost_predictions <- predict(best_bmodel, newdata = insurancenum.oos, n.trees = best_param$n.trees)
mean((boost_predictions - insurancenum.oos$charges)^2) #MSE: 27081253
summary(best_bmodel) #relative-influence plots & stats
par(mfrow=c(1,2))
plot(boost.ins,i="age") #partial-dependence plot for age
plot(boost.ins,i="smoker") #partial-dependence plot for smoker
plot(boost.ins,i="bmi") #partial-dependence plot for bmi

# Both RF and Boosting provide an improved MSE; however, still worse than Ridge.

# 10a - MEDIAN/QUANTILE REGRESSION () ----
# Median regression is a particular case of quantile regression with tau=0.50.
# This method focuses on estimating the median of the conditional distribution 
#  of the response variable given the predictor variables.
# In more general terms, the median is a measure of central tendency that is 
#  less influenced by outliers than the mean. Since our "charges" data presents 
#  a skewed distribution, non-normality and contains many extreme values (outliers), 
#  the median is less sensitive to them, being more appropriate and robust than the mean.

# 10b - MEDIAN REGRESSION - QUANTILE REGRESSION for q=0.50 ----
# In-sample fit
med       <- 0.50
fit.lr   <- lm(charges ~ age+bmi+smoker:bmi, data = insurancenum.is)
residuals(fit.lr)
plot(density(fit.lr$residuals))
fit.qrmed <- rq(charges ~ age+bmi+smoker:bmi, tau = med, data = insurancenum.is, method = "fn")

# Computing standardized residuals adjusted to q=0.50
residuals(fit.lr)
residuals_standardized <- (residuals(fit.lr) - mean(residuals(fit.lr))) / sd(residuals(fit.lr))
quant_res <- quantile(residuals_standardized, med)
summary(quant_res)

# Out-of-sample predictions
qmed.lr <- predict(fit.lr, insurancenum.oos) + sigma(fit.lr)*quant_res
qmed.qr <- predict(fit.qrmed, insurancenum.oos)
summary(qmed.lr)

# Coverage evaluation - likelihood test
# "LIKELIHOOD RATIO TEST" is used to compare the likelihood of 2 alternatives:

#   H0: mean(I)=0.50 (quantiles predicted are effectively the right quantile)
#   H1: mean(I)≠0.50

#Firstly, we compute binary indicators for whether the observed charges are less 
# than the predicted charges from linear regression and quantile regression, respectively.
i.qmed.lr <- 1*(insurancenum.oos$charges < qmed.lr)
i.qmed.qr <- 1*(insurancenum.oos$charges < qmed.qr)

#Then, we compute proportions of undercoverage (when the observed charges are less 
# than the predicted charges) for linear regression and quantile regression, respectively.
uc.qmed.lr <- mean(i.qmed.lr) #0.4701493 --> 47.0% of cases where LR predicted charges < actual charges
uc.qmed.qr <- mean(i.qmed.qr) #0.4975124 --> 49.8% of cases where MR predicted charges < actual charges

uc.qmed.lr #0.4701493 #LR model is overestimating charges at q=0.50
uc.qmed.qr #0.4975124 #MR model is slightly underestimating charges at q=0.50

#Finally, we compute p-values using the chi-squared distribution and likelihood 
# ratio test statistics. These p-values indicate the significance of the test.
pv.qmed.lr <- 1-pchisq(-2*(sum((i.qmed.lr)*log(med)+(1-i.qmed.lr)*log(0.50))-sum((i.qmed.lr)*log(uc.qmed.lr)+(1-i.qmed.lr)*log(1-uc.qmed.lr))), 1)
pv.qmed.qr <- 1-pchisq(-2*(sum((i.qmed.qr)*log(med)+(1-i.qmed.qr)*log(0.50))-sum((i.qmed.qr)*log(uc.qmed.qr)+(1-i.qmed.qr)*log(1-uc.qmed.qr))), 1)

pv.qmed.lr #0.2311635 #accepting the null hypothesis at alpha=0.05
pv.qmed.qr #0.9205419 #accepting the null hypothesis at alpha=0.05

cbind(rbind(uc.qmed.lr,uc.qmed.qr),
      rbind(pv.qmed.lr,pv.qmed.qr))

#Both LR-MR performs well in predicting the median q=0.50. If we have a closer look,
# even though they both lead to the acceptance of the null hypothesis,
# on average MR seems to performs slightly better than LR and we prefer MR.

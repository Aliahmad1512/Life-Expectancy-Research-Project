########################### Life Expectancy ################################
############################## R-PROJECT ###################################


# 1) Identify the Problem Statement - what are you trying to solve?

## In this casestudy, we are provided average life expectancy of people of 193 Countries.
## We have to predict next year value using linear regression.

# 2) Import the dataset
# Here, the target variable is "Life_Expectancy" which is Continuous in nature.

life <- read.csv(choose.files())

life

# Basic EDA

head(life)
class(life)
names(life)
View(life)

dim(life)
# dim() function returns Total dimension i.e. 
# both the number of rows and column in a dataframe.

nrow(life)
ncol(life)
# We can also use ncol() function to find the number of columns
# and nrow() to find the number of rows separately.

summary(life)

library(psych)      ### stats package - "psych" ####

describe(life)
# This function provides more deep dive statistics including
# standard deviation, mean absolute deviation, skew, etc

# Renaming columns 
names(life)[names(life) == "Under.five_Deaths"] <- "Under_five_Deaths"
names(life)[names(life) == "HIV.AIDS"] <- "HIV_AIDS"
names(life)[names(life) == "Thinness_5.9_Years"] <- "Thinness_5_9_Years"
names(life)[names(life) == "Thinness_1.19_Years"] <- "Thinness_1_19_Years"

# Now again looking at the column names
colnames(life)

# 3) Identifying the type of variables:-
## Categorical - "Country" and "Status" are not in factor variable,so we need to change.

## Numerical - "LifeExpectancy","AdultMortality","InfantDeaths","Alcohol","PercentageExpenditure","HepatitisB",
# "Measles","BMI","Under-fiveDeaths","Polio","TotalExpenditure","Diphtheria","HIV-AIDS","GDP","PerCapitaGDP",
# "Population","Thinness1-19Years","Thinness5-9Years","IncomeCompositionofResources","Schooling"

# Structure check of the variables
str(life)

length(unique(life$Country))
#can't help in prediction too many unique values

# Remove useless columns
life <- life[,-c(1,17)]

# 4) Data pre-processing:

# find the missing value by using visualization
install.packages("Amelia")
library(Amelia)

missmap(life, main="Life Expectancy - Finding Missing Data",
        col=c("red", "black"), legend = F)

# checking for missing values
colSums(is.na(life))
colSums(is.na(life)) / nrow(life)

#Missing values imputation by median as these columns have many outliers.
boxplot(life$Life_Expectancy)
median(life$Life_Expectancy, na.rm = T)
life$Life_Expectancy[which(is.na(life$Life_Expectancy))] <- 72.1


boxplot(life$Adult_Mortality)
median(life$Adult_Mortality, na.rm = T)
life$Adult_Mortality[which(is.na(life$Adult_Mortality))] <- 144

boxplot(life$Hepatitis_B) 
median(life$Hepatitis_B, na.rm = T)
life$Hepatitis_B[which(is.na(life$Hepatitis_B))] <- 92

boxplot(life$Polio)
median(life$Polio, na.rm = T)
life$Polio[which(is.na(life$Polio))] <- 93


boxplot(life$Total_Expenditure) 
median(life$Total_Expenditure, na.rm = T)
life$Total_Expenditure[which(is.na(life$Total_Expenditure))] <- 5.755

boxplot(life$Diphtheria) 
median(life$Diphtheria, na.rm = T)
life$Diphtheria[which(is.na(life$Diphtheria))] <- 93

boxplot(life$Per_Capita_GDP) 
median(life$Per_Capita_GDP, na.rm = T)
life$Per_Capita_GDP[which(is.na(life$Per_Capita_GDP))] <- 3738.52

boxplot(life$Population)  
median(life$Population, na.rm = T)
life$Population[which(is.na(life$Population))] <- 8171061

boxplot(life$Thinness_1_19_Years) 
median(life$Thinness_1_19_Years, na.rm = T)
life$Thinness_1_19_Years[which(is.na(life$Thinness_1_19_Years))] <- 3.3

boxplot(life$Thinness_5_9_Years) 
median(life$Thinness_5_9_Years, na.rm = T)
life$Thinness_5_9_Years[which(is.na(life$Thinness_5_9_Years))] <- 3.3

boxplot(life$Schooling) 
median(life$Schooling, na.rm = T)
life$Schooling[which(is.na(life$Schooling))] <- 12.3

#Missing values imputation by mean as these columns don't have any outlier.
boxplot(life$BMI) 
mean(life$BMI, na.rm = T)
life$BMI[which(is.na(life$BMI))] <- 38.32125

boxplot(life$Alcohol)
mean(life$Alcohol, na.rm = T)
life$Alcohol[which(is.na(life$Alcohol))] <- 4.602861

boxplot(life$Income_Composition_of_Resources) 
mean(life$Income_Composition_of_Resources, na.rm = T)
life$Income_Composition_of_Resources[which(is.na(life$Income_Composition_of_Resources))] <- 0.6275511

#now checking again for missing values
colSums(is.na(life))

# Checking for the presence of outliers in variables using boxplot.

boxplot(life$Life_Expectancy) 
quantile(life$Life_Expectancy, seq(0,1,0.01))
life$Life_Expectancy <- ifelse(life$Life_Expectancy<45,45, life$Life_Expectancy)
boxplot(life$Life_Expectancy)

boxplot(life$Adult_Mortality) 
quantile(life$Adult_Mortality, seq(0,1,0.01))
life$Adult_Mortality <- ifelse(life$Adult_Mortality>430,430, life$Adult_Mortality)
boxplot(life$Adult_Mortality)

boxplot(life$Hepatitis_B) 
quantile(life$Hepatitis_B, seq(0,1,0.01))
life$Hepatitis_B <- ifelse(life$Hepatitis_B<62,62, life$Hepatitis_B)
boxplot(life$Hepatitis_B)

boxplot(life$Polio) 
quantile(life$Polio, seq(0,1,0.01))
life$Polio <- ifelse(life$Polio<50,50, life$Polio)
boxplot(life$Polio)

boxplot(life$Total_Expenditure) 
quantile(life$Total_Expenditure, seq(0,1,0.01))
life$Total_Expenditure <- ifelse(life$Total_Expenditure>11,11, life$Total_Expenditure)
boxplot(life$Total_Expenditure)

boxplot(life$Diphtheria) 
quantile(life$Diphtheria, seq(0,1,0.01))
life$Diphtheria <- ifelse(life$Diphtheria<50,50, life$Diphtheria)
boxplot(life$Diphtheria)

boxplot(life$Per_Capita_GDP) 
quantile(life$Per_Capita_GDP, seq(0,1,0.01))
life$Per_Capita_GDP <- ifelse(life$Per_Capita_GDP>29000,29000, life$Per_Capita_GDP)
boxplot(life$Per_Capita_GDP)

boxplot(life$Population) 
quantile(life$Population, seq(0,1,0.01))
life$Population <- ifelse(life$Population>58000000,58000000, life$Population)
boxplot(life$Population)

boxplot(life$Thinness_1_19_Years) 
quantile(life$Thinness_1_19_Years, seq(0,1,0.01))
life$Thinness_1_19_Years <- ifelse(life$Thinness_1_19_Years>15,15, life$Thinness_1_19_Years)
boxplot(life$Thinness_1_19_Years)

boxplot(life$Thinness_5_9_Years) 
quantile(life$Thinness_5_9_Years, seq(0,1,0.01))
life$Thinness_5_9_Years <- ifelse(life$Thinness_5_9_Years>15,15, life$Thinness_5_9_Years)
boxplot(life$Thinness_5_9_Years)

boxplot(life$Schooling) 
quantile(life$Schooling, seq(0,1,0.01))
life$Schooling <- ifelse(life$Schooling>19,19, life$Schooling)
life$Schooling <- ifelse(life$Schooling<5,5, life$Schooling)
boxplot(life$Schooling)

# Outlier Treatment done 

# 5) Data visualizations : Univariate analysis & Bivariate analysis

################## Univariate Analysis ##################

# Multiple Continuous Variables
ColsForHist <- c("Life_Expectancy","Year","Adult_Mortality","Infant_Deaths",
                 "Alcohol","Percentage_Expenditure","Hepatitis_B","Measles",
                 "BMI","Under_five_Deaths","Polio","Total_Expenditure","Diphtheria",
                 "HIV_AIDS","Per_Capita_GDP","Population","Thinness_1_19_Years",
                 "Thinness_5_9_Years","Income_Composition_of_Resources","Schooling")
  
# library for colors
library(RColorBrewer)

par(mfrow=c(2,2))

# Using loops to create the histograms for each column
for (ColumnName in ColsForHist){
  hist(life[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))}

# Multiple Categorical Variables
ColsForBar <- c("Status")

# looping to create the Bar-Plots for each column
for (ColumnName in ColsForBar){
  barplot(table(life[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Spectral"))}

#################### Bivariate Analysis ########################

# Relationship between target variable and predictors variables
# Categorical vs Continuous ---  Box Plot
# Continuous Vs Continuous  ---  Scatter plot

# Categorical vs Continuous analysis-- Boxplot

par(mfrow=c(2,2))

ColsForBar <-c("Status")

for(box_cols in ColsForBar){
  boxplot(Life_Expectancy~life[  ,c(box_cols)], data=life  , main=paste('Boxplot of :',box_cols),col=brewer.pal(8,"Accent"))
}

# Continuous Vs Continuous --- Scatter plot
ContinuousCols<-c("Year","Life_Expectancy","Adult_Mortality","Infant_Deaths",
                 "Alcohol","Percentage_Expenditure","Hepatitis_B","Measles",
                 "BMI","Under_five_Deaths","Polio","Total_Expenditure","Diphtheria",
                 "HIV_AIDS","Per_Capita_GDP","Population","Thinness_1_19_Years",
                 "Thinness_5_9_Years","Income_Composition_of_Resources","Schooling")

par(mfrow=c(1,1))
plot(life[,ContinuousCols],col='blue')

### Statistical Tests ###
# Strength of Relationship between predictor and target variable
# Continuous Vs Continuous ---- Correlation test
# Continuous Vs Categorical---- ANOVA test

###################### ANOVA TEST ##############################
# Continuous vs Categorical relationship strength: ANOVA
# Analysis of Variance(ANOVA)

# Small P-Value < 5% - Variables are correlated 
#### (Null hypothesis H0 is rejected) #########
# Large P-Value > 5% - Variables are not correlated 
#### (Null hypothesis H0 is accepted) ##########

colsforanova<-c("Status") 

for(anovacols in colsforanova){
  anovaresult=summary(aov(Life_Expectancy ~ life[,c(anovacols)],data=life))
  print(colsforanova)
  print(anovaresult)}

## Good predictor variables - "Status"-- low p-value
## so we reject the null hypothesis.


###################### Correlation test ########################
# Continuous Vs Continuous ---- Correlation test

# Correlation using Heatmap visualization
library(corrplot)
corrplot(cor(life[,3:21]))

ContinuousCols <- c("Life_Expectancy","Adult_Mortality","Infant_Deaths",
                 "Alcohol","Percentage_Expenditure","Hepatitis_B","Measles",
                 "BMI","Under_five_Deaths","Polio","Total_Expenditure","Diphtheria",
                 "HIV_AIDS","Per_Capita_GDP","Population","Thinness_1_19_Years",
                 "Thinness_5_9_Years","Income_Composition_of_Resources","Schooling")

CorrData <- cor(life[,ContinuousCols ], use = "complete.obs")
CorrData

# Final columns which has high correlation with the target variable
names(CorrData['Life_Expectancy',])

names(CorrData[,'Life_Expectancy'][abs(CorrData[,'Life_Expectancy'])>0.3])

#"Adult_Mortality","Alcohol","Percentage_Expenditure","BMI",
#"Polio","Diphtheria","HIV_AIDS","Per_Capita_GDP",
#"Thinness_1_19_Years","Thinness_5_9_Years",
#"Income_Composition_of_Resources","Schooling" --- are good variables.                      

#6) Feature selection for model:

##Potential predictors are ---
#"Status","Adult_Mortality","Alcohol","Percentage_Expenditure","BMI",
#"Polio","Diphtheria","HIV_AIDS","Per_Capita_GDP","Thinness_1_19_Years","Thinness_5_9_Years",
#"Income_Composition_of_Resources","Schooling"

##### Encoding concept #####
#there is "Status" for which we have to do encoding as there are char variable.

life$Status <- as.factor(life$Status)

str(life)

# 7) Splitting the data into train & test

install.packages("caTools") 
library(caTools)

set.seed(150)
split <- sample.split(life$Life_Expectancy, SplitRatio = 0.80)
split

table(split)

training <- subset(life, split==TRUE)
nrow(training)
testing <- subset(life, split==FALSE)
nrow(testing)

############################################################

# 8) Model Building: 
# Building the Linear Regression model with training dataset
# linear regression model function : lm(dv~., data=training)

lin_regession <- lm(Life_Expectancy~., data=training)
lin_regession
summary(lin_regession)

# there are some variable which is not statically significant, 
# hence, we have to remove this.

lin_regession1 <- lm(Life_Expectancy~.-Status-Alcohol-Percentage_Expenditure-Measles-Thinness_1_19_Years-Thinness_5_9_Years, data=training)
lin_regession1
summary(lin_regession1)

###############################
# Multiple R-squared: 0.8507	
# Adjusted R-squared: 0.8499 
###############################

# The fitness of model is good

### The Significant variables are ---
#"Year","Adult_Mortality","Infant_Deaths" "Alcohol","BMI","Hepatitis_B",
#"Under_five_Deaths","Polio","Total_Expenditure","Diphtheria","HIV_AIDS",
#"Per_Capita_GDP","Population","Income_Composition_of_Resources","Schooling"                        

###########################################################################

#9) Predictions : predict model by using test dataset.

linear_pred <- predict(lin_regession1, newdata = testing)
linear_pred 

# Combined actual Life Expectancy and the predicted Life Expectancy

linear_pred_cbind <- cbind(testing$Life_Expectancy, linear_pred)
linear_pred_cbind

#10) Assumption Tests of Linear Regression model --

#### Autocorrelation ####
#we have to do durbin Watson test, if value falls between 0 and 4,
# there is no autocorrelation, any value which is less then 0 or more than 4,
# will consider autocorrelation

install.packages("lmtest")
library(lmtest)

install.packages("faraway")
library(faraway)

install.packages("car")
library(car)

dwtest(lin_regession1)
# DW = 0.68973 - hence, no Autocorrelation found

#### Multicollinearity check: means idv does influence another idv variable ####
# approach - Variance Inflation Factor (vif) = 1/(1-R^2)
# If vif value is equal to or less then 5, then we can consider there is no multicollinearity, 
# if vif value is more than 5, will consider multicollinearity.

vif(lin_regession1)

# there is multicollinearity found in "Infant_Deaths" and "Under_five_Deaths"
# value - Infant_Deaths : 137.968206 and Under_five_Deaths : 139.262842
# "Under_five_Deaths" has more value, hence we will remove this one

lin_regession2 <- lm(Life_Expectancy~.-Status-Alcohol-Percentage_Expenditure-Measles-Thinness_1_19_Years-Thinness_5_9_Years-Under_five_Deaths, data=training)
lin_regession2
summary(lin_regession2)

vif(lin_regession2)
# Hence, now there is no multicollinearity

###############################
# Multiple R-squared:  0.843	
# Adjusted R-squared:  0.8421 
###############################

# 11) MAPE and MDAPE:

# Mean absolute percentage error (MAPE)

library(MLmetrics)

MeanAPE <- mean(abs((testing$Life_Expectancy-linear_pred)/testing$Life_Expectancy)) * 100
MeanAPE

# Median absolute percentage error (MDAPE)

MedianAPE <- median(abs((testing$Life_Expectancy-linear_pred)/testing$Life_Expectancy)) * 100
MedianAPE

print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))

#######################################################
# Multiple R-squared:  0.843	
# Adjusted R-squared:  0.8421 
# Mean Absolute Percentage Error (MAPE) : 4.256897 
# Median Absolute Percentage Error (MDAPE) : 2.935505
# Mean Accuracy of Linear Regression Model :  95.74
# Median Accuracy of Linear Regression Model :  97.06
#######################################################

###################################################BUSINESS RECOMMENDATIONS#########################################

#The Developed countries should help developing countries in eradicating the diseases which are affecting the life of the people by providing vaccinations
#The government of developing countries should launch various schemes to motivate people to send their kids to schools
#Government should organize free healthcare camps to provide free vaccinations for the needy and poor people so that they 
#don't have to spend their money and they also stay healthy to treat their families well.
#The government should increase the subsidy on liquor and increase healthcare and welfare camps to generate awareness among people, how bad drinking is and how it affects your body.
#WHO with the help of developed nations should help the government of developing countries in providing free food, education and organize healthcare camps.


###################################################################################################################
############################################ LINEAR REGRESSION MODEL ##############################################
###################################################################################################################



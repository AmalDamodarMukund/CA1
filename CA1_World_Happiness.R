#Reading whole raw data from CSV file
happiness_data_raw <- read.csv("World-happiness.csv")

#Dataframe
happiness_data_raw

#Structure of Dataframe
str(happiness_data_raw)

#Data cleaning

#Selecting only required variables
happiness_data_selected <- happiness_data_raw[,c(1:7,9)]
str(happiness_data_selected)

#Summarizing the rows with NULL values
colSums(is.na(happiness_data_selected))

#Removing NULL value rows
happiness_data <- na.omit(happiness_data_selected)

#Checking and confirming there are no rows with NA rows after replacement with 0
colSums(is.na(happiness_data))

#Displaying structure of final dataframe
str(happiness_data)

# Renaming the columns to meaningful one
names(happiness_data)[1] <- "Country"
names(happiness_data)[2] <- "Year"
names(happiness_data)[3] <- "Life_Ladder_Score"
names(happiness_data)[4] <- "GDP"
names(happiness_data)[5] <- "Social_Support_Score"
names(happiness_data)[6] <- "Life_expectancy"
names(happiness_data)[7] <- "Freedom_Score"
names(happiness_data)[8] <- "Corruption_Perception_Score"
str(happiness_data)

#############################################
#Q1. Life_Ladder_Score Vs GDP
#H0 - Life ladder is not affected by GDP
#H1 – Life ladder is affected by GDP

#Plotting histogram of Life ladder
hist(happiness_data$Life_Ladder_Score, 
     col='steelblue', 
     xlab = "Life ladder",
     main='Life ladder')

#Plotting histogram of GDP
hist(happiness_data$GDP, 
     col='steelblue', 
     xlab = "GDP",
     main='GDP per capita')

#Q-Q plot of Life ladder
qqnorm(happiness_data$Life_Ladder_Score, main = "Life ladder score")
qqline(happiness_data$Life_Ladder_Score, col = "red")

#Q-Q plot of GDP
qqnorm(happiness_data$GDP, main = "GDP per capita")
qqline(happiness_data$GDP, col = "red")

#Shapiro test of Life ladder
normality_test <- shapiro.test(happiness_data$Life_Ladder_Score)
normality_test$p.value
#p-value <0.05 so data not normally distributed

#Shapiro test of GDP
normality_test <- shapiro.test(happiness_data$GDP)
normality_test$p.value
#p-value <0.05 so data not normally distributed

#Dependent variable is Continuous 
#Independent variable is Continuous 
#Dependent variable is not normally distributed
#So Spearman’s Correlation Co-efficient

correlation <- cor.test(x=happiness_data$GDP, y=happiness_data$Life_Ladder_Score, method = 'spearman')
correlation
#P-value < 2.2e-16 and Correlation value is 0.8092679  
#So Strong positive correlation
#H1 is correct


#######################################

#Q2. Life_expectancy Vs Social_Support_Score
#H0 - Life expectancy is not affected by Social support
#H1 – Life expectancy is affected by Social support

#Plotting histogram of Life expectancy
hist(happiness_data$Life_expectancy, 
     col='steelblue', 
     xlab = "Age",
     main='Life expectancy')

#Plotting histogram of Social support
hist(happiness_data$Social_Support_Score, 
     col='steelblue', 
     xlab = "Social support Score",
     main='Social support')

#Q-Q plot of Life expectancy
qqnorm(happiness_data$Life_expectancy, main = "Life expectancy score")
qqline(happiness_data$Life_expectancy, col = "red")

#Q-Q plot of Social support
qqnorm(happiness_data$Social_Support_Score, main = "Social support Score")
qqline(happiness_data$Social_Support_Score, col = "red")

#Shapiro test of Life expectancy
normality_test <- shapiro.test(happiness_data$Life_expectancy)
normality_test$p.value
#p-value <0.05 so data not normally distributed

#Shapiro test of Social support
normality_test <- shapiro.test(happiness_data$Social_Support_Score)
normality_test$p.value
#p-value <0.05 so data not normally distributed

#Dependent variable is Continuous 
#Independent variable is Continuous 
#Dependent variable is not normally distributed
#So Spearman’s Correlation Co-efficient

correlation <- cor.test(x=happiness_data$Social_Support_Score, y=happiness_data$Life_expectancy, method = 'spearman')
correlation
#P-value < 2.2e-16 and Correlation value is 0.6747387 
#So positive correlation
#H1 is correct

#############################################
#Q3. Life_Ladder_Score Vs Corruption_Perception_Score
#H0 - Life ladder is not affected by Corruption perception
#H1 – Life ladder is affected by Corruption perception

#Plotting histogram of Life ladder
hist(happiness_data$Life_Ladder_Score, 
     col='steelblue', 
     xlab = "Life ladder",
     main='Life ladder')

#Plotting histogram of Corruption perception
hist(happiness_data$Corruption_Perception_Score, 
     col='steelblue', 
     xlab = "Corruption Perception Score",
     main='Corruption perception')

#Q-Q plot of Life ladder
qqnorm(happiness_data$Life_Ladder_Score, main = "Life ladder score")
qqline(happiness_data$Life_Ladder_Score, col = "red")

#Q-Q plot of Corruption perception
qqnorm(happiness_data$Corruption_Perception_Score, main = "Corruption perception")
qqline(happiness_data$Corruption_Perception_Score, col = "red")

#Shapiro test of Life ladder
normality_test <- shapiro.test(happiness_data$Life_Ladder_Score)
normality_test$p.value
#p-value <0.05 so data not normally distributed

#Shapiro test of Corruption perception
normality_test <- shapiro.test(happiness_data$Corruption_Perception_Score)
normality_test$p.value
#p-value <0.05 so data not normally distributed

#Dependent variable is Continuous 
#Independent variable is Continuous 
#Dependent variable is not normally distributed
#So Spearman’s Correlation Co-efficient

correlation <- cor.test(x=happiness_data$Corruption_Perception_Score, y=happiness_data$Life_Ladder_Score, method = 'spearman')
correlation
#P-value < 2.2e-16 and Correlation value is -0.331616
#So slight negative correlation
#H1 is correct

###########################################

#Q4. Life_expectancy Vs GDP
#H0 - Life expectancy is not affected by GDP
#H1 – Life expectancy is affected by GDP

#Plotting histogram of Life expectancy
hist(happiness_data$Life_expectancy, 
     col='steelblue', 
     xlab = "Age",
     main='Life expectancy')

#Plotting histogram of GDP
hist(happiness_data$GDP, 
     col='steelblue', 
     xlab = "GDP",
     main='GDP per capita')

#Q-Q plot of Life expectancy
qqnorm(happiness_data$Life_expectancy, main = "Life expectancy score")
qqline(happiness_data$Life_expectancy, col = "red")

#Q-Q plot of GDP
qqnorm(happiness_data$GDP, main = "GDP per capita")
qqline(happiness_data$GDP, col = "red")

#Shapiro test of Life expectancy
normality_test <- shapiro.test(happiness_data$Life_expectancy)
normality_test$p.value
#p-value <0.05 so data not normally distributed

#Shapiro test of GDP
normality_test <- shapiro.test(happiness_data$GDP)
normality_test$p.value
#p-value <0.05 so data not normally distributed

#Dependent variable is Continuous 
#Independent variable is Continuous 
#Dependent variable is not normally distributed
#So Spearman’s Correlation Co-efficient

correlation <- cor.test(x=happiness_data$GDP, y=happiness_data$Life_expectancy, method = 'spearman')
correlation
#P-value < 2.2e-16 and Correlation value is 0.878222
#So Strong positive correlation
#H1 is correct

###########################################

#Q5. Life_Ladder_Score Vs Freedom_Score
#H0 - Life ladder is not affected by Freedom of choice
#H1 – Life ladder is affected by Freedom of choice

#Plotting histogram of Life ladder
hist(happiness_data$Life_Ladder_Score, 
     col='steelblue', 
     xlab = "Life ladder",
     main='Life ladder')

#Plotting histogram of Freedom of choice
hist(happiness_data$Freedom_Score, 
     col='steelblue', 
     xlab = "Freedom Score",
     main='Freedom of choice')

#Q-Q plot of Life ladder
qqnorm(happiness_data$Life_Ladder_Score, main = "Life ladder score")
qqline(happiness_data$Life_Ladder_Score, col = "red")

#Q-Q plot of Freedom of choice
qqnorm(happiness_data$Freedom_Score, main = "Freedom of choice")
qqline(happiness_data$Freedom_Score, col = "red")

#Shapiro test of Life ladder
normality_test <- shapiro.test(happiness_data$Life_Ladder_Score)
normality_test$p.value
#p-value <0.05 so data not normally distributed

#Shapiro test of Freedom of choice
normality_test <- shapiro.test(happiness_data$Freedom_Score)
normality_test$p.value
#p-value <0.05 so data not normally distributed

#Dependent variable is Continuous 
#Independent variable is Continuous 
#Dependent variable is not normally distributed
#So Spearman’s Correlation Co-efficient

correlation <- cor.test(x=happiness_data$Freedom_Score, y=happiness_data$Life_Ladder_Score, method = 'spearman')
correlation
#P-value < 2.2e-16 and Correlation value is 0.5352722
#So positive correlation
#H1 is correct



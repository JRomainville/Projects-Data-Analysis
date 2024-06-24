# Jose Romainville
#Jan , 2024
# R code for A2: Household Spend - Individual
#Purpose: Build a Prediction Mo
library(purrr)
library(DataExplorer)
library(rpart)
library(caret)
library(plyr)
library(dplyr)
library(MLmetrics)
library(vtreat)
library(corrplot)
library(ggplot2)
library(reshape2)
library(rpart.plot)
#clean enviroment
rm(list=ls())

#/////////////LOAD DATA//////////////////////////
## Set the working directory
setwd("~/Hult/DUAL DEGREE/Visualizing & Analyzing Data with R Methods & Tools - DAT-5323 - BMBANDD1/Hult_Class/Cases/A2_Household_Spend/studentTables")
options(scipen=999)
# data
#Read Files
allTrainingFiles<- list.files(path = '~/Hult/DUAL DEGREE/Visualizing & Analyzing Data with R Methods & Tools - DAT-5323 - BMBANDD1/Hult_Class/Cases/A2_Household_Spend/studentTables',
                               pattern = 'training',
                               full.names = T)
allprospectsFiles<- list.files(path = '~/Hult/DUAL DEGREE/Visualizing & Analyzing Data with R Methods & Tools - DAT-5323 - BMBANDD1/Hult_Class/Cases/A2_Household_Spend/studentTables',
                               pattern = 'prospects',
                               full.names = T)
alltestingFiles<- list.files(path = '~/Hult/DUAL DEGREE/Visualizing & Analyzing Data with R Methods & Tools - DAT-5323 - BMBANDD1/Hult_Class/Cases/A2_Household_Spend/studentTables',
                               pattern = 'testing',
                             full.names = T)
#Load Data
training_mergedFull <- lapply(allTrainingFiles, read.csv)
training_mergedFull <- join_all(training_mergedFull, by='tmpID', type='left')
prospects_merged <- lapply(allprospectsFiles, read.csv)
prospects_merged <- join_all(prospects_merged, by='tmpID', type='left')
testing_merged <- lapply(alltestingFiles, read.csv)
testing_merged <- join_all(testing_merged, by='tmpID', type='left')

# SAMPLE
### Using the training data the partition of 80/20. 
# Partitioning
splitPercent <- round(nrow(training_mergedFull) %*% .8)
totalRecords <- 1:nrow(training_mergedFull)
set.seed(1234)
idx <- sample(totalRecords, splitPercent)
training_merged <- training_mergedFull[idx, ]
validation <- training_mergedFull[-idx, ]
#EDA

#////////////////Selecting usable variables////////////////////////////////////////////////////
# After looking at the Data Dictionary lets drop all the variables that are
#clearly unnecesary or not related the household spend as well as unethical
# ones like religious or political affiliations
# or race and nationality as well as not relevant attributes like donations.Example if you are a horse owner 
# or not there is no relation to buying domestic merchandise 
coltodrop<-c('tmpID','ResidenceHHGenderDescription','EthnicDescription','BroadEthnicGroupings',
             'MosaicZ4','HorseOwner','ReligiousContributorInHome','PoliticalContributerInHome','DonatesEnvironmentCauseInHome',
             'DonatesToCharityInHome','DonatestoAnimalWelfare','DonatestoArtsandCulture',
             'DonatestoChildrensCauses','DonatestoHealthcare','DonatestoInternationalAidCauses'
             ,'DonatestoVeteransCauses','DonatestoHealthcare1','DonatestoInternationalAidCauses1',
             'DonatestoWildlifePreservation','FirstName','LastName','TelephonesFullPhone',
             'lat','lon','fips','stateFips','ReligiousMagazineInHome','PartiesDescription',
             'ReligionsDescription','LikelyUnionMember','GunOwner','supportsAffordableCareAct',
             'supportsGayMarriage','supportsGunControl','supportsTaxesRaise','overallsocialviews',
             'DonatestoConservativeCauses','DonatestoLiberalCauses','DonatestoLocalCommunity','InterestinCurrentAffairsPoliticsInHousehold')

#yhat is the value to predict
prospects_temp<-prospects_merged$tmpID
# Drop the columns for all of the tables
training_merged <- training_merged[, !names(training_merged) %in% coltodrop]
prospects_merged <- prospects_merged[, !names(prospects_merged) %in% coltodrop]
testing_merged <- testing_merged[, !names(testing_merged) %in% coltodrop]
validation <- validation[, !names(validation) %in% coltodrop]
# Review the data 
head(training_merged)
str(training_merged)

training_merged[training_merged == ""] <- NA
# Create missingness plot
plot_missing(training_merged)
plot_missing(training_merged[, colSums(is.na(training_merged)) > 0])
# are being treated like chr "HomePurchasePrice", "LandValue", "EstHomeValue"
# This should change to numerical
# Specify the columns to process
columns_to_process <- c("HomePurchasePrice", "LandValue", "EstHomeValue")

# Loop through each column and apply the conversion to all data
# training
for (col in columns_to_process) {
  training_merged[[col]] <- as.numeric(gsub("[$,]", "", training_merged[[col]]))
}
# validation
for (col in columns_to_process) {
  validation[[col]] <- as.numeric(gsub("[$,]", "", validation[[col]]))
}
# testing
for (col in columns_to_process) {
  testing_merged[[col]] <- as.numeric(gsub("[$,]", "", testing_merged[[col]]))
}
# prospect
for (col in columns_to_process) {
  prospects_merged[[col]] <- as.numeric(gsub("[$,]", "", prospects_merged[[col]]))
}
# Verify the changes
str(training_merged[, columns_to_process])
#////////////////Numerical Values////////////////////////////////////////////////////
#the numerical values
numericVarstrain <- which(sapply(training_merged, is.numeric))
numericVarstrain
# ISPSA MedianEducationYears                  Age    HomePurchasePrice            LandValue 
# 2                        4                   22                   26                   27 
# storeVisitFrequency         EstHomeValue                 yHat 
# 29                          31                           32 

#////////////////missing Values////////////////////////////////////////////////

missing_num_vars <- numericVarstrain[sapply(training_merged[numericVarstrain], 
                                            function(x) any(is.na(x) | x == ""))]
missing_num_vars
# Create a table to store variable names and their missing percentages for numerical variables
missing_percentage_num_table <- data.frame(
  Variable = character(length(missing_num_vars)),
  PercentageMissing = numeric(length(missing_num_vars))
)

# Calculate the percentage of missing values for each numerical variable
for (i in seq_along(missing_num_vars)) {
  var <- missing_num_vars[i]
  total_missing <- sum(is.na(training_merged[[var]]) | training_merged[[var]] == "")
  percentage_missing <- (total_missing / length(training_merged[[var]])) * 100

  # Store results in the table
  missing_percentage_num_table[i, ] <- c(var, percentage_missing)
  
  cat(sprintf(" %s: %.2f%% missing\n", names(training_merged)[var], percentage_missing))
}

# ISPSA: 3.38% missing
# MedianEducationYears: 3.06% missing
# Age: 0.07% missing
# HomePurchasePrice: 52.95% missing
# LandValue: 29.35% missing
# EstHomeValue: 1.20% missing

# Drop the column: HomePurchasePrice have more than 40% of missing values
training_merged <- subset(training_merged, select = -c(HomePurchasePrice))
validation <- subset(validation, select = -c(HomePurchasePrice))
testing_merged <- subset(testing_merged, select = -c(HomePurchasePrice))
prospects_merged <- subset(prospects_merged, select = -c(HomePurchasePrice))
# Remove the rows: EstHomeValue,Age have less than 1.2% of missing values
colremrows <- c("EstHomeValue", "Age")
training_merged <- training_merged[complete.cases(training_merged[, colremrows]), ]
print(table(training_merged$Age))
# Verify the changes
str(training_merged)
# Replace values: LandValue,MedianEducationYears,ISPSA
hist(training_merged$LandValue, main = "Distribution of LandValue", xlab = "LandValue")
hist(training_merged$MedianEducationYears, main = "Distribution of MedianEducationYears", xlab = "MedianEducationYears")
hist(training_merged$ISPSA, main = "Distribution of ISPSA", xlab = "ISPSA")
#Median imputation as the distribution is more skewed:LandValue,ISPSA
# Identify missing values LandValue
missing_values <- is.na(training_merged$LandValue) | training_merged$LandValue == ""
# Replace missing values with the median
training_merged$LandValue[missing_values] <- median(training_merged$LandValue, na.rm = TRUE)
#verify
missing_values <- is.na(training_merged$LandValue) | training_merged$LandValue == ""
sum(missing_values)
# Identify missing values ISPSA
missing_values <- is.na(training_merged$ISPSA) | training_merged$ISPSA == ""
# Replace missing values with the median
training_merged$ISPSA[missing_values] <- median(training_merged$ISPSA, na.rm = TRUE)
#verify
missing_values <- is.na(training_merged$ISPSA) | training_merged$ISPSA == ""
sum(missing_values)
#Mean imputation as the distribution is more centred :MedianEducationYears
# Identify missing values MedianEducationYears
missing_values <- is.na(training_merged$MedianEducationYears) | training_merged$MedianEducationYears == ""
# Replace missing values with the mean
training_merged$MedianEducationYears[missing_values] <- mean(training_merged$MedianEducationYears, na.rm = TRUE)
#verify
missing_values <- is.na(training_merged$MedianEducationYears) | training_merged$MedianEducationYears == ""
sum(missing_values)
#To avoid type of data change
testing_merged$MedianEducationYears <- as.numeric(testing_merged$MedianEducationYears)
prospects_merged$MedianEducationYears <- as.numeric(prospects_merged$MedianEducationYears)
validation$MedianEducationYears <- as.numeric(validation$MedianEducationYears)
#//////////////Correlation before outlier treatment///////////////////////////////////////////////////////
#Let see their correlation
numeric_vars_train <- training_merged[, sapply(training_merged, is.numeric)]
correlation_matrix <- cor(numeric_vars_train)
melted_corr <- melt(correlation_matrix)
ggplot(melted_corr, aes(Var1, Var2, fill = value, label = round(value, 2))) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Corrected label
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# there are no high correlations between the variables and the Household spending
# only ISPSA, and MedianEducationYears hold a week correlation
#//////////Outlier treatment/////////////////////////////////////////////////////

# To see the values discrete and continuous

 boxplot(training_merged$ISPSA,horizontal = TRUE)
 boxplot(training_merged$MedianEducationYears,horizontal = TRUE)
 boxplot(training_merged$Age,horizontal = TRUE)
 boxplot(training_merged$storeVisitFrequency,horizontal = TRUE)
 boxplot(training_merged$yHat,horizontal = TRUE)
 boxplot(training_merged$EstHomeValue,horizontal = TRUE)
 boxplot(training_merged$LandValue,horizontal = TRUE)
 
#From the variables ISPSA have outliers which is OK as these are classification values
# numbers and there is no influence of the outliers. However EstHomeValue and LandValue do have outliers
# For the dependent variable yHat there are some outliers
 #///////////////////  yHat////////////////////////////
 # Calculate the interquartile range (IQR)
 Q1 <- quantile(training_merged$yHat, 0.25)
 Q3 <- quantile(training_merged$yHat, 0.75)
 IQR_value <- Q3 - Q1
 
 # Define the upper and lower bounds for outliers
 lower_bound <- Q1 - 1.5 * IQR_value
 upper_bound <- Q3 + 1.5 * IQR_value
 
 # Identify outliers
 outliers <- training_merged$yHat < lower_bound | training_merged$yHat > upper_bound
 
 # Count the number of outliers
 num_outliers <- sum(outliers)
 
 # Calculate the percentage of outliers
 percentage_outliers <- (num_outliers / length(training_merged$yHat)) * 100 
 # Print results
 cat("Number of outliers:", num_outliers, "\n")
 cat("Percentage of outliers:", percentage_outliers, "%\n")
#Outliers are less than 5% of the data so it might be ok to drop them

 training_mergedNew <- training_merged[!outliers, ]
 boxplot(training_mergedNew$yHat,horizontal = TRUE)
 # it seems that more outliers are being created, as the original amount
 # is less than 5% of the data set
# Lets Replace outliers with upper and lower fence values capping the outliers for a better treatment
 training_merged$yHat[outliers] <- ifelse(training_merged$yHat[outliers] < lower_bound, lower_bound, upper_bound)
#Review of the boxplot 
 boxplot(training_merged$yHat,horizontal = TRUE)

 #///////////////////  EstHomeValue////////////////////////////
 # Calculate the interquartile range (IQR)
 Q1 <- quantile(training_merged$EstHomeValue, 0.25)
 Q3 <- quantile(training_merged$EstHomeValue, 0.75)
 IQR_value <- Q3 - Q1
 # Define the upper and lower bounds for outliers
 lower_bound <- Q1 - 1.5 * IQR_value
 upper_bound <- Q3 + 1.5 * IQR_value
 # Identify outliers
 outliers <- training_merged$EstHomeValue < lower_bound | training_merged$EstHomeValue > upper_bound
 # Count the number of outliers
 num_outliers <- sum(outliers)
 # Calculate the percentage of outliers
 percentage_outliers <- (num_outliers / length(training_merged$EstHomeValue)) * 100 
 # Print results
 cat("Number of outliers:", num_outliers, "\n")
 cat("Percentage of outliers:", percentage_outliers, "%\n")
 # Outliers are less than 5% of the data. In this case the EstHomeValue could have
 # natural variations as there are few exclusive areas in the cities
 # where the home values are extremely high compared to the other areas of the same city
 # and in here they are being compared to lots of places, maybe even rural
 # so its better to leave the outliers as they are
 #///////////////////LandValue////////////////////////////
 # Calculate the interquartile range (IQR)
 Q1 <- quantile(training_merged$LandValue, 0.25)
 Q3 <- quantile(training_merged$LandValue, 0.75)
 IQR_value <- Q3 - Q1
 # Define the upper and lower bounds for outliers
 lower_bound <- Q1 - 1.5 * IQR_value
 upper_bound <- Q3 + 1.5 * IQR_value
 # Identify outliers
 outliers <- training_merged$LandValue < lower_bound | training_merged$LandValue > upper_bound
 # Count the number of outliers
 num_outliers <- sum(outliers) 
 # Calculate the percentage of outliers
 percentage_outliers <- (num_outliers / length(training_merged$LandValue)) * 100
 cat("Number of outliers:", num_outliers, "\n")
 cat("Percentage of outliers:", percentage_outliers, "%\n")
 # Outliers are less than 10% of the data. In this case the LandValue follows
 # the same caracteristic as EstHomeValue
 
 #//////////////Correlation after outlier treatment///////////////////////////////
 #Let see their correlation
 numeric_vars_train <- training_merged[, sapply(training_merged, is.numeric)]
 correlation_matrix <- cor(numeric_vars_train)
 melted_corr <- melt(correlation_matrix)
 ggplot(melted_corr, aes(Var1, Var2, fill = value, label = round(value, 2))) +
   geom_tile(color = "white") +
   geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Corrected label
   scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 # there are still weak between the variables and the Household spending
 # only ISPSA, and MedianEducationYears hold a week correlation
 # To avoid overfiting using variables with correlations near 0 lets keep only the ones
 # who have at least some correlation with yHat ISPSA and MedianEducationYears.
 # Let also include the ones with correlation with this two variables
 #It is kind of surprising at firs that the frequency of visit is not a driving factor
 # but Bedding Bathing & Yonder have online presence and people might visit
 # the store just to compare prices as most of the entries have a 6 times visit over the 
 # last 12 months
 
 coltodrop<-c('EstHomeValue','LandValue')
 
 training_merged <- training_merged[, !names(training_merged) %in% coltodrop]
 prospects_merged <- prospects_merged[, !names(prospects_merged) %in% coltodrop]
 testing_merged <- testing_merged[, !names(testing_merged) %in% coltodrop]
 validation <- validation[, !names(validation) %in% coltodrop]
 #////////////////categorical Values////////////////////////////////////////////////////
categorical_vars_train <- which(sapply(training_merged, function(x) is.factor(x) | is.character(x)))
categorical_vars_train
# Print results

# PresenceOfChildrenCode                             HomeOwnerRenter 
# 1                                           3 
# NetWorth                                    Investor 
# 5                                           6 
# BusinessOwner                                   Education 
# 7                                           8 
# OccupationIndustry                                  HorseOwner 
# 9                                          10 
# CatOwner                                    DogOwner 
# 11                                          12 
# OtherPetOwner                                  HomeOffice 
# 13                                          14 
# BookBuyerInHome                          UpscaleBuyerInHome 
# 15                                          16 
# BuyerofAntiquesinHousehold                       BuyerofArtinHousehold 
# 17                                          18 
# GeneralCollectorinHousehold                BooksAudioReadinginHousehold 
# 19                                          20 
# ComputerOwnerInHome                     DonatestoLocalCommunity 
# 21                                          22 
# FamilyMagazineInHome                FemaleOrientedMagazineInHome 
# 23                                          24 
# GardeningMagazineInHome              CulinaryInterestMagazineInHome 
# 25                                          26 
# HealthFitnessMagazineInHome                  DoItYourselfMagazineInHome 
# 27                                          28 
# FinancialMagazineInHome InterestinCurrentAffairsPoliticsInHousehold 
# 29                                          30 
# Veteran                                      Gender 
# 31                                          32 
# county                                        city 
# 34                                          35 
# state                           HomePurchasePrice 
# 36                                          37 
# LandValue                            DwellingUnitSize 
# 38                                          39 
# PropertyType                                EstHomeValue 
# 41                                          42

#Lets see the missing values
missing_categorical_vars <- categorical_vars_train[sapply(training_merged[categorical_vars_train], 
                                            function(x) any(is.na(x) | x == ""))]
missing_categorical_vars

# Create a table to store variable names and their missing percentages
missing_percentage_table <- data.frame(
  Variable = character(length(missing_categorical_vars)),
  PercentageMissing = numeric(length(missing_categorical_vars))
)

# Calculate the percentage of missing values for each variable
for (i in seq_along(missing_categorical_vars)) {
  var <- missing_categorical_vars[i]
  total_missing <- sum(is.na(training_merged[[var]]) | training_merged[[var]] == "")
  percentage_missing <- (total_missing / length(training_merged[[var]])) * 100
# Store results in the table
  missing_percentage_table[i, ] <- c(names(training_merged)[var], percentage_missing)
  
  cat(sprintf(" %s: %.2f%% missing\n", names(training_merged)[var], percentage_missing))
}
# Identify columns with more than 40% missing values to drop those columns
# in this dataset the columns 
print(missing_percentage_table)

CatColToDrop <- missing_percentage_table$Variable[missing_percentage_table$PercentageMissing > 70]

# Drop columns with more than 70% missing values
training_merged <- training_merged[, !names(training_merged) %in% CatColToDrop]
prospects_merged <- prospects_merged[, !names(prospects_merged) %in% CatColToDrop]
testing_merged <- testing_merged[, !names(testing_merged) %in% CatColToDrop]
validation <- validation[, !names(validation) %in% CatColToDrop]
#//////////// After removing the categorical values with too much missing values////////
#With the remaining categorical
categorical_vars_train <- which(sapply(training_merged, function(x) is.factor(x) | is.character(x)))
categorical_vars_train
#Lets see the missing values
missing_categorical_vars <- categorical_vars_train[sapply(training_merged[categorical_vars_train], 
                                                          function(x) any(is.na(x) | x == ""))]
missing_categorical_vars


# Create a table to store variable names and their missing percentages
missing_percentage_table <- data.frame(
  Variable = character(length(missing_categorical_vars)),
  PercentageMissing = numeric(length(missing_categorical_vars))
)

# Calculate the percentage of missing values for each variable
for (i in seq_along(missing_categorical_vars)) {
  var <- missing_categorical_vars[i]
  total_missing <- sum(is.na(training_merged[[var]]) | training_merged[[var]] == "")
  percentage_missing <- (total_missing / length(training_merged[[var]])) * 100
  # Store results in the table
  missing_percentage_table[i, ] <- c(names(training_merged)[var], percentage_missing)
  
  cat(sprintf(" %s: %.2f%% missing\n", names(training_merged)[var], percentage_missing))
}
# PresenceOfChildrenCode: 10.96% missing
# HomeOwnerRenter: 6.54% missing
# NetWorth: 21.47% missing
# Investor: 57.77% missing
# BookBuyerInHome: 65.81% missing
# GeneralCollectorinHousehold: 69.93% missing
# Gender: 0.03% missing
# DwellingUnitSize: 15.59% missing
# HealthFitnessMagazineInHome: 69.45% missing


# for HealthFitnessMagazineInHome case there are lots of missing values
# and there is not that important to predict the expenses in household
training_merged<-subset(training_merged, select = -HealthFitnessMagazineInHome)
validation<-subset(validation, select = -HealthFitnessMagazineInHome)
testing_merged<-subset(testing_merged, select = -HealthFitnessMagazineInHome)
prospects_merged<-subset(prospects_merged, select = -HealthFitnessMagazineInHome)
#///// evaluate the missing values greater than 50%//////////
print(table(training_merged$Investor))
order_data <- names(sort(table(training_merged$Investor), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(Investor, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of Investor",
       x = "Investor",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#       Yes 
# 6844 5004 
#Missings should be No. Investor could represent an important variable to know the spend
# Replace missing values or empty strings with "No"
training_merged$Investor <- ifelse(is.na(training_merged$Investor) | training_merged$Investor == "", 
                                   "No", 
                                   training_merged$Investor)
# frequency plot
order_data <- names(sort(table(training_merged$Investor), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(Investor, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of Investor",
       x = "Investor",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# BookBuyerInHome
print(table(training_merged$BookBuyerInHome))
#                         1 book purchase in home  2 book purchases in home 3 book purchases in home 
# 7797                     1547                      931                      580 
# 4 book purchases in home 5 book purchases in home 6 book purchases in home 7 book purchases in home 
# 363                      241                      142                      108 
# 8 book purchases in home 9 book purchases in home 
# 48                       91 
# missing should be 0 purchases
# Replace missing values or empty strings with "0"
# BookBuyerInHome could represent an important variable to know the spend as people need places to 
# put their books
training_merged$BookBuyerInHome <- ifelse(is.na(training_merged$BookBuyerInHome) | training_merged$BookBuyerInHome == "", 
                                   "0 book purchases in home", 
                                   training_merged$BookBuyerInHome)
# frequency plot
order_data <- names(sort(table(training_merged$BookBuyerInHome), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(BookBuyerInHome, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of BookBuyerInHome",
       x = "BookBuyerInHome",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# i most of the purchases of more than 3 book purchases have less than 5%
# for the model lets put them together in a '>=3 book purchases in home' class
freq_table <- table(training_merged$BookBuyerInHome)

# Identify categories with less than 5% frequency
low_freq_categories <- names(freq_table[freq_table/sum(freq_table) < 0.05])

# Replace low-frequency categories with 'other'
training_merged$BookBuyerInHome[training_merged$BookBuyerInHome %in% low_freq_categories] <- '>=3 book purchases in home'

# Frequency table after replacement
print(table(training_merged$BookBuyerInHome))

# Frequency plot after replacement
order_data <- names(sort(table(training_merged$BookBuyerInHome), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(BookBuyerInHome, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of BookBuyerInHome",
       x = "BookBuyerInHome",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#GeneralCollectorinHousehold
print(table(training_merged$GeneralCollectorinHousehold))
#       Yes 
# 8285 3563
# missing should be '0 purchases'No'
# Replace missing values or empty strings with "No"
training_merged$GeneralCollectorinHousehold <- ifelse(is.na(training_merged$GeneralCollectorinHousehold) | training_merged$GeneralCollectorinHousehold == "", 
                                          "No", 
                                          training_merged$GeneralCollectorinHousehold)
# GeneralCollectorinHousehold could represent an important variable to know the spend as people need places to 
# put their books

print(table(training_merged$GeneralCollectorinHousehold))


#///// If there are less than 2% of missing lets drop the rows//////////
colremrows <- c("Gender")
training_merged <- training_merged[!(is.na(training_merged[[colremrows]]) | training_merged[[colremrows]] == ""), ]
#//// evaluate the other cases with missing values ////////////////////////
# if there are more than 85% of frequency of a certain value then 
# that column is not relevant for the model as it mostly always will have 
# the same value so it will be dropped. 
#Also grouping the values with less than 5% of frequency as 'Others'
#///////////////////DwellingUnitSize////////////////////////////
print(table(training_merged$DwellingUnitSize))
# frequency plot
order_data <- names(sort(table(training_merged$DwellingUnitSize), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(DwellingUnitSize, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of DwellingUnitSize",
       x = "DwellingUnitSize",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# in this case i will replace all of the cases that are not 1-Single Family Dwelling
# (79% of frequency) with the value 'Others'
# Calculate the frequency of each category
freq_table <- table(training_merged$DwellingUnitSize)
# Find the category with the highest frequency
max_freq_category <- names(freq_table)[which.max(freq_table)]
# Replace all cases that are not the most frequent category with 'Others'
training_merged$DwellingUnitSize <- ifelse(training_merged$DwellingUnitSize != max_freq_category, 
                                           'Others', 
                                           training_merged$DwellingUnitSize)
# Create a new order for the factor levels
order_data <- names(sort(table(training_merged$DwellingUnitSize), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(DwellingUnitSize, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of DwellingUnitSize",
       x = "DwellingUnitSize",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#///////////////////PresenceOfChildrenCode////////////////////////////

print(table(training_merged$PresenceOfChildrenCode))
# frequency plot
order_data <- names(sort(table(training_merged$PresenceOfChildrenCode), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(PresenceOfChildrenCode, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of PresenceOfChildrenCode",
       x = "PresenceOfChildrenCode",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# in this case i will replace all of the missing with the most frequent value
# as i am assuming that not knowing is that most likely they dont have a kid

# Identify missing values
missing_rows <- is.na(training_merged$PresenceOfChildrenCode) | training_merged$PresenceOfChildrenCode == ""

# Find the most frequent non-missing value
max_freq_category <- names(sort(table(training_merged$PresenceOfChildrenCode), decreasing = TRUE))[1]

# Replace missing values with the most frequent non-missing value
training_merged$PresenceOfChildrenCode[missing_rows] <- max_freq_category

# Frequency table after replacement
print(table(training_merged$PresenceOfChildrenCode))

# Frequency plot after replacement
order_data <- names(sort(table(training_merged$PresenceOfChildrenCode), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(PresenceOfChildrenCode, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of PresenceOfChildrenCode",
       x = "PresenceOfChildrenCode",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#/////////////////////////////NetWorth//////////////////////////////////////////////
print(table(training_merged$NetWorth))
# frequency plot
order_data <- names(sort(table(training_merged$NetWorth), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(NetWorth, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of NetWorth",
       x = "NetWorth",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# in this case it is better to randomly replace this missing values with the categories available
# as the missing values are the second highest in frequency and would affect
# the model if replaced by the leader in frequency. Other thing is that for this variable
# having the lowest frequency combined will not add value to the model.
# Identify missing values
missing_rows <- is.na(training_merged$NetWorth) | training_merged$NetWorth == ""
# Calculate frequencies of non-missing values
freq_table <- table(training_merged$NetWorth[!missing_rows])
# Calculate proportions of non-missing values
non_missing_props <- freq_table / sum(freq_table)
# Replace missing values with proportions
set.seed(123)  # For reproducibility
replacement_values <- sample(names(non_missing_props), sum(missing_rows), replace = TRUE, prob = non_missing_props)
training_merged$NetWorth[missing_rows] <- replacement_values

# Frequency table after replacement
print(table(training_merged$NetWorth))

# Frequency plot after replacement
order_data <- names(sort(table(training_merged$NetWorth), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(NetWorth, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of NetWorth",
       x = "NetWorth",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# //////////// Evaluate if the other cat values are useful for the model //////////////
# if there are more than 85% of frequency of a certain value then 
# that column is not relevant for the model as it mostly always will have 
# the same value so it will be dropped. 
#Also grouping the values with less than 5% of frequency as 'Others'

categorical_vars_train <- which(sapply(training_merged, function(x) is.factor(x) | is.character(x)))
categorical_vars_train
# PresenceOfChildrenCode               NetWorth              Education 
# 1                      4                      5 
# OccupationIndustry    ComputerOwnerInHome                 Gender 
# 6                      7                      8 
# county                   city                  state 
# 10                     11                     12 
# DwellingUnitSize           PropertyType                Veteran 
# 14                     16                     19 
#//////////Education /////////////////
print(table(training_merged$Education ))
# frequency plot
order_data <- names(sort(table(training_merged$Education ), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(Education , levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of Education ",
       x = "Education ",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
#Variable ok as most of the values are more than 4% of frequency
# for the model can lead to over fitting
freq_table <- table(training_merged$Education)

# Identify categories with less than 5% frequency
low_freq_categories <- names(freq_table[freq_table/sum(freq_table) < 0.05])

# Replace low-frequency categories with 'other'
training_merged$Education[training_merged$Education %in% low_freq_categories] <- 'other'
# Frequency table after replacement
print(table(training_merged$Education))
#///////////////////////////////OccupationIndustry////////////////////////////////////
print(table(training_merged$OccupationIndustry))
# frequency plot
order_data <- names(sort(table(training_merged$OccupationIndustry), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(OccupationIndustry, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of OccupationIndustry",
       x = "OccupationIndustry",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# in this case it is a variable that can be drop as the most frequent value
# "unknown" will not add value to the analysis and the rest of the occupations
# have frequencies with less than 5%.
training_merged<-subset(training_merged, select = -OccupationIndustry)
validation<-subset(validation, select = -OccupationIndustry)
prospects_merged<-subset(prospects_merged, select = -OccupationIndustry)
testing_merged<-subset(testing_merged, select = -OccupationIndustry)

#////////////////////////////ComputerOwnerInHome///////////////////////////////////
print(table(training_merged$ComputerOwnerInHome))
# frequency plot
order_data <- names(sort(table(training_merged$ComputerOwnerInHome), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(ComputerOwnerInHome, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of ComputerOwnerInHome",
       x = "ComputerOwnerInHome",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Variable ok
#////////////////////////////county///////////////////////////////////
print(table(training_merged$county))
# frequency plot
order_data <- names(sort(table(training_merged$county), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(county, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of county",
       x = "county",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## # as none of them have more than 5% of the frequency having this variable 
# for the model can lead to over fitting
training_merged<-subset(training_merged, select = -county)
validation<-subset(validation, select = -county)
testing_merged<-subset(testing_merged, select = -county)
prospects_merged<-subset(prospects_merged, select = -county)

#////////////////////////////city///////////////////////////////////
print(table(training_merged$city))
# frequency plot
order_data <- names(sort(table(training_merged$city), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(city, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of city",
       x = "city",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# # as none of them have more than 5% of the frequency having this variable 
# for the model can lead to over fitting
freq_table <- table(training_merged$city)

# Identify categories with less than 5% frequency
low_freq_categories <- names(freq_table[freq_table/sum(freq_table) < 0.005])

# Replace low-frequency categories with 'other'
training_merged$city[training_merged$city %in% low_freq_categories] <- 'other'

# Frequency table after replacement
print(table(training_merged$city))

# Frequency plot after replacement
order_data <- names(sort(table(training_merged$city), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(city, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of city",
       x = "city",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
## # as none of them have more than 5% of the frequency having this variable 
# for the model can lead to over fitting
training_merged<-subset(training_merged, select = -city)
validation<-subset(validation, select = -city)
testing_merged<-subset(testing_merged, select = -city)
prospects_merged<-subset(prospects_merged, select = -city)
#////////////////////////////state///////////////////////////////////
print(table(training_merged$state))
# frequency plot
order_data <- names(sort(table(training_merged$state), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(state, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of state",
       x = "state",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# there are less unique values but is still low frequency values
## in this case for all the variables with less than 1% will be 'Other' as the varaible is too sparce
# Calculate frequencies of each category
freq_table <- table(training_merged$state)

# Identify categories with less than 5% frequency
low_freq_categories <- names(freq_table[freq_table/sum(freq_table) < 0.01])

# Replace low-frequency categories with 'other'
training_merged$state[training_merged$state %in% low_freq_categories] <- 'other states'

# Frequency table after replacement
print(table(training_merged$state))

# Frequency plot after replacement
order_data <- names(sort(table(training_merged$state), decreasing = TRUE))

ggplot(training_merged, aes(x = factor(state, levels = order_data))) +
  geom_bar() +
  labs(title = "Bar Plot of state",
       x = "state",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#////////////////////////////PropertyType///////////////////////////////////
print(table(training_merged$PropertyType))
# frequency plot
order_data <- names(sort(table(training_merged$PropertyType), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(PropertyType, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of PropertyType",
       x = "PropertyType",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# for all the variables with less than 5% will be 'Other'
# Calculate frequencies of each category
freq_table <- table(training_merged$PropertyType)

# Identify categories with less than 5% frequency
low_freq_categories <- names(freq_table[freq_table/sum(freq_table) < 0.05])

# Replace low-frequency categories with 'other'
training_merged$PropertyType[training_merged$PropertyType %in% low_freq_categories] <- 'other'

# Frequency table after replacement
print(table(training_merged$PropertyType))

# Frequency plot after replacement
order_data <- names(sort(table(training_merged$PropertyType), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(PropertyType, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of PropertyType",
       x = "PropertyType",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#////////////////////////////Veteran///////////////////////////////////
print(table(training_merged$Veteran))
# frequency plot
order_data <- names(sort(table(training_merged$Veteran), decreasing = TRUE))
ggplot(training_merged, aes(x = factor(Veteran, levels = order_data))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Bar Plot of Veteran",
       x = "Veteran",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# this column can be dropped as it will not provide value to the model
training_merged<-subset(training_merged, select = -Veteran)
validation<-subset(validation, select = -Veteran)
testing_merged<-subset(testing_merged, select = -Veteran)
prospects_merged<-subset(prospects_merged, select = -Veteran)
rm(list = setdiff(ls(), c("training_merged", "prospects_merged","testing_merged","missing_percentage_table","validation","prospects_temp")))
#////////////////////////// MODIFY ////////////////////////////////////////

informartiveFeatures <- setdiff(names(training_merged), "yHat")
targetName <- 'yHat'
plan <- designTreatmentsN(dframe      = training_merged, 
                          varlist     = informartiveFeatures,
                          outcomename = targetName)
# Apply the plan to all sections of the data
treatedTrain <- prepare(plan, training_merged)
treatedValidation <- prepare(plan, validation)
treatedTest <- prepare(plan, testing_merged) 
treatedProspects <- prepare(plan, prospects_merged)
                            
#////////////////MODEL/////////////////////////
names(treatedTrain)
head(treatedTrain)

set.seed(1234)

#//// Linear//////

fitlm <- train(yHat ~ ., 
               data = treatedTrain,
               method = "lm",  # Use "lm" for linear regression
               trControl = trainControl(method = "cv", number = 5),
               # Other parameters as needed
)
fitlm
# RMSE      Rsquared    MAE     
# 93.84742  0.06826159  71.71445

# lets find the significant values of the linear model
summary_fitlm <- summary(fitlm)

# Extract the coefficients and t-values
coefficients <- coef(summary_fitlm)
t_values <- coefficients[, "t value"]

# Set a significance level (e.g., 0.05)
significance_level <- 0.05

# Identify variables with significant t-values
significant_variables <- rownames(coefficients)[abs(t_values) > qt(1 - significance_level/2, df = fitlm$finalModel$df.residual)]
# Assuming significant_variables is the list of significant variable names
significant_formula <- as.formula(paste("yHat ~", paste(significant_variables, collapse = " + ")))

# Use the significant_formula in the train function
fitlm_significant <- train(significant_formula,
                           data = treatedTrain,
                           method = "lm",
                           trControl = trainControl(method = "cv", number = 5),
                           # Other parameters as needed
)

fitlm_significant
# RMSE      Rsquared    MAE     
# 93.55334  0.07357967  71.48422
#slightly better than the previous

#//// rpart//////
fitrpart <- train(yHat ~., 
             data = treatedTrain, 
             #"recursive partitioning (trees)
             method = "rpart", 
             #Define a range for the CP to test
             tuneGrid = data.frame(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1)), 
             control = rpart.control(minsplit = 1, minbucket = 2)) 

# Examine
fitrpart
# cp      RMSE       Rsquared    MAE     
# 0.0001  123.21602  0.02583016  93.15928
# 0.0010   96.19601  0.08323941  71.32298
# 0.0050   89.50333  0.15306355  67.18252
# 0.0100   89.50115  0.15309904  67.18269
# 0.0500   90.56420  0.13297100  67.57377
# 0.0700   95.31034  0.07014186  73.91289
# 0.1000   97.25123         NaN  76.86903
plot(fitrpart)
#ploting the tree
prp(fitrpart$finalModel, extra = 1)
#cp = 0.005 gives back a slightly better model than the previous

#with significant variables
fitrpart_significant <- train(significant_formula, 
                  data = treatedTrain, 
                  #"recursive partitioning (trees)
                  method = "rpart", 
                  #Define a range for the CP to test
                  tuneGrid = data.frame(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1)), 
                  control = rpart.control(minsplit = 1, minbucket = 2))
fitrpart_significant
# cp      RMSE      Rsquared    MAE     
# 0.0001  97.89237  0.03404956  75.20293
# 0.0010  93.64229  0.07269922  71.61693
# 0.0050  93.61230  0.07327545  71.56274
# 0.0100  93.61230  0.07327545  71.56274
# 0.0500  93.61230  0.07327545  71.56274
# 0.0700  96.31535  0.06579217  75.43711
# 0.1000  97.22160         NaN  76.83636
# now the cp=0.001 is slightly worse than the previous model
#ploting the tree
prp(fitrpart_significant$finalModel, extra = 1)

# //////////////////////////ASSESS///////////////////////////////////////////
# Making predictions
#fitlm_significant
trainPredictions      <- predict(fitlm_significant, treatedTrain)
TestPredictionslm      <- predict(fitlm_significant, treatedTest)
ValidationPredictionslm      <- predict(fitlm_significant, treatedValidation)

TestdResults <-data.frame(actuals        = treatedTest$yHat,
                          predicted      = TestPredictionslm,
                          residualErrors = treatedTest$yHat-TestPredictionslm )
ValidationResults <-data.frame(actuals        = treatedValidation$yHat,
                               predicted      = ValidationPredictionslm,
                               residualErrors = treatedValidation$yHat-ValidationPredictionslm )
TrainResults <-data.frame(actuals        = treatedTrain$yHat,
                          predicted      = trainPredictions,
                          residualErrors = treatedTrain$yHat-trainPredictions )
(ValidationResultsRMSE <- MLmetrics::RMSE(ValidationResults$predicted, 
                                          ValidationResults$actuals))
(testResultsRMSE <- MLmetrics::RMSE(TestdResults$predicted, 
                                    TestdResults$actuals))
(trainResultsRMSE <- MLmetrics::RMSE(TrainResults$predicted, 
                                     TrainResults$actuals))
# > ValidationResultsRMSE
# [1] 95.16957
# > testResultsRMSE
# [1] 94.13914
# > trainResultsRMSE
# [1] 93.51813
#RPart
trainPredictions      <- predict(fitrpart, treatedTrain)
ValidationPredictions      <- predict(fitrpart, treatedValidation)
TestPredictions      <- predict(fitrpart, treatedTest)
ValidationResults <-data.frame(actuals        = treatedValidation$yHat,
                             predicted      = ValidationPredictions,
                             residualErrors = treatedValidation$yHat-ValidationPredictions )
TestResults <-data.frame(actuals        = treatedTest$yHat,
                          predicted      = TestPredictions,
                          residualErrors = treatedTest$yHat-TestPredictions )
TrainResults <-data.frame(actuals        = treatedTrain$yHat,
                         predicted      = trainPredictions,
                         residualErrors = treatedTrain$yHat-trainPredictions )
(ValidationResultsRMSE <- MLmetrics::RMSE(ValidationResults$predicted, 
                                          ValidationResults$actuals))
(testResultsRMSE <- MLmetrics::RMSE(TestResults$predicted, 
                                    TestResults$actuals))
(trainResultsRMSE <- MLmetrics::RMSE(TrainResults$predicted, 
                                     TrainResults$actuals))

# > ValidationResultsRMSE
# [1] 91.51989
# > testResultsRMSE
# [1] 89.67507
# > trainResultsRMSE
# [1] 89.41338
# These predictions are consistent for the sets
# Still rPart model is the one with better RMSE
(trainResultsMAPE <- MLmetrics::MAPE(TrainResults$predicted, 
                                     TrainResults$actuals))
# 0.435598 : this mean 43% of the predictions of this model deviate from the actual value arround 43%

#The Best model is fitrpart
plot(fitrpart)
#ploting the tree
prp(fitrpart$finalModel)
# Using the best possible model make predictions on the prospect set.
prospectPredictions <- predict(fitrpart, treatedProspects)
max(prospectPredictions$Prediction)
min(prospectPredictions$Prediction)
# Column bind the predictions to the prospect CSV; finish the case submissions. 
prospectPredictionsMatrix <- cbind(tmpID = prospects_temp, Prediction = prospectPredictions)
prospectPredictions <- as.data.frame(prospectPredictionsMatrix)

boxplot(prospectPredictions$Prediction,horizontal = TRUE)
write.csv(prospectPredictions, file = "prospectPredictions.csv", row.names = FALSE)
#End
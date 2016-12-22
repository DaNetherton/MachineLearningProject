setwd("C:/Users/nethertd1/Desktop/MachineLearning")
rm(list=ls())
library(ISLR); library(ggplot2); 
library(kernlab)
library(dplyr)
library(plyr)
library(gridExtra)
library(Hmisc)
library(caret)
#Download Files (ensuring reproducability)
#fileUrl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
#download.file(fileUrl, destfile = "pml-training.csv")
#dateDownloaded<-today()

#fileUrl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
#download.file(fileUrl, destfile = "pml-testing.csv")
#dateDownloaded<-today()

testing <- read.csv("pml-testing.csv", stringsAsFactors = FALSE)
training <- read.csv("pml-training.csv", stringsAsFactors = FALSE)
#glimpse(training)
#glimpse(testing)
#table(training$classe)
summary(training)
#The following fields shouldn't be part of the model:
#user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp

#If windows are important, the fact that there's a new one doesn't seem as important as the distinction between windows.  So that new_window field can be thrown out.
training <- select(training, -user_name, -raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp, -new_window)
testing <- select(training, -user_name, -raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp, -new_window)

# Create a function for cleaning the weird values in some of the character fields and converting them to numeric
cleanDivO <- function(n)
{
  n <- gsub("#DIV/0!", "", n)  #  get rid of the #DIV/0!
  n <- as.numeric(n) # and convert to numeric
}

# Clean the fields that came in as character
training$kurtosis_roll_belt <- cleanDivO(training$kurtosis_roll_belt)
training$kurtosis_picth_belt <- cleanDivO(training$kurtosis_picth_belt)
training$kurtosis_yaw_belt <- cleanDivO(training$kurtosis_yaw_belt)
training$skewness_roll_belt <- cleanDivO(training$skewness_roll_belt)
training$skewness_roll_belt.1 <- cleanDivO(training$skewness_roll_belt.1)
training$skewness_yaw_belt <- cleanDivO(training$skewness_yaw_belt)
training$max_yaw_belt <- cleanDivO(training$max_yaw_belt)
training$min_yaw_belt <- cleanDivO(training$min_yaw_belt)
training$amplitude_yaw_belt <- cleanDivO(training$amplitude_yaw_belt)
training$kurtosis_roll_arm <- cleanDivO(training$kurtosis_roll_arm)
training$kurtosis_picth_arm <- cleanDivO(training$kurtosis_picth_arm)
training$kurtosis_yaw_arm <- cleanDivO(training$kurtosis_yaw_arm)
training$skewness_roll_arm <- cleanDivO(training$skewness_roll_arm)
training$skewness_pitch_arm <- cleanDivO(training$skewness_pitch_arm)
training$skewness_yaw_arm <- cleanDivO(training$skewness_yaw_arm)
training$kurtosis_roll_dumbbell <- cleanDivO(training$kurtosis_roll_dumbbell)
training$kurtosis_picth_dumbbell <- cleanDivO(training$kurtosis_picth_dumbbell)
training$kurtosis_yaw_dumbbell <- cleanDivO(training$kurtosis_yaw_dumbbell)
training$skewness_roll_dumbbell <- cleanDivO(training$skewness_roll_dumbbell)
training$skewness_pitch_dumbbell <- cleanDivO(training$skewness_pitch_dumbbell)
training$skewness_yaw_dumbbell <- cleanDivO(training$skewness_yaw_dumbbell)
training$max_yaw_dumbbell <- cleanDivO(training$max_yaw_dumbbell)
training$min_yaw_dumbbell <- cleanDivO(training$min_yaw_dumbbell)
training$amplitude_yaw_dumbbell <- cleanDivO(training$amplitude_yaw_dumbbell)
training$kurtosis_roll_forearm <- cleanDivO(training$kurtosis_roll_forearm)
training$kurtosis_picth_forearm <- cleanDivO(training$kurtosis_picth_forearm)
training$kurtosis_yaw_forearm <- cleanDivO(training$kurtosis_yaw_forearm)
training$skewness_roll_forearm <- cleanDivO(training$skewness_roll_forearm)
training$skewness_pitch_forearm <- cleanDivO(training$skewness_pitch_forearm)
training$skewness_yaw_forearm <- cleanDivO(training$skewness_yaw_forearm)
training$max_yaw_forearm <- cleanDivO(training$max_yaw_forearm)
training$min_yaw_forearm <- cleanDivO(training$min_yaw_forearm)
training$amplitude_yaw_forearm <- cleanDivO(training$amplitude_yaw_forearm)

# Look at them and see if any of them are now empty (and can, thus, be thrown out)
length(training$user_name[!is.na(training$user_name)])
length(training$cvtd_timestamp[!is.na(training$cvtd_timestamp)])
length(training$new_window[!is.na(training$new_window)])
length(training$kurtosis_roll_belt[!is.na(training$kurtosis_roll_belt)])
length(training$kurtosis_picth_belt[!is.na(training$kurtosis_picth_belt)])
length(training$kurtosis_yaw_belt[!is.na(training$kurtosis_yaw_belt)])
length(training$skewness_roll_belt[!is.na(training$skewness_roll_belt)])
length(training$skewness_roll_belt.1[!is.na(training$skewness_roll_belt.1)])
length(training$skewness_yaw_belt[!is.na(training$skewness_yaw_belt)])
length(training$max_yaw_belt[!is.na(training$max_yaw_belt)])
length(training$min_yaw_belt[!is.na(training$min_yaw_belt)])
length(training$amplitude_yaw_belt[!is.na(training$amplitude_yaw_belt)])
length(training$kurtosis_roll_arm[!is.na(training$kurtosis_roll_arm)])
length(training$kurtosis_picth_arm[!is.na(training$kurtosis_picth_arm)])
length(training$kurtosis_yaw_arm[!is.na(training$kurtosis_yaw_arm)])
length(training$skewness_roll_arm[!is.na(training$skewness_roll_arm)])
length(training$skewness_pitch_arm[!is.na(training$skewness_pitch_arm)])
length(training$skewness_yaw_arm[!is.na(training$skewness_yaw_arm)])
length(training$kurtosis_roll_dumbbell[!is.na(training$kurtosis_roll_dumbbell)])
length(training$kurtosis_picth_dumbbell[!is.na(training$kurtosis_picth_dumbbell)])
length(training$kurtosis_yaw_dumbbell[!is.na(training$kurtosis_yaw_dumbbell)])
length(training$skewness_roll_dumbbell[!is.na(training$skewness_roll_dumbbell)])
length(training$skewness_pitch_dumbbell[!is.na(training$skewness_pitch_dumbbell)])
length(training$skewness_yaw_dumbbell[!is.na(training$skewness_yaw_dumbbell)])
length(training$max_yaw_dumbbell[!is.na(training$max_yaw_dumbbell)])
length(training$min_yaw_dumbbell[!is.na(training$min_yaw_dumbbell)])
length(training$amplitude_yaw_dumbbell[!is.na(training$amplitude_yaw_dumbbell)])
length(training$kurtosis_roll_forearm[!is.na(training$kurtosis_roll_forearm)])
length(training$kurtosis_picth_forearm[!is.na(training$kurtosis_picth_forearm)])
length(training$kurtosis_yaw_forearm[!is.na(training$kurtosis_yaw_forearm)])
length(training$skewness_roll_forearm[!is.na(training$skewness_roll_forearm)])
length(training$skewness_pitch_forearm[!is.na(training$skewness_pitch_forearm)])
length(training$skewness_yaw_forearm[!is.na(training$skewness_yaw_forearm)])
length(training$max_yaw_forearm[!is.na(training$max_yaw_forearm)])
length(training$min_yaw_forearm[!is.na(training$min_yaw_forearm)])
length(training$amplitude_yaw_forearm[!is.na(training$amplitude_yaw_forearm)])

# Reformat classe as numeric
training$classe[training$classe=="A"] <- "1"
training$classe[training$classe=="B"] <- "2"
training$classe[training$classe=="C"] <- "3"
training$classe[training$classe=="D"] <- "4"
training$classe[training$classe=="E"] <- "5"
training$classe <- as.numeric(training$classe)

# These fields have no information content and can be dropped from the model.
training$kurtosis_yaw_belt
training$skewness_yaw_belt
training$kurtosis_yaw_dumbbell
training$skewness_yaw_dumbbell
training$skewness_yaw_forearm

# Well, let's try Principle Components Analysis to narrow this thing down
preProc <- preProcess(log10(training[,-155]+1), method="pca")
trainPC <- predict(preProc,log10(training[,-155]+1))
modelFit <- train(training$classe ~ ., method="glm", data=trainPC)

#* https://rstudio-pubs-static.s3.amazonaws.com/94745_f4ff1eeef6a34c1d9146f0bfb7998854.html
# https://rstudio-pubs-static.s3.amazonaws.com/136736_1a63fa96d8a244599f8db18ebc27ef74.html
# https://rpubs.com/abeasock/coursera-machine-learning-assignment
#* http://rstudio-pubs-static.s3.amazonaws.com/19805_64737972f101470fae16ea10e70b262f.html
#* http://rstudio-pubs-static.s3.amazonaws.com/23115_093b86293413411792eee518bccc1723.html
# http://xiaodanzhang.com/Coursera-PML-Quantified-Self-Project/report.html
# http://sshaikh.org/2015/05/11/human-activity-recognition-and-machine-learning/

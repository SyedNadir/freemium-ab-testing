# clear environment (objects and packages)
rm(list = ls(all.names = TRUE))

# Clear packages
pacman::p_unload(rgl)

# clear plots
if(!is.null(dev.list())) dev.off() # or graphics.off()

# clear console
cat('\014') # or ctrl + L

getwd()
setwd("D:/Data Science/git projects/freemium-ab-testing/anlytics using R")

FTrials <- read.csv('merged_trial_results_numMap.csv')
FTrials <- FTrials[c(2:12)]

str(FTrials)

library('psych')
library('dplyr')
describe(FTrials) #psych

### Task 1: Fix data quality issues identified in codebook (i.e in data understanding)

# Issue: TV "trial" is loaded as int.
# Fix: convert trial from int to factor
FTrials$trial <- factor(FTrials$trial, levels = c(0,1))

# Issue: 356 missing values=NA of "gender", "age" & "industry_code"
# Fix: As these are just 0.1% of total observations, we will ignore observations with missing values
FTrials <- FTrials[complete.cases(FTrials), ]

# transform age to fix skewness - john tukey ladder: -1/x^2   -1/x  -1/sqrt(x)  ln(x)  sqrt(x)  x  x^2  x^3
library('ggplot2')
head(sqrt(FTrials$age))

hist(FTrials$age, col = 'red') # histogram of Original data - age
hist(log(FTrials$age, 10), col = 'red') # histogram of log 10 transform
hist(FTrials$age^(1/40), col = 'red') # histogram of 40th root transform

FTrials <- mutate(FTrials, age_trans=age^(1/40))
FTrials <- FTrials[c(2:7, 12,9,10,11)]
# After transformation Age skewness reduced from 0.74 to 0.37 but NOT Zero. 
# So will use Original Age without transaformation.
df <- filter(FTrials, age < 28)
names(FTrials)
str(FTrials)

### Task 2: prepare data to feed into model
# like convert categorical features to numeric - mapping/dummy variables

# filter data for each group
FTrials_control <- filter(FTrials, group == 1) # Control
FTrials_control <- FTrials_control[-10] # drop feature "group"
str(FTrials_control)
FTrials_test <- filter(FTrials, group == 2) # test
FTrials_test <- FTrials_test[-10] # drop feature "group"
str(FTrials_test)

# save in csv file to build models
write.csv(FTrials_control, 'control_trial_results_numMap.csv')
write.csv(FTrials_test, 'test_trial_results_numMap.csv')

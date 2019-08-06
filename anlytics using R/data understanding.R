
getwd()
setwd("U:/My Docs/Data Science/14 weeks training/Projects/AB Testing")

### Task 1: Consolidate data into one table

trials <- read.csv('ab_trial_results.csv')
industry <- read.csv('user_industry.csv')
FTrials <- merge(trials, industry, by = 'user_id', all = TRUE) # merge dataframes with full outer join on user_id

# move "trial" target variable to end
names(FTrials)
FTrials <- select(FTrials, "user_id","date","source","device","payee","browser","sex","age",
                  "industry_code","group","trial")

# save it in csv for future use
write.csv(FTrials,file = "merged_trial_results.csv")



### Task 2: Pre-process (Statistical Analysis)

# check num of observations/variables and data types
str(FTrials)
summary(FTrials)

# pckgs <- c('psych','dplyr','ggplot')
# install.packages(pckgs, dependencies = TRUE)

library('dplyr')
library('psych')

# check if categorial data have unknown/unidentified values
distinct(FTrials, age, .keep_all = FALSE) #dplyr

# filter data for each group, if needed
# FTrials_control <- filter(FTrials, group == 'Control')
# FTrials_test <- filter(FTrials, group == 'Test')

glimpse(FTrials) # dplyr
describe(FTrials) #psych

# correlation of numeric fields
FTrialsNum <- select (FTrials, age, trial)
str(FTrialsNum)
names(FTrialsNum)
cor(FTrialsNum)

### Task 3: Data Visualization

library('ggplot2')

# check skewness of numeric feature
hist(FTrials$age, col = 'red') # histogram

plot(density(filter(FTrials, FTrials$age != 'NA')$age)) # kernel desnity plot

# check distribution of categorical features
barplot(table(FTrials$date), main="date distribution", ylab='Num of dates')
barplot(table(FTrials$source), main="source distribution", ylab='Num of sources')
barplot(table(FTrials$device), main="device distribution", ylab='Num of devices')
barplot(table(FTrials$payee), main="payee distribution", xlab='Type of payee' ,ylab='Num of payees')
barplot(table(FTrials$browser), main="browser distribution", ylab='Num of browsers')
barplot(table(FTrials$sex), main="gender distribution", ylab='Num of genders')
barplot(table(FTrials$industry_code), main="industry_code distribution", ylab='Num of industry_code')
barplot(table(FTrials$group), main="group distribution", ylab='Num of group')
barplot(table(FTrials$trial), main="TV trial distribution", xlab='Trial', ylab='Num of trial')

# check Age distribution across ONE categorical feature - using boxplot
boxplot(FTrials$age ~ FTrials$group, FTrials, xlab = 'Type of group', ylab = 'Age')
boxplot(FTrials$age ~ FTrials$source, FTrials, xlab = 'Type of source', ylab = 'Age')
boxplot(FTrials$age ~ FTrials$device, FTrials, xlab = 'Type of devices', ylab = 'Age')
boxplot(FTrials$age ~ FTrials$payee, FTrials, xlab = 'Type of payee', ylab = 'Age')
boxplot(FTrials$age ~ FTrials$industry_code, FTrials, xlab = 'Type of industry_code', ylab = 'Age')
boxplot(FTrials$age ~ FTrials$sex, FTrials, xlab = 'Type of gender', ylab = 'Age')
boxplot(FTrials$age ~ FTrials$trial, FTrials, xlab = 'Type of trial', ylab = 'Age')

# scatter plot
scatter.smooth(FTrials$age, FTrials$trial)


### Task 4: How to fix data quality issues

# transform age to fix skewness - john tukey ladder
head(log(FTrials$age))

hist(log(FTrials$age, 2), col = 'red') # histogram

# convert trial from int to factor
FTrials$trial <- factor(FTrials$trial, levels = c(0,1))

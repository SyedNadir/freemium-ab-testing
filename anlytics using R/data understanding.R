
# clear environment (objects and packages)
rm(list = ls(all.names = TRUE))

# clear plots
if(!is.null(dev.list())) dev.off() # or graphics.off()

# clear console
cat('\014') # or ctrl + L

getwd()
setwd("U:/My Docs/Data Science/14 weeks training/Projects/AB Testing/data")

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

#FTrials <- read.csv('merged_trial_results.csv')

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

# first convert categorical features to numeric
FTrialsNum <- select(FTrials, "date","source","device","payee","browser","sex","age",
                     "industry_code","group","trial")
directions <- NULL
directions.factor <- NULL
directions <- FTrialsNum$group
directions.factor <- factor(directions)
FTrialsNum$group <- as.numeric(directions.factor)
# FTrialsNum$date <- as.numeric(as.character(as.Date(directions.factor, format = '%m/%d/%Y'), format = '%Y%m%d'))

# write.csv(FTrialsNum,file = "merged_trial_results_numMap.csv")

FTrialsNum <- read.csv('merged_trial_results_numMap.csv')

str(FTrialsNum)
names(FTrialsNum)
cor(FTrialsNum, FTrialsNum$trial, use = 'complete.obs')

# not much information in correlation with mapping, re-try with dummy variables

#install.packages('dummies')
library('dummies')
# create dummies for all factor variables in FTrials
str(FTrials)
names(FTrials)
FTrials.dummy <- dummy.data.frame(select(FTrials, "date","source","device","payee","browser","sex", "age", "industry_code","group","trial"),
                                  names = c("date","source","device","payee","browser","sex","industry_code","group"), 
                                  sep = '.')
names(FTrials.dummy)

cor(FTrials.dummy,FTrials.dummy$trial, use = 'complete.obs')

# now select (n-1) & aggregate dummy variables
newFTrials.dummy <- select(FTrials.dummy, "date.1/10/2017", "date.1/3/2017", "date.1/4/2017", "date.1/5/2017", "date.1/6/2017", "date.1/7/2017", "date.1/8/2017", "date.1/9/2017",
       "source.Email", "source.Facebook", 
       "device.desktop", 
       "payee.Non-Primary", 
       "browser.Android (In-App)", "browser.Chrome", "browser.FireFox", "browser.IE", "browser.iOS (In-App)", "browser.Opera",
       "sex.F", 
       "age",
       "industry_code.AOR", "industry_code.BOR", "industry_code.CIL", "industry_code.DUR", "industry_code.GRT", "industry_code.ICG", "industry_code.LPC", "industry_code.LPK", "industry_code.LPP", "industry_code.MFE", "industry_code.MFG", "industry_code.PGG", "industry_code.PWO", "industry_code.RCA", "industry_code.RGA", "industry_code.SPC", "industry_code.TRV",
       "group.Control", "trial")

newFTrials.dummy <- newFTrials.dummy %>% 
  transmute(date = rowSums(.[grep("date.*", names(.))], na.rm = TRUE),
         source = rowSums(.[grep("source.*", names(.))], na.rm = TRUE),
         device = `device.desktop`,
         payee = `payee.Non-Primary`,
         browser = rowSums(.[grep("browser.*", names(.))], na.rm = TRUE),
         sex = sex.F,
         age = age,
         industry = rowSums(.[grep("industry_code.*", names(.))], na.rm = TRUE),
         group = group.Control,
         trial = trial
         )

distinct(newFTrials.dummy$industry)

# write.csv(newFTrials.dummy,file = "merged_trial_results_dummy.csv")

cor(newFTrials.dummy, newFTrials.dummy$trial, use = 'complete.obs')

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

corFtrial <- round(cor(FTrialsNum, use = 'complete.obs'), 5)
corFtrial <- cor(FTrialsNum, use = 'complete.obs')
head(corFtrial)

# package reshape is required to melt the correlaion matrix:
library('reshape2')
melted_corFtrial <- melt(corFtrial)
head(corFtrial)

ggplot(melted_corFtrial)

### Task 4: How to fix data quality issues

# transform age to fix skewness - john tukey ladder
head(log(FTrials$age))

hist(log(FTrials$age, 2), col = 'red') # histogram

# convert trial from int to factor
FTrials$trial <- factor(FTrials$trial, levels = c(0,1))

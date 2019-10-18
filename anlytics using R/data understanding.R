
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

FTrials <- read.csv('merged_trial_results.csv')
FTrials <- FTrials[c(2:12)]

### Task 2: Pre-process (Statistical Analysis)

# check num of observations/variables and data types
str(FTrials)
summary(FTrials)

# pckgs <- c('psych','dplyr','ggplot')
# install.packages(pckgs, dependencies = TRUE)

library('dplyr')
library('psych')

# check if categorial data have unknown/unidentified values
table(FTrials$source)
table(FTrials$device)
table(FTrials$payee)
table(FTrials$browser)
table(FTrials$sex)
table(FTrials$industry_code)
table(FTrials$group)
# or use dplyr distinct(FTrials, age, .keep_all = FALSE)

glimpse(FTrials) # dplyr
describe(FTrials) #psych

# correlation of numeric fields

# first convert categorical features to numeric - mapping
FTrialsNum <- select(FTrials, "user_id","date","source","device","payee","browser","sex","age",
                     "industry_code","group","trial")
FTrialsNum$date <- as.numeric(as.character(as.Date(FTrialsNum$date, format = '%m/%d/%Y'), format = '%Y%m%d'))
FTrialsNum$source <- as.numeric(factor(FTrials$source))
FTrialsNum$device <- as.numeric(factor(FTrials$device))
FTrialsNum$payee <- as.numeric(factor(FTrials$payee))
FTrialsNum$browser <- as.numeric(factor(FTrials$browser))
FTrialsNum$sex <- as.numeric(factor(FTrials$sex))
FTrialsNum$industry_code <- as.numeric(factor(FTrials$industry_code))
FTrialsNum$group <- as.numeric(factor(FTrials$group))

# save in csv for future use
write.csv(FTrialsNum,file = "merged_trial_results_numMap.csv")

#FTrialsNum <- read.csv('merged_trial_results_numMap.csv')

str(FTrialsNum)
names(FTrialsNum)
describe(FTrialsNum)

cor(FTrialsNum, use = 'complete.obs', method = 'pearson')
cor(FTrialsNum, use = 'complete.obs', method = 'kendall')
cor(FTrialsNum, use = 'complete.obs', method = 'spearman')

# Input features show very weak correlation with TV trial
# Input features do NOT show cor=1 among each other (so No duplicates)
# Now check corr with dummy variables

#install.packages('dummies')
library('dummies')
# create dummies for all factor variables in FTrials
str(FTrials)
names(FTrials)
FTrials.dummy <- dummy.data.frame(select(FTrials, "user_id","date","source","device","payee","browser","sex", "age",
                                         "industry_code","group","trial"),
                                  names = c("date","source","device","payee","browser","sex","industry_code","group"), 
                                  sep = '.')
names(FTrials.dummy)

cor(FTrials.dummy,FTrials.dummy$trial, use = 'complete.obs', method = 'pearson')
cor(FTrials.dummy,FTrials.dummy$trial, use = 'complete.obs', method = 'kendall')
cor(FTrials.dummy,FTrials.dummy$trial, use = 'complete.obs', method = 'spearman')

# Data is high dimensional 48 features so
# now select (n-1) & aggregate dummy variables
newFTrials.dummy <- select(FTrials.dummy, "user_id",
       "date.1/10/2017", "date.1/3/2017", "date.1/4/2017", "date.1/5/2017", "date.1/6/2017", "date.1/7/2017",
       "date.1/8/2017", "date.1/9/2017",
       "source.Email", "source.Facebook", 
       "device.desktop", 
       "payee.Non-Primary", "payee.Primary",
       "browser.Android (In-App)", "browser.Chrome", "browser.FireFox", "browser.IE", "browser.iOS (In-App)", "browser.Opera",
       "sex.F", 
       "age",
       "industry_code.AOR", "industry_code.BOR", "industry_code.CIL", "industry_code.DUR", "industry_code.GRT", "industry_code.ICG",
       "industry_code.LPC", "industry_code.LPK", "industry_code.LPP", "industry_code.MFE", "industry_code.MFG", "industry_code.PGG",
       "industry_code.PWO", "industry_code.RCA", "industry_code.RGA", "industry_code.SPC",
       "group.Control", "trial")

newFTrials.dummy <- newFTrials.dummy %>% 
  transmute(user_id = user_id,
         date = rowSums(.[grep("date.*", names(.))], na.rm = TRUE),
         source = rowSums(.[grep("source.*", names(.))], na.rm = TRUE),
         device = device.desktop,
         payee = rowSums(.[grep("payee.*", names(.))], na.rm = TRUE),
         browser = rowSums(.[grep("browser.*", names(.))], na.rm = TRUE),
         sex = sex.F,
         age = age,
         industry = rowSums(.[grep("industry_code.*", names(.))], na.rm = TRUE),
         group = group.Control,
         trial = trial
         )

table(newFTrials.dummy$industry)

# save in csv for future use
write.csv(newFTrials.dummy,file = "merged_trial_results_dummy.csv")

FTrials.dummy <- read.csv('merged_trial_results_dummy.csv')

describe(newFTrials.dummy)

cor(FTrials, FTrials$trial, use = 'complete.obs', method = 'pearson')
cor(FTrials, FTrials$trial, use = 'complete.obs', method = 'kendall')
cor(FTrials, FTrials$trial, use = 'complete.obs', method = 'spearman')

### Task 3: Data Visualization

library('ggplot2')

# check skewness of numeric features
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

ggplot(FTrialsNum, aes(x=age, y=trial)) + geom_point(na.rm = TRUE, color='blue', alpha=.25)

#check if data is linearly separable
ggplot(FTrialsNum, aes(x=age, y=user_id,color=factor(trial))) + geom_point()


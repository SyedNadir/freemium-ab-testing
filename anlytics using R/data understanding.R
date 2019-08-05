
getwd()
setwd("U:/My Docs/Data Science/14 weeks training/Projects/AB Testing")

# Task 1: Consolidate data into one table
trials <- read.csv('ab_trial_results.csv')
industry <- read.csv('user_industry.csv')
FTrials <- merge(trials, industry, by = 'user_id', all = TRUE) # merge dataframes with full outer join on user_id

# move "trial" target variable to end
names(FTrials)
FTrials <- select(FTrials, "user_id","date","source","device","payee","browser","sex","age",
                  "industry_code","group","trial")

# save it in csv for future use
write.csv(FTrials,file = "merged_trial_results.csv")

# task 1: Completed


# Task 2: Pre-process (Statistical Analysis)

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

# convert int to factor
FTrials$trial <- factor(FTrials$trial, levels = c(0,1))


# correlation of numeric fields
FTrialsNum <- select (FTrials, age)
str(FTrialsNum)
names(FTrialsNum)
cor(FTrialsNum)

# check distribution of numeric feature
hist(FTrials$age) # histogram

plot(density(filter(FTrials, FTrials$age != 'NA')$age)) # kernel desnity plot



# clear environment (objects and packages)
rm(list = ls(all.names = TRUE))

# Clear packages
pacman::p_unload(rgl)

# clear plots
if(!is.null(dev.list())) dev.off() # or graphics.off()

# clear console
cat('\014') # or ctrl + L

getwd()
setwd("U:/projects/data Science/freemium-AB-testing/data")

### Task 1: select the model
#        1) Could build decision tree and use as benchmark - No assumptions required
#        2) Check if data is linearly related or Not. If linear use SVR, multiple Linear regression otherwise KSVR.

### Task 2: Split data

install.packages("caTools")
library('caTools')

set.seed(123)

split <- sample.split(FTrials$trials, SplitRatio = 0.80)
training_set <- subset(FTrials, split == TRUE)
test_set <- subset(FTrials, split == FALSE)

### Task 3: Scale data if needed
### Task 4: Build/Fit Model
### Task 5: Predict/test Model
### Task 6: Assess/Evaluate Model
### Task 7: Optimize Model
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

FTrials <- read.csv('control_trial_results_dummy.csv')
FTrials <- FTrials[c(2:11)]

FTrials$trial <- factor(FTrials$trial)
FTrials$date <- factor(FTrials$date)
FTrials$source <- factor(FTrials$source)
FTrials$device <- factor(FTrials$device)
FTrials$payee <- factor(FTrials$payee)
FTrials$browser <- factor(FTrials$browser)
FTrials$sex <- factor(FTrials$sex)
FTrials$industry_code <- factor(FTrials$industry_code, level = levels(as.factor(FTrials$industry_code)))
str(FTrials)

### Task 1: select the model - check assumptions
#        1) Could build decision tree and use as benchmark - No assumptions required
#        2) Input variables are NOT related (cor=0)                           False
#        3) Input variables are normally distributed (Skewness=0/Histogram)   False
#        4) All Input variables must be numeric (Dummy)                       True
#        5) No missing values (complete.cases)                                True
#        6) TV is binary 0/1                                                  True
#        7) Data is linearly seprable (Scatter plot)                          False

### So build/Compare Decision Tree, K Nearest Neighbor, Logistic Regression & K-SVM

### Task 2: Split data

install.packages("caTools")
library('caTools')

set.seed(123)

split <- sample.split(FTrials$trial, SplitRatio = 0.80)
training_set <- subset(FTrials, split == TRUE)
test_set <- subset(FTrials, split == FALSE)

### Task 3: Scale data if needed

#######################################################################################################
### Feature Scaling - For KNN & Logistic Regression
#######################################################################################################

training_set[-10] = scale(training_set[-10]) 
# prediction boundary
test_set[-10] = scale(test_set[-10])

### Task 4: Build/Fit Model

# Build first Model - Decition Tree
library('rpart')

DTmodel <- rpart(formula = trial ~ .,
                 data = training_set,
                 method = "class")
#control = rpart.control(maxdepth = 10))
DTmodel

# Build Second Model - KNN

# fit and test both

library(class)
KNN_pred = knn(train = training_set[, -10],
               test = test_set[, -10],
               cl = training_set[, 10],
               k = 10,
               prob = TRUE)

# Build Third Model - Logistic Regression

LogisticModel = glm(formula = trial ~ .,
                    family = binomial,
                    data = training_set)
LogisticModel

# Build Fourth Model - Kernel SVM

install.packages('e1071')
library(e1071)
KSVMModel = svm(formula = trial ~ .,  # Arg 1
                data = training_set,  # Arg 2
                type = 'C-classification', # Arg 3
                kernel = 'radial')

### Task 5: Predict/test Model

# Decision Tree
DTpred = predict(DTmodel, type = 'class', newdata = test_set[-10])

# Logistic Regression
prob_pred = predict(LogisticModel, type = 'response', newdata = test_set[-10]) 
prob_pred
# Transform probabilities into 1 OR 0
glm_pred = ifelse(prob_pred > 0.5, 1, 0)  
glm_pred <- factor(glm_pred)

# K-SVM
KSVM_pred = predict(KSVMModel, newdata = test_set[-10])

#######################################################################################################
### Making the Confusion Matrix
#######################################################################################################

DTcm = table(test_set[, 10], DTpred)
DTcm 

KNNcm = table(test_set[, 10], KNN_pred)
KNNcm

glmcm = table(test_set[, 10], glm_pred)
glmcm

KSVMcm = table(test_set[, 10], KSVM_pred)
KSVMcm

### Task 6: Assess/Evaluate Model

model_accuracy <- function(cm, mode='A'){
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  
  ########################## Calculating the Accuracy  ##################################################
  if(mode=='A')
    result = sum(diag(cm))/sum(cm) # 95%
  else {
    Precision = diag/colsums
    Recall = diag / rowsums 
    result = 2 * Precision * Recall / (Precision + Recall)
  }
  return(result)
}

# Decision Tree
Accuracy = model_accuracy(DTcm, 'A')
F1_score = model_accuracy(DTcm, 'F')
#Actual & Predicted conversion rate
Aconv_rate <- mean(as.numeric(test_set$trial))
Pconv_rate <- mean(as.numeric(DTpred))

# KNN
Accuracy = model_accuracy(KNNcm, 'A')
F1_score = model_accuracy(KNNcm, 'F')
#Predicted conversion rate
Pconv_rate <- mean(as.numeric(KNN_pred))

# Logistic Regression
Accuracy = model_accuracy(glmcm, 'A')
F1_score = model_accuracy(glmcm, 'F')
#Predicted conversion rate
Pconv_rate <- mean(as.numeric(glm_pred))

# K-SVM
Accuracy = model_accuracy(KSVMcm, 'A')
F1_score = model_accuracy(KSVMcm, 'F')
#Predicted conversion rate
Pconv_rate <- mean(as.numeric(KSVM_pred))

### Task 7: Visualize Model

########################## Visualize the Decision Tree  ##################################################
install.packages("rpart.plot")
library('rpart.plot')
rpart.plot(DTmodel, box.palette = "RdBu", shadow.col = "gray", nn=TRUE)

### Task 8: Optimize Model
# TBD
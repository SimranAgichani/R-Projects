rm(list = ls())
setwd("C:/Users/simra/Downloads")
library(readxl)
d <- read_excel("TelcoChurn.xlsx",sheet='Data')
colnames(d)=tolower(make.names(colnames(d)))
str(d)

table(d$churn)
#Correlation matrix
df <- d[,]%>%select_if(is.numeric)
str(df)
library(PerformanceAnalytics)
chart.Correlation(df) 
#Data preprocessing

d$customerid <- NULL                      # Drop unique identity column
colSums(is.na(d))                         # checking null values
d$totalcharges <- ifelse(is.na(d$totalcharges), d$tenure * d$monthlycharges, d$totalcharges)    #handling null as total charges are monthly charges over the tenure of service
d$partner <- ifelse(d$partner=='Yes',1,0)
d$dependents <- ifelse(d$dependents=='Yes',1,0)
d$multiplelines <- ifelse(d$multiplelines=='Yes',1,0)
d$phoneservice <- ifelse(d$phoneservice=='Yes',1,0)
d$internetservice <- ifelse(d$internetservice=='No',0,1)
d$onlinesecurity <- ifelse(d$onlinesecurity=='Yes',1,0)
d$onlinebackup <- ifelse(d$onlinebackup=='Yes',1,0)
d$deviceprotection <- ifelse(d$deviceprotection=='Yes',1,0)
d$techsupport <- ifelse(d$techsupport=='Yes',1,0)
d$streamingtv <- ifelse(d$streamingtv=='Yes',1,0)
d$streamingmovies <- ifelse(d$streamingmovies=='Yes',1,0)
d$churn <- ifelse(d$churn=='Yes',1,0)
d$paperlessbilling <- ifelse(d$paperlessbilling=='Yes',1,0)
#d$phone.internet= ifelse(d$phoneservice=='1' & d$internetservice=='1',1,0)  #new variable for both phone and internet service


d$gender = factor(d$gender)
d$contract = factor(d$contract)
d$paymentmethod = factor(d$paymentmethod)
table(d$churn)
str(d)

#create subset
phone <- subset(d, phoneservice == "1" & internetservice=="0")

internet <-subset(d, phoneservice=="0" & internetservice=="1")

both <- subset(d, phoneservice == "1" & internetservice=="1")

#sample the data and split 75% training and 25% test
set.seed(1024)
#for phone
trainIndex_p <- sample(1:nrow(phone), size=round(0.75*nrow(phone)), replace=FALSE)
train_p <- phone[trainIndex_p,]
test_p  <- phone[-trainIndex_p,]
dim(train_p); dim(test_p)

logit_p  <- glm(churn ~ dependents + tenure + multiplelines + contract + 
                paymentmethod + monthlycharges, family=binomial (link="logit"), data=train_p)

exp(logit_p$coef)                                           # Odds ratio
exp(cbind(OddsRatio = coef(logit_p), confint(logit_p)))       # Odds Ratio and 95% C.I.

test_xp <- test_p[ , c(1:19)]

predlogit_p <-predict(logit_p, newdata=test_xp, type="response")

#install.packages("pROC")
library(pROC)

# Create ROC curve
roc_p <- roc(test_p$churn, predlogit_p)

# Get optimal threshold
c_p <- coords(roc_p, "best", ret=c("threshold", "specificity", "sensitivity", "J"))
threshold_p <- c_p$threshold

print(threshold_p) 
predlogit_p <- ifelse(predlogit_p>threshold_p, 1, 0)


ClassificationError <- mean(predlogit_p != test_p$churn) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))        # Accuraty rate

# Confusion matrix
cm <- table(test_p$churn, predlogit_p)   
tp <- cm[2,2] # True positives
tn <- cm[1,1] # True negatives
fp <- cm[1,2] # False positives
fn <- cm[2,1] # False negatives

# Accuracy
accuracy <- (tp + tn) / (tp + tn + fp + fn)
# Precision
precision <- tp / (tp + fp)
# Recall (Sensitivity)
recall <- tp / (tp + fn)
# F1-score
f1_score <- 2 * ((precision * recall) / (precision + recall))

# install.packages("ROCR")
library(ROCR)
pr_p <- prediction(predlogit_p, test_p$churn)
prf_p <- performance(pr_p, measure="tpr", x.measure="fpr")
plot(prf_p)

# ROC plot: TPR vs FPR
auc <- performance(pr_p, measure="auc")
auc <- auc@y.values[[1]] # Area under the curve
auc


# Print the results
cat("Accuracy = ", round(accuracy, 2), "\n")
cat("Precision = ", round(precision, 2), "\n")
cat("Recall = ", round(recall, 2), "\n")
cat("F1-score = ", round(f1_score, 2), "\n")
cat("AUC = ", round(auc, 2), "\n")   


# Multicollinearity test
library(car)
vif(logit_p)

#Durbin Watson Test for Autocorrelation
library(lmtest)
durbinWatsonTest(logit_p)


#internet service
trainIndex_i <- sample(1:nrow(internet), size=round(0.75*nrow(internet)), replace=FALSE)
train_i <- internet[trainIndex_i,]
test_i  <- internet[-trainIndex_i,]
dim(train_i); dim(test_i)
logit_i  <- glm(churn ~ dependents + tenure + contract + onlinebackup + 
                  deviceprotection + paymentmethod + monthlycharges + 
                  streamingtv, family=binomial (link="logit"), data=train_i)

exp(logit_i$coef)                                           # Odds ratio
exp(cbind(OddsRatio = coef(logit_i), confint(logit_i)))       # Odds Ratio and 95% C.I.

test_xi <- test_i[ , c(1:19)]
predlogit_i <-predict(logit_i, newdata=test_xi, type="response")


# Create ROC curve
roc_i <- roc(test_i$churn, predlogit_i)

# Get optimal threshold
c_i <- coords(roc_i, "best", ret=c("threshold", "specificity", "sensitivity", "J"))
threshold_i <- c_i$threshold

print(threshold_i) 
predlogit_i <- ifelse(predlogit_i>threshold_i, 1, 0)


ClassificationError <- mean(predlogit_i != test_i$churn) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))        # Accuraty rate

# Confusion matrix
cm <- table(test_i$churn, predlogit_i)   
tp <- cm[2,2] # True positives
tn <- cm[1,1] # True negatives
fp <- cm[1,2] # False positives
fn <- cm[2,1] # False negatives

# Accuracy
accuracy <- (tp + tn) / (tp + tn + fp + fn)
# Precision
precision <- tp / (tp + fp)
# Recall (Sensitivity)
recall <- tp / (tp + fn)
# F1-score
f1_score <- 2 * ((precision * recall) / (precision + recall))


pr_i <- prediction(predlogit_i, test_i$churn)
prf_i <- performance(pr_i, measure="tpr", x.measure="fpr")
plot(prf_i)

# ROC plot: TPR vs FPR
auc <- performance(pr_i, measure="auc")
auc <- auc@y.values[[1]] # Area under the curve
auc


# Print the results
cat("Accuracy = ", round(accuracy, 2), "\n")
cat("Precision = ", round(precision, 2), "\n")
cat("Recall = ", round(recall, 2), "\n")
cat("F1-score = ", round(f1_score, 2), "\n")
cat("AUC = ", round(auc, 2), "\n")   

# Multicollinearity test

vif(logit_i)

#Durbin Watson Test for Autocorrelation

durbinWatsonTest(logit_i)

# for both
trainIndex_b <- sample(1:nrow(both), size=round(0.75*nrow(both)), replace=FALSE)
train_b <- both[trainIndex_b,]
test_b  <- both[-trainIndex_b,]
dim(train_b); dim(test_b)
logit_b  <- glm(churn ~ dependents + tenure + contract + multiplelines + 
                  onlinesecurity+ onlinebackup+ deviceprotection+ techsupport+ 
                  paymentmethod + monthlycharges+ streamingtv, family=binomial (link="logit"), data=train_b)

exp(logit_b$coef)                                           # Odds ratio
exp(cbind(OddsRatio = coef(logit_b), confint(logit_b)))       # Odds Ratio and 95% C.I.

test_xb <- test_b[ , c(1:19)]

predlogit_b <-predict(logit_b, newdata=test_xb, type="response")


# Create ROC curve
roc_b <- roc(test_b$churn, predlogit_b)

# Get optimal threshold
c_b <- coords(roc_b, "best", ret=c("threshold", "specificity", "sensitivity", "J"))
threshold_b <- c_b$threshold

print(threshold_b) 
predlogit_b <- ifelse(predlogit_b>threshold_b, 1, 0)


ClassificationError <- mean(predlogit_b != test_b$churn) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))        # Accuraty rate

# Confusion matrix
cm <- table(test_b$churn, predlogit_b)   
tp <- cm[2,2] # True positives
tn <- cm[1,1] # True negatives
fp <- cm[1,2] # False positives
fn <- cm[2,1] # False negatives

# Accuracy
accuracy <- (tp + tn) / (tp + tn + fp + fn)
# Precision
precision <- tp / (tp + fp)
# Recall (Sensitivity)
recall <- tp / (tp + fn)
# F1-score
f1_score <- 2 * ((precision * recall) / (precision + recall))


pr_b <- prediction(predlogit_b, test_b$churn)
prf_b <- performance(pr_b, measure="tpr", x.measure="fpr")
plot(prf_b)

# ROC plot: TPR vs FPR
auc <- performance(pr_b, measure="auc")
auc <- auc@y.values[[1]] # Area under the curve
auc

# Print the results
cat("Accuracy = ", round(accuracy, 2), "\n")
cat("Precision = ", round(precision, 2), "\n")
cat("Recall = ", round(recall, 2), "\n")
cat("F1-score = ", round(f1_score, 2), "\n")
cat("AUC = ", round(auc, 2), "\n")   

# Multicollinearity test

vif(logit_b)

#Durbin Watson Test for Autocorrelation

durbinWatsonTest(logit_b)

library(stargazer)

stargazer(logit_p, logit_i, logit_b, type="text", single.row=TRUE)

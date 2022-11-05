# the script to make the decision tree
library(data.table)
data = fread("Churn_Modelling.csv")
library(ggplot2)

data$Geography = factor(data$Geography)
data$Gender = factor(data$Gender)
data$HasCrCard = factor(data$HasCrCard)
data$RowNumber = NULL
data$CustomerId = NULL
data$Surname  = NULL

data$Exited = ifelse(data$Exited==0,"No","Yes")
data$HasCrCard = ifelse(data$HasCrCard==0,"No","Yes")
data$IsActiveMember = ifelse(data$IsActiveMember==0,"No","Yes")
data$Exited = factor(data$Exited)
data$HasCrCard = factor(data$HasCrCard)
data$IsActiveMember = factor(data$IsActiveMember)

ger_mean_balance = mean(data$Balance[data$Geography=="Germany"])
spain_mean_balance = mean(data$Balance[data$Geography=="Spain"])
france_mean_balance = mean(data$Balance[data$Geography=="France"])

data$normalised_balance = ifelse(data$Geography=="Germany",data$Balance/ger_mean_balance,ifelse(data$Geography=="Spain",data$Balance/spain_mean_balance,data$Balance/france_mean_balance))

# to check if all of them is normalised
sum(data$Balance[data$Geography=="Germany"]/ger_mean_balance)  == sum(data$normalised_balance[data$Geography=="Germany"])
sum(data$Balance[data$Geography=="Spain"]/spain_mean_balance)  == sum(data$normalised_balance[data$Geography=="Spain"])
sum(data$Balance[data$Geography=="France"]/france_mean_balance)  == sum(data$normalised_balance[data$Geography=="France"])
# since all returns true all the balance data has been normalised based on location

# now we are going to normalise the credit score based on age 

data$normalised_creditscore = data$CreditScore/data$Age
(sum(data$CreditScore)/sum(data$Age))==sum(data$normalised_creditscore)
data$balance_per_product = data$Balance/data$NumOfProducts

data$salary_credit_score_ratio = data$EstimatedSalary/data$CreditScore



#library(e1071)
#skewness(data$balance_per_product)
summary(data)

# spliting into train and test data set
library(caTools)
train = sample.split(Y = data$Exited, SplitRatio = 0.7)
trainset <- subset(data, train == T)
testset <- subset(data, train == F)

data[Exited=="Yes",.N]
trainset[Exited=="Yes",.N]
# trainset exactly 0.7 of data


set.seed(2004)
library(rpart)
max_tree = rpart(Exited~.,data = trainset,method = 'class',control = rpart.control(minsplit = 2,cp = 0))

plotcp(max_tree)
printcp(max_tree)


# check with the code in slide
# check if the cp value is actually 0.0066 with the automated code
###########################################
cv_errorcap = max_tree$cptable[which.min(max_tree$cptable[,"xerror"]),"xerror"]+
              max_tree$cptable[which.min(max_tree$cptable[,"xerror"]),"xstd"]
i = 1
j = 4
while(max_tree$cptable[i,j]>cv_errorcap)
{
  i = i+1
}
optimal_cp = ifelse(i>1,sqrt(max_tree$cptable[i,1] * max_tree$cptable[i-1,1]),1)
optimal_cp
#########################################
prune_train <- prune(max_tree, cp = optimal_cp)

library(rpart.plot)
rpart.plot(prune_train, nn= T, main = "unbalanced Pruned Tree")

prune_train$variable.importance


# predict the exited variable with the prune tree train 
predict_test <- predict(prune_train, newdata = testset, type = "class")

table(testset$Exited, predict_test, deparse.level = 2)
##############################################
#calculating the accuracy 
overall_accuracy = mean(predict_test==testset$Exited)
overall_accuracy
sensitivity = 253/(253+358)
sensitivity
specificity = 2297/(2297+92)
specificity

####################################################
# try using the same number of yes and no 
data[Exited=="Yes",.N]
data[Exited=="No",.N]

#select 2037 no randomly
data_train_no = trainset[Exited=="No"]
data_train_yes = trainset[Exited=="Yes"]

library(dplyr)
data_train_no = sample_n(data_train_no, 1426)
data_train_balanced =  rbind(data_train_no, data_train_yes)
# train the model on this balanced data but still test on the test_train 

set.seed(2004)
library(rpart)
max_tree_balanced = rpart(Exited~.,data = data_train_balanced,method = 'class',control = rpart.control(minsplit = 2,cp = 0))

plotcp(max_tree_balanced,main = "Balanced dataset")
###############################################
cv_errorcap = max_tree_balanced$cptable[which.min(max_tree_balanced$cptable[,"xerror"]),"xerror"]+
  max_tree_balanced$cptable[which.min(max_tree_balanced$cptable[,"xerror"]),"xstd"]
i = 1
j = 4
while(max_tree_balanced$cptable[i,j]>cv_errorcap)
{
  i = i+1
}
optimal_cp = ifelse(i>1,sqrt(max_tree_balanced$cptable[i,1] * max_tree_balanced$cptable[i-1,1]),1)
optimal_cp
################################################
prune_train_balanced <- prune(max_tree_balanced, cp = optimal_cp)

library(rpart.plot)
rpart.plot(prune_train_balanced, nn= T, main = "Balanced Pruned Tree")

(prune_train_balanced$variable.importance)

predict_test_balanced <- predict(prune_train_balanced, newdata = testset, type = "class")

table(testset$Exited, predict_test_balanced, deparse.level = 2)

#########################################################
# calculating the accuracy
overall_accuracy = mean(predict_test_balanced==testset$Exited)
overall_accuracy
sensitivity = 412/(412+199)
sensitivity
specificity = 2008/(2008+381)
specificity
#####################################################


# work on logistic regression
# first we will work on normal data firs t

normal_logistic <- glm(Exited~.,family =binomial, data = trainset)
normal_logistic = step(normal_logistic)
summary(normal_logistic)

OR <- exp(coef(normal_logistic))
OR

OR.CI <- exp(confint(normal_logistic))
OR.CI


prob_norm <- predict(normal_logistic,newdata = testset, type = 'response')


testset$prob = prob_norm

# Set the threshold for predicting Y = 1 based on probability.
threshold <- 0.5
y.hat_norm <- ifelse(prob_norm > threshold, "Yes", "No")
table(testset$Exited, y.hat_norm, deparse.level = 2)
#########################################################
# calculating the accuracy
overall_accuracy = mean(y.hat_norm==testset$Exited)
overall_accuracy
sensitivity = 136/(136+475)
sensitivity
specificity = 2311/(2311+78)
specificity
#####################################################


plot(testset$Exited,testset$prob,xlab = "Exited",ylab= "Prob",main = "Prob against cateogry")
quantile(testset$prob[testset$Exited=="No"])

# try threshold as 0.3 


# try find optimal threshold using CART model
library(rpart)
threshold_tree = rpart(Exited~prob,data = testset,method = 'class',control = rpart.control(minsplit = 2,cp = 0))
library(rpart.plot)
rpart.plot(threshold_tree)
plotcp(threshold_tree)
threshold_tree = prune(threshold_tree,cp = 0.038)

threshold <- 0.3
y.hat_norm <- ifelse(prob_norm > threshold, "Yes", "No")
table(testset$Exited, y.hat_norm, deparse.level = 2)
#########################################################
# calculating the accuracy
overall_accuracy = mean(y.hat_norm==testset$Exited )
overall_accuracy
sensitivity = 314/(314+297)
sensitivity
specificity = 2011/(2011+378)
specificity
#####################################################


# now do the balance part




balance_logistic <- glm(Exited~.,family =binomial, data = data_train_balanced)
balance_logistic = step(balance_logistic)
summary(balance_logistic)

OR <- exp(coef(balance_logistic))
OR

OR.CI <- exp(confint(balance_logistic))
OR.CI


prob_balance <- predict(balance_logistic,newdata = testset, type = 'response')
testset$prob_balance = prob_balance
threshold <- 0.5
y.hat_balance <- ifelse(prob_balance > threshold, "Yes", "No")
table(testset$Exited, y.hat_balance, deparse.level = 2)
#########################################################
# calculating the accuracy
overall_accuracy = mean(y.hat_balance==testset$Exited )
overall_accuracy
sensitivity = 420/(420+191)
sensitivity
specificity = 1683/(1683+706)
specificity
#####################################################

plot(testset$Exited,testset$prob_balance,xlab = "Exited",ylab= "Prob",main = "Blanced prob against cateogry")
quantile(testset$prob_balance[testset$Exited=="No"])
quantile(testset$prob_balance[testset$Exited=="Yes"])

threshold_tree = rpart(Exited~prob_balance,data = testset,method = 'class',control = rpart.control(minsplit = 2,cp = 0))
rpart.plot(threshold_tree)
plotcp(threshold_tree)
printcp(threshold_tree)
threshold_tree = prune(threshold_tree,cp = cp)
cp= sqrt(0.02454992*0.00709220)



threshold <- 0.55
y.hat_balance <- ifelse(prob_balance > threshold, "Yes", "No")
table(testset$Exited, y.hat_balance, deparse.level = 2)
#########################################################
# calculating the accuracy
overall_accuracy = mean(y.hat_balance==testset$Exited )
overall_accuracy
sensitivity = 322/(322+289)
sensitivity
specificity = 1981/(1981+408)
specificity
#####################################################
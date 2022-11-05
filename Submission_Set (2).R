# ========================================================================================================
# Title:        A deep dive into churn rate
# Purpose:      Identify moments of impact in regards to customer churn
# Team:         Group 5
# Members:      Chen Jiaru, Myat Thu Soe, Oh En Qi Sherman, Venkat Subramanian
# DOC:          30-10-2020
# Topics:       CART, Logistic Regression
# Data Source:  churn_modelling.csv
# Packages:     data.table, arsenal, ggplot2, ggpubr, rpart, rpart.plot, dplyr, caTools
#=========================================================================================================
##--------------------------------------------------------------------------------------------------------
## Setting up the workspace
##--------------------------------------------------------------------------------------------------------

library(data.table)
library(arsenal)
library(ggplot2)
library(ggpubr)
library(caTools)
library(rpart)
library(rpart.plot)
library(dplyr)

setwd("~/NTU_Y2S1/BC2406 Analytics I Visual & Predictive Technology/Project/Team Assignment and Project")

##--------------------------------------------------------------------------------------------------------
## Working with the dataset 
##--------------------------------------------------------------------------------------------------------

# Importing and doing preliminary cleaning of dataset
churn_model.dt <- fread("Data/Churn_Modelling.csv")

# Generating summary to see if any preliminary cleaning is required
churn_model2.dt <- copy(churn_model.dt)
summary(churn_model.dt)

# Converting Gender variable to binary
churn_model2.dt[, Gender := ifelse(Gender == "Male", 1, 0)]

# Factorising relevant variables
churn_model2.dt$Geography <- factor(churn_model2.dt$Geography)
churn_model2.dt$Gender <- factor(churn_model2.dt$Gender)
churn_model2.dt$HasCrCard <- factor(churn_model2.dt$HasCrCard)
churn_model2.dt$IsActiveMember <- factor(churn_model2.dt$IsActiveMember)
churn_model2.dt$Exited <- factor(churn_model2.dt$Exited)

# Working with subset of useful variables
churn_model2.dt <- subset(churn_model2.dt, select = -c(RowNumber, CustomerId, Surname))
##--------------------------------------------------------------------------------------------------------
## Data exploration 
##--------------------------------------------------------------------------------------------------------

##........................................................................................................
## Generating (preliminary) summary tables 
##........................................................................................................

# Generating summary of all the variables after preliminary cleaning
summary(churn_model2.dt)

# Generating summary of the predictor variables against outcome variable ("Exited")
table_one <- tableby(Exited ~ ., data = churn_model2.dt)
summary(table_one, title = "Summary")

##........................................................................................................
## Generating univariate graphs (categorical variables)
##........................................................................................................

# Checking if there is any imbalance in the dataset
a1 <- ggplot(churn_model2.dt, aes(Exited)) +
  geom_bar(fill="#FF7068") +
  labs(title = "Proportion of Cases: Exited vs Not Exited", y="Number of cases") +
  scale_x_discrete(labels=c("No","Yes"))
a1

# Checking if there is correlation between IsActiveMember and Exited
a2 <- ggplot(churn_model2.dt, aes(Exited)) +
  geom_bar(position = 'fill', aes(fill = IsActiveMember)) +
  labs(y = "Proportion of customers") +
  labs(title = "Impact of Member's Activity on customer exiting") +
  scale_fill_manual(name = "Active Member", labels = c("No","Yes"), 
                    values=c("#02AEAE", "#FF7068")) +
  scale_x_discrete(labels=c("No","Yes"))
a2

# Checking if there is correlation between HasCrCard and Exited
a3 <- ggplot(churn_model2.dt, aes(Exited)) +
  geom_bar(position = 'fill', aes(fill = HasCrCard)) +
  labs(title = "Impact of Credit Card ownership on customer exiting",
       y = "Proportion of customers") +
  scale_fill_manual(name = "Owns Credit Card", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068")) +
  scale_x_discrete(labels=c("No","Yes"))
a3

# Checking if there is correlation between Gender and Exited
a4 <- ggplot(churn_model2.dt, aes(Exited)) +
  geom_bar(position = 'fill', aes(fill = Gender)) +
  labs(title = "Impact of Gender on customer exiting", y = "Proportion of customers") +
  scale_fill_manual(name = "Gender", labels = c("Female","Male"),
                    values=c("#02AEAE", "#FF7068")) +
  scale_x_discrete(labels=c("No","Yes"))
a4

# Checking if there is correlation between Geography and Exited
a5 <- ggplot(churn_model2.dt, aes(Geography)) +
  geom_bar(position = 'fill', aes(fill = Exited)) +
  labs(title = "Impact of Geography on customer exiting", y = "Proportion of customers") +
  scale_fill_manual(name = "Exited", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
a5

##........................................................................................................
## Generating univariate graphs (continuous variables)
##........................................................................................................

# Prepartion of stat_summary for pointrange
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# Checking if there is correlation between Balance and Exited
b1 <- ggplot(churn_model2.dt, aes(Exited, Balance, fill=Exited)) +
  geom_violin() +
  stat_summary(fun.data=data_summary, geom="pointrange", color="black") +
  labs(title = "Impact of Balance on customer exiting") +
  scale_fill_manual(values=c("#02AEAE", "#FF7068")) +
  scale_x_discrete(labels=c("No","Yes")) +
  theme(legend.position = "none")
b1

# Checking for imbalance in cases of "Exited" when Balance = 0 
b2 <- ggplot(churn_model2.dt[Balance==0], aes(Exited)) +
  geom_bar(fill = "#FF7068") +
  labs(title = "Impact of 0 Balance on customer exiting", 
       y = "Number of customers") +
  scale_x_discrete(labels=c("No","Yes"))
b2


# Checking if there is correlation between Age and Exited
b3 <- ggplot(churn_model2.dt, aes(Exited, Age, fill=Exited)) +
  geom_violin() +
  stat_summary(fun.data=data_summary, geom="pointrange", color="black") +
  labs(title = "Impact of Age on customer exiting") +
  scale_fill_manual(values=c("#02AEAE", "#FF7068")) +
  scale_x_discrete(labels=c("No","Yes")) +
  theme(legend.position = "none")
b3

# Checking if there is correlation between NumOfProducts and Exited
b4 <- ggplot(churn_model2.dt, aes(factor(NumOfProducts))) +
  geom_bar(position = "fill",aes(fill = Exited)) +
  labs(title = "Impact of number of Products on customer exiting",
       x = "Number of Products", y = "Proportion of Customers") +
  scale_fill_manual(name = "Exited", labels = c("No","Yes"), 
                    values=c("#02AEAE", "#FF7068"))
b4

# Is the sample size for NumOfProducts == 3 | 4 sufficiently large?
nrow(churn_model2.dt[NumOfProducts == 3 | NumOfProducts == 4])

# Checking if there is correlation between CreditScore and Exited <Appendix>
b5 <- ggplot(churn_model2.dt, aes(Exited, CreditScore, fill=Exited)) +
  geom_boxplot() +
  labs(title = "Impact of Credit Score on customer exiting",
       x = "Exited", y = "Credit Score") +
  scale_fill_manual(values=c("#02AEAE", "#FF7068")) +
  scale_x_discrete(labels=c("No","Yes")) +
  theme(legend.position = "none")
b5

# Checking if there is correlation between Tenure and Exited <Appendix>
b6 <- ggplot(churn_model2.dt, aes(factor(Tenure), fill = Exited)) +
  geom_bar(position = 'fill') +
  labs(title = "Impact of Tenure on customer exiting", 
       x = "Tenure", y = "Proportion of Customers") +
  scale_fill_manual(name = "Exited", labels = c("No","Yes"), 
                    values=c("#02AEAE", "#FF7068"))
b6

# Checking if there is correlation between EstimatedSalary and Exited <Appendix>
b7 <- ggplot(churn_model2.dt, aes(Exited, EstimatedSalary, fill=Exited)) +
  geom_boxplot() +
  labs(title = "Impact of Estimated Salary on customer exiting") +
  scale_fill_manual(values=c("#02AEAE", "#FF7068")) +
  scale_x_discrete(name = "Exited", labels = c("No","Yes")) +
  theme(legend.position = "none")
b7

##........................................................................................................
## Generating bivariate graphs
##........................................................................................................

# Geography specific analysis ----------------------------------------------------------------------------

# Is there a (relevant) skew in distribution of Balance by Geography?
c1 <- ggplot(churn_model2.dt, aes(Geography, Balance)) +
  geom_boxplot(aes(fill=Geography)) +
  labs(title = "Distribution of Balance by Geography") +
  scale_fill_manual(values = c("#02AEAE", "#FF7068", "#F29724")) +
  theme(legend.position = "none")
c1

# Is there a (relevant) skew in distribution of NumOfProducts by Geography? <Appendix>
c2 <- ggplot(churn_model2.dt, aes(Geography, NumOfProducts)) +
  geom_boxplot(aes(fill=Geography)) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  labs(title = "Distribution of Number of Products by Geography")
c2

# Is there a (relevant) skew in distribution of Estimated Salary by Geography? <Appendix>
c3 <- ggplot(churn_model2.dt, aes(Geography, EstimatedSalary)) +
  geom_boxplot(aes(fill=Geography)) +
  labs(title = "Distribution of Estimated Salary by Geography") +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))
c3

# Is there a (relevant) skew in distribution of Age by Geography? <Appendix>
c4 <- ggplot(churn_model2.dt, aes(Geography, Age)) +
  geom_boxplot(aes(fill=Geography)) +
  labs(title = "Distribution of Age by Geography") +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))
c4

# Gender specific analysis -------------------------------------------------------------------------------

# Is there a (relevant) skew in distribution of Balance by Gender? <Appendix>
d1 <- ggplot(churn_model2.dt, aes(Gender, Balance, fill=Exited)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("#02AEAE", "#FF7068"),
                    name = "Exited", labels = c("No","Yes")) +
  labs(title = "Balance against Gender", x = "Gender", y = "Balance") +
  scale_x_discrete(labels=c("Female","Male"))

d1

# Is there a (relevant) skew in distribution of Age by Gender?
d2 <- ggplot(churn_model2.dt, aes(Gender, Age, fill=Exited)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("#02AEAE", "#FF7068"),
                    name = "Exited", labels = c("No","Yes")) +
  labs(title = "Impact of Age against Gender on Exited") +
  scale_x_discrete(labels=c("Female","Male"))
d2 

# Age specific analysis ----------------------------------------------------------------------------------

# What is the distribution of Age like for all cases?
e1 <- ggplot(churn_model2.dt, aes(Age)) +
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1,
                 colour="black", fill="white") +
  geom_vline(aes(xintercept=median(Age)),
             color="red", linetype="dashed", size=1) +
  geom_density(alpha=.6, fill="#FF7068") +
  theme(legend.position="none") +
  clean_theme()
e1

# What is the distribution of Age like for Exited vs non-Exited cases?
e2 <- ggplot(churn_model2.dt, aes(Age, Exited, fill=Exited)) +
  geom_boxplot() +
  scale_fill_manual(name = "Exited", labels = c("No","Yes"), 
                    values=c("#02AEAE", "#FF7068")) +
  scale_y_discrete(labels=c("Yes","No")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position = "bottom")
e2

ggarrange(e1, e2, nrow=2, heights = c(2,1), align = "hv",
          labels = c("A","B"), common.legend = TRUE, legend = "bottom")

# Checking if there is correlation between Age against CreditScore on Exited
e3 <- ggplot(churn_model2.dt, aes(Age, CreditScore, fill = Exited)) +
  geom_smooth(method=lm, alpha=0.2) +
  scale_fill_manual(values = c("#02AEAE", "#FF7068"),
                    name = "Exited", labels = c("No","Yes")) +
  labs(title = "Impact of Credit Score against Age on Exited", x = "Age", y = "Credit Score")
e3

# Checking if there is correlation between Age against Balance on Exited <Appendix>
e4 <- ggplot(churn_model2.dt, aes(Age, Balance, fill = Exited)) +
  geom_smooth(method=lm, alpha=0.2) +
  scale_fill_manual(values = c("salmon", "#5ab4ac"),
                    name = "Exited", labels = c("No","Yes")) +
  labs(title = "Balance against Age", x = "Age", y = "Balance")
e4

# IsActiveMember specific analysis -----------------------------------------------------------------------

# Is there a (relevant) skew in distribution of Balance by Active Member on Exited?
f1 <- ggplot(churn_model2.dt, aes(IsActiveMember, Balance)) +
  geom_boxplot(aes(fill=Exited)) +
  labs(title = "Balance",
       x="Active Customer") +
  scale_fill_manual(values = c("#02AEAE", "#FF7068"),
                    name = "Exited", labels = c("No","Yes")) +
  scale_x_discrete(labels=c("No","Yes"))
f1

# Is there a (relevant) skew in distribution of Age by Active Member on Exited?
f2 <- ggplot(churn_model2.dt, aes(IsActiveMember, Age)) +
  geom_boxplot(aes(fill=Exited)) +
  labs(title = "Age",
       x="Active Customer") +
  scale_fill_manual(values = c("#02AEAE", "#FF7068"),
                    name = "Exited", labels = c("No","Yes")) +
  scale_x_discrete(labels=c("No","Yes"))
f2

# Side by side graph of distribution of Age and Balance against ActiveMember 
ggarrange(f1, f2,
          ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

# Is there a correlation between 3 variables Balance (Where Balance != 0), 
# Age and Active Member on Exited? <Appendix>
f3 <- ggplot(churn_model2.dt[Balance != 0], 
             aes(x=Age, y=Balance, color = Exited, shape = IsActiveMember)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = c("#02AEAE", "#FF7068"), labels = c("No","Yes")) +
  scale_shape_manual(name = "Active Member", values = c(17,15), labels = c("No","Yes")) +
  geom_vline(xintercept = 65, color = "red", linetype="dashed", size=1) +
  labs(title = "Balance against Age and Active Member on Exited")
f3

# What is the percentage of those above 65, balance != 0, not exited and active 
# compared to all above 65, balance != 0 and not exited?
nrow(churn_model2.dt[Balance!=0 & Age >=65& Exited == 0 & IsActiveMember == 1])/
  nrow(churn_model2.dt[Balance!=0 & Exited == 0 & Age >=65])

# Balance specific analysis ------------------------------------------------------------------------------

# Is there correlation between NumOfProducts against Balance on Exited?
g1 <- ggplot(churn_model2.dt, aes(Balance, NumOfProducts, fill=Exited)) +
  geom_smooth(method=lm, alpha=0.2) +
  scale_fill_manual(values = c("#02AEAE", "#FF7068"),
                    name = "Exited", labels = c("No","Yes")) +
  labs(title = "Impact of Number of Products against Balance on Exited", 
       y = "Number of Products")
g1

# Is there is correlation between Tenure against Balance on Exited? <Appendix>
g2 <- ggplot(churn_model2.dt, aes(Balance, Tenure)) +
  geom_smooth(method=lm,aes(fill = Exited)) +
  labs(title = "Tenure Against Balance")  +
  scale_fill_manual(values = c("salmon", "#5ab4ac"),
                    name = "Exited", labels = c("No","Yes"))
g2

# EstimatedSalary specific analysis ----------------------------------------------------------------------

# What is the distribution of estimated salary?
h1 <- ggplot(churn_model2.dt, aes(EstimatedSalary)) +
  geom_histogram(binwidth=2000, fill="salmon") +
  labs(title = "Distribution of EstimatedSalary")
h1

# Is there a correlation between CreditScore against Estimated Salary on Exited?
h2 <- ggplot(churn_model2.dt, aes(EstimatedSalary, CreditScore, fill = Exited)) +
  geom_smooth(method=lm, alpha=0.2) +
  scale_fill_manual(values = c("#02AEAE", "#FF7068"),
                    name = "Exited", labels = c("No","Yes")) +
  labs(title = "Impact of Credit Score Against Estimated Salary on Exited", y = "Credit Score",
       x = "Estimated Salary")
h2

# Is there a correlation between Balance against Estimated Salary on Exited? <Appendix>
h3 <- ggplot(churn_model2.dt, aes(EstimatedSalary, Balance)) +
  geom_smooth(method=lm,aes(fill = Exited)) +
  labs(title = "Balance Against Estimated Salary",
       y = "Balance", x = "Estimated Salary") +
  scale_fill_manual(values = c("#02AEAE", "#FF7068"),
                    name = "Exited", labels = c("No","Yes"))
h3

# Is there a correlation between NumOfProducts against Estiamted Salary on Exited? <Appendix>
h4 <- ggplot(churn_model2.dt, aes(EstimatedSalary, NumOfProducts)) +
  geom_point(aes(color = Exited)) +
  labs(title = "Number of products Against Estimated Salary",
       y = "Number of Products", x = "Estimated Salary") +
  scale_color_manual(values = c("#02AEAE", "#FF7068"),
                    name = "Exited", labels = c("No","Yes"))
h4

##--------------------------------------------------------------------------------------------------------
## Data preparation
##--------------------------------------------------------------------------------------------------------
# Removing redundant variables <previously ran>-----------------------------------------------------------
churn_model2.dt[, RowNumber:=NULL]
churn_model2.dt[, CustomerId:=NULL]
churn_model2.dt[, Surname:=NULL]

# Categorising categorical variables <previously ran>-----------------------------------------------------
churn_model2.dt$Geography <- factor(churn_model2.dt$Geography)
churn_model2.dt$Gender <- factor(churn_model2.dt$Gender)
churn_model2.dt$HasCrCard <- factor(churn_model2.dt$HasCrCard)
churn_model2.dt$IsActiveMember <- factor(churn_model2.dt$IsActiveMember)
churn_model2.dt$Exited <- factor(churn_model2.dt$Exited)

# Feature Engineering-------------------------------------------------------------------------------------

# Creating variable NormalisedBalance
ger_mean_balance = mean(churn_model2.dt$Balance[churn_model2.dt$Geography=="Germany"])
spain_mean_balance = mean(churn_model2.dt$Balance[churn_model2.dt$Geography=="Spain"])
france_mean_balance = mean(churn_model2.dt$Balance[churn_model2.dt$Geography=="France"])

churn_model2.dt$NormalisedBalance = ifelse(churn_model2.dt$Geography=="Germany",
            churn_model2.dt$Balance/ger_mean_balance,ifelse(churn_model2.dt$Geography=="Spain",
            churn_model2.dt$Balance/spain_mean_balance,churn_model2.dt$Balance/france_mean_balance))

# to check if all of them is normalised
sum(churn_model2.dt$Balance[churn_model2.dt$Geography=="Germany"]/ger_mean_balance)  == 
  sum(churn_model2.dt$NormalisedBalance[churn_model2.dt$Geography=="Germany"])
sum(churn_model2.dt$Balance[churn_model2.dt$Geography=="Spain"]/spain_mean_balance)  == 
  sum(churn_model2.dt$NormalisedBalance[churn_model2.dt$Geography=="Spain"])
sum(churn_model2.dt$Balance[churn_model2.dt$Geography=="France"]/france_mean_balance)  == 
  sum(churn_model2.dt$NormalisedBalance[churn_model2.dt$Geography=="France"])
# since all returns true all the balance data has been normalised based on location


# Creating variable CreditScorePerAge 
churn_model2.dt$CreditScorePerAge = churn_model2.dt$CreditScore/churn_model2.dt$Age

# Creating variable BalancePerProduct
churn_model2.dt$balancePerProduct = churn_model2.dt$Balance/churn_model2.dt$NumOfProducts

# Creating variable SalaryCreditScoreRatio
churn_model2.dt$SalaryCreditScoreRatio = churn_model2.dt$EstimatedSalary/churn_model2.dt$CreditScore



##--------------------------------------------------------------------------------------------------------
## CART Model (Unbalanced dataset)
##--------------------------------------------------------------------------------------------------------
# Splitting into train and test set-----------------------------------------------------------------------
set.seed(2004)
train = sample.split(Y = churn_model2.dt$Exited, SplitRatio = 0.7)
trainset <- subset(churn_model2.dt, train == T)
testset <- subset(churn_model2.dt, train == F)

# Analytics check to ensure trainset has 0.7 of the data
trainset[Exited==1,.N]/churn_model2.dt[Exited==1,.N]

# Growing the tree----------------------------------------------------------------------------------------
set.seed(2004)
max_tree = rpart(Exited~.,data = trainset,
                 method = 'class',control = rpart.control(minsplit = 2,cp = 0))

# Check the pruning sequence and 10-fold CV errors--------------------------------------------------------
plotcp(max_tree)
printcp(max_tree)
# sqrt(0.00911641*0.00420757)
cp_1 = 0.0124

# Checking if the cp_1 is indeed 0.0124-------------------------------------------------------------------
cv_errorcap = max_tree$cptable[which.min(max_tree$cptable[,"xerror"]),"xerror"]+
  max_tree$cptable[which.min(max_tree$cptable[,"xerror"]),"xstd"]
i = 1
j = 4
while(max_tree$cptable[i,j]>cv_errorcap)
{
  i = i+1
}
optimal_cp = ifelse(i>1,sqrt(max_tree$cptable[i,1] * max_tree$cptable[i-1,1]),1)
optimal_cp # Optimal_cp is 0.0124154

# Pruning the tree to optimal size (based on 1SE rule)----------------------------------------------------
prune_train <- prune(max_tree, cp = optimal_cp)
printcp(prune_train)
plotcp(prune_train)

# Plotting the tree and checking the variable importance--------------------------------------------------
rpart.plot(prune_train, nn= T, main = "Pruned Tree with cp = 0.0124154")

prune_train$variable.importance

# Predicting the test set with the trained model----------------------------------------------------------
predict_test <- predict(prune_train, newdata = testset, type = "class")

table(testset$Exited, predict_test, deparse.level = 2)

# Evaluating the Confusion Matrix--------------------------------------------------------------------------
overall_accuracy = mean(predict_test==testset$Exited)
overall_accuracy
sensitivity = 253/(253+358)
sensitivity
specificity = 2297/(2297+92)
specificity

##--------------------------------------------------------------------------------------------------------
## CART Model (Balanced dataset)
##--------------------------------------------------------------------------------------------------------
# Creating the balanced train set-------------------------------------------------------------------------
# Checking the number of samples
trainset[Exited==1,.N]

# Splitting up the yes and no cases
data_train_no = trainset[Exited==0]
data_train_yes = trainset[Exited==1]

data_train_no = sample_n(data_train_no, 1426)
data_train_balanced =  rbind(data_train_no, data_train_yes)

# Growing the tree----------------------------------------------------------------------------------------
set.seed(2004)
max_tree_balanced = rpart(Exited~.,data = data_train_balanced,method = 'class',
                          control = rpart.control(minsplit = 2,cp = 0))

# Check the pruning sequence and 10-fold CV errors--------------------------------------------------------
plotcp(max_tree_balanced)
printcp(max_tree_balanced)
cp_2 = 0.004958673

# Checking if the cp_2 is indeed 0.004958673--------------------------------------------------------------
cv_errorcap = max_tree_balanced$cptable[which.min(max_tree_balanced$cptable[,"xerror"]),"xerror"]+
  max_tree_balanced$cptable[which.min(max_tree_balanced$cptable[,"xerror"]),"xstd"]
i = 1
j = 4
while(max_tree_balanced$cptable[i,j]>cv_errorcap)
{
  i = i+1
}
optimal_cp = ifelse(i>1,sqrt(max_tree_balanced$cptable[i,1] * max_tree_balanced$cptable[i-1,1]),1)
optimal_cp # Optimal_cp is 0.004958673

# Pruning the tree to optimal size (based on 1SE rule)----------------------------------------------------
prune_train_balanced <- prune(max_tree_balanced, cp = optimal_cp)

printcp(prune_train_balanced)
plotcp(prune_train_balanced)

# Plotting the tree and checking the variable importance--------------------------------------------------
rpart.plot(prune_train_balanced, nn= T, main = "Balanced Pruned Tree with cp = 0.005")

prune_train_balanced$variable.importance

# Predicting the test set with the trained model----------------------------------------------------------
predict_test_balanced <- predict(prune_train_balanced, newdata = testset, type = "class")

table(testset$Exited, predict_test_balanced, deparse.level = 2)

# Evaulating the confusion Matrix-------------------------------------------------------------------------
overall_accuracy = mean(predict_test_balanced==testset$Exited)
overall_accuracy
sensitivity = 412/(412+199)
sensitivity
specificity = 2008/(2008+381)
specificity

##--------------------------------------------------------------------------------------------------------
## Logistic Regression Model (Unbalanced dataset)
##--------------------------------------------------------------------------------------------------------
# Finding the Optimal Logistic Model----------------------------------------------------------------------
# Creating the Logistic Model
normal_logistic <- glm(Exited~.,family =binomial, data = trainset)

# Using the step function to reduce the variables to decrease overfitting & underfitting 
normal_logistic = step(normal_logistic)
summary(normal_logistic)

# Prediction on test set using standard threshold---------------------------------------------------------
# Finding the probability of each case
prob_norm <- predict(normal_logistic, newdata = testset, type = 'response')
testset$prob = prob_norm

# Set the threshold for predicting Y = 1 based on probability.
threshold <- 0.5

# If probability > threshold, then predict Y = 1, else predict Y = 0.
y.hat_norm <- ifelse(prob_norm > threshold, "Yes", "No")

# Create a confusion matrix with actuals on rows and predictions on columns.
table(testset$Exited, y.hat_norm, deparse.level = 2)

# Checking the odds ratio of siginificant independent variables-------------------------------------------
OR <- exp(coef(normal_logistic))
OR

OR.CI <- exp(confint(normal_logistic))
OR.CI


# Finding the optimal threshold---------------------------------------------------------------------------
k1 <- ggplot(testset, aes(Exited, prob, fill=Exited)) +
  geom_boxplot() +
  labs(title = "Probability distribution of customer exiting for 0.5 threshold", 
       y="Probability distribution") +
  scale_fill_manual(values=c("#02AEAE", "#FF7068")) +
  scale_x_discrete(name = "Exited", labels = c("No","Yes")) +
  theme(legend.position = "none")
k1

quantile(testset$prob[testset$Exited==0])
quantile(testset$prob[testset$Exited==1])

# Use a Cart Model to find the optimal split( which will be the threshold) to categorise the cases
threshold_tree_normal = rpart(Exited~prob,data = testset,method = 'class',control = rpart.control(minsplit = 2,cp = 0))
plotcp(threshold_tree_normal)
printcp(threshold_tree_normal)
# optimal cp is 0.012

threshold_tree_normal = prune(threshold_tree_normal,cp = 0.012)
rpart.plot(threshold_tree_normal)

# since the first split is when probability is lesser than 0.37 it will be a no case

# Prediction on test set using 0.37 threshold---------------------------------------------------------
# Set the threshold for predicting Y = 1 based on probability.
threshold <- 0.37

# If probability > threshold, then predict Y = 1, else predict Y = 0.
y.hat_norm <- ifelse(prob_norm > threshold, "Yes", "No")

# Create a confusion matrix with actuals on rows and predictions on columns.
table(testset$Exited, y.hat_norm, deparse.level = 2)

# Evaluating the confusion matrix---------------------------------------------------------------------
overall_accuracy = mean(y.hat_norm==testset$Exited )
overall_accuracy
sensitivity = 246/(246+365)
sensitivity
specificity = 2172/(2172+217)
specificity
##--------------------------------------------------------------------------------------------------------
## Logistic Regression Model (Balanced dataset)
##--------------------------------------------------------------------------------------------------------
# Finding the Optimal Logistic Model----------------------------------------------------------------------
# Creating the Logistic Model
balance_logistic <- glm(Exited~.,family =binomial, data = data_train_balanced)

# Using the step function to reduce the variables to decrease overfitting & underfitting 
balance_logistic = step(balance_logistic)
summary(balance_logistic)

# Checking the odds ratio of siginificant independent variables-------------------------------------------
OR <- exp(coef(balance_logistic))
OR

OR.CI <- exp(confint(balance_logistic))
OR.CI

# Prediction on test set using standard threshold---------------------------------------------------------
# Finding the probability of each case
prob_balance <- predict(balance_logistic,newdata = testset, type = 'response')
testset$prob_balance = prob_balance

# Set the threshold for predicting Y = 1 based on probability.
threshold <- 0.5

# If probability > threshold, then predict Y = 1, else predict Y = 0.
y.hat_balance <- ifelse(prob_balance > threshold, "Yes", "No")

# Create a confusion matrix with actuals on rows and predictions on columns.
table(testset$Exited, y.hat_balance, deparse.level = 2)

# Finding the optimal threshold---------------------------------------------------------------------------
k2 <- ggplot(testset, aes(Exited, prob_balance, fill=Exited)) +
  geom_boxplot() +
  labs(title = "Probability distribution of customer exiting for 0.5 threshold", 
       y="Probability distribution") +
  scale_fill_manual(values=c("#02AEAE", "#FF7068")) +
  scale_x_discrete(name = "Exited", labels = c("No","Yes")) +
  theme(legend.position = "none")
k2

# Finding the optimal Threshold--------------------------------------------------------------------------
plot(testset$Exited,testset$prob_balance,xlab = "Exited",ylab= "Prob",
     main = "Blanced prob against cateogry")
quantile(testset$prob_balance[testset$Exited==0])
quantile(testset$prob_balance[testset$Exited==1])

threshold_tree_balanced = rpart(Exited~prob_balance,data = testset,method = 'class',control = rpart.control(minsplit = 2,cp = 0))
plotcp(threshold_tree)
printcp(threshold_tree)
cp= sqrt(0.02454992*0.00709220)
threshold_tree = prune(threshold_tree,cp = cp)
rpart.plot(threshold_tree)

# the first split is when the probability is when balanced probability lesser than 0.55


# Prediction on test set using threshold == 0.55-----------------------------------------------------------
# Set the threshold for predicting Y = 1 based on probability.
threshold <- 0.55

# If probability > threshold, then predict Y = 1, else predict Y = 0.
y.hat_balance <- ifelse(prob_balance > threshold, "Yes", "No")

# Create a confusion matrix with actuals on rows and predictions on columns.
table(testset$Exited, y.hat_balance, deparse.level = 2)

# Evaluating the confusion matrix-------------------------------------------------------------------------
overall_accuracy = mean(y.hat_balance==testset$Exited )
overall_accuracy
sensitivity = 376/(376+235)
sensitivity
specificity = 1844/(1844+545)
specificity



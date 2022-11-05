# we are predicting the type, so focus more on the accuracy of the model
# if we want to focus more on the rabz cases, the focus more on the sensitivity of the model



setwd("D:/Users/venkat/Desktop/business/YEAR 2 SEM 1/BC2406/AY2020sem1 CBA")
library(data.table)
library(ggplot2)
library(tidyr)
library(arsenal)
data = fread("AHD.csv",stringsAsFactors = TRUE)
View(data)
summary(data)

data$Fbs = factor(data$Fbs)
data$RestECG = factor(data$RestECG)
data$ExAng = factor(data$ExAng)
data$Slope  = factor(data$Slope)
data$Sex = factor(data$Sex)
data$gender = ifelse(data$Sex==0,"F","M")
data$Sex = NULL
data$gender = factor(data$gender)

summary(data)
# data summary --------------------------------------------------------------------------
summary_table <- tableby(AHD ~ ., data )
summary(summary_table, title = "Summary",text = TRUE)

#data exploration
######################################################################################

# finding the proportion 
ggplot(data, aes(AHD)) +
  geom_bar(fill="#FF7068") +
  labs(title = "Proportion of AHD Cases: NO vs YES", y="Number of cases") +
  scale_x_discrete(labels=c("No","Yes"))

# find the disribution, of the cases across ages

ggplot(data, aes(x=Age, y=AHD)) + 
  geom_violin(trim=FALSE)
# observation:pEOPLE WHO are older then to have yes in AHD
# explore more 

# effect of chestpain against AHD
ggplot(data, aes(ChestPain)) +
  geom_bar(position = 'fill', aes(fill = AHD)) +
  labs(title = "ChestPain against AHD", y = "Proportion of cases") +
  scale_fill_manual(name = "AHD", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))

# those of the cases that are no, are mainly under nonanginal, and nontypical
# most of the yes cases come from asymptomatic chest pain
# explore in more detail

# now comparing RestBp
ggplot(data, aes(x=RestBP, y=AHD)) + 
  geom_violin(trim=FALSE)
# the restBP distribution is approx the same, but those that have AHD have a larger range
# explore in more detail 

# chol
ggplot(data, aes(x=Chol, y=AHD)) + 
  geom_violin(trim=FALSE)
plot(data$AHD,data$Chol)
# distribution is approximately the same, the median of the yes is slightly higher than no
# explore in more detail

#fbs
ggplot(data, aes(Fbs)) +
  geom_bar(position = 'fill', aes(fill = AHD)) +
  labs(title = "Fbs against AHD", y = "Proportion of cases") +
  scale_fill_manual(name = "AHD", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# the fbs remained unchange, it still give realtively the same result when it is a 0 and when it is a 1.


#find how restecg affects AHD
ggplot(data, aes(RestECG)) +
  geom_bar(position = 'fill', aes(fill = AHD)) +
  labs(title = "RestECG against AHD", y = "Proportion of cases") +
  scale_fill_manual(name = "AHD", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# the yes cases are higher when restecg is a 1 and in 2 ,
# more no cases when RESTECG is 0 need to go into detail

#MaxHR
ggplot(data, aes(x=MaxHR, y=AHD)) + 
  geom_violin(trim=FALSE)
plot(data$AHD,data$MaxHR)
# those that do not have AHD actually have a higher heartrate-> maybe since most of them are of younger age
#need to look inot more detail

#exang
ggplot(data, aes(ExAng)) +
  geom_bar(position = 'fill', aes(fill = AHD)) +
  labs(title = "Exang against AHD", y = "Proportion of cases") +
  scale_fill_manual(name = "AHD", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# when EXang is a yes, ahd higher proportion of yes, if it is a no AHd more likely to be no
# explore more

#oldpeak 
ggplot(data, aes(x=Oldpeak, y=AHD)) + 
  geom_violin(trim=FALSE)
plot(data$AHD,data$Oldpeak)
# larger interquantile range for yes, lower median for no
#look into more detail

#slope
ggplot(data, aes(Slope)) +
  geom_bar(position = 'fill', aes(fill = AHD)) +
  labs(title = "Slope against AHD", y = "Proportion of cases") +
  scale_fill_manual(name = "AHD", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# higher cases if yes in 2 and 3 , more cases of no in 1
#explore more

#ca
ggplot(data, aes(x=Ca, y=AHD)) + 
  geom_violin(trim=FALSE)
plot(data$AHD,data$Ca)

ggplot(data, aes(Ca)) +
  geom_bar(position = 'fill', aes(fill = AHD)) +
  labs(title = "Ca against AHD", y = "Proportion of cases") +
  scale_fill_manual(name = "AHD", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))

# when it is a no, most likely to have 0 CA, but if it is a yes it can take from 0 to 3
#look more in detail 




#Thal
ggplot(data, aes(Thal)) +
  geom_bar(position = 'fill', aes(fill = AHD)) +
  labs(title = "Thal against AHD", y = "Proportion of cases") +
  scale_fill_manual(name = "AHD", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# tend to be a no when the thal is normal, if it fixed and reversible tend to be a yes
# look more in detail

#effect of gender
ggplot(data, aes(gender)) +
  geom_bar(position = 'fill', aes(fill = AHD)) +
  labs(title = "Gender against AHD", y = "Proportion of cases") +
  scale_fill_manual(name = "AHD", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# more males have a yes than females
# look more in detail
#-----------------------------------------------------------------------------------------------------
## bivaritate analysis
# comparing age against gender
ggplot(data, aes(x=Age, y=gender)) + 
  geom_violin(trim=FALSE)
# even when the females have age greater then 60, females have very little yes cases
# gender might a factor in affecting the AHD
# need to look more into why females have a more no cases

#age against chestpain
ggplot(data, aes(x=Age, y=data$ChestPain)) + 
  geom_violin(trim=FALSE)
# for ages less than 50 we can see that most of them have nonanginal and nontypical pains
# typical and asmptomatic pains have a large group of people that are atage above 60
# and we know that the asymptomatic pain casues more yes cases, This could be related to the age instead
# what is different here is that eventhough typical pain has a group of above 60 , typical pain causes more no
# need to look more into typical chest pain 
plot(data$ChestPain,data$Age)


# comparing chestpain against gender
ggplot(data, aes(ChestPain)) +
  geom_bar(position = 'fill', aes(fill = gender)) +
  labs(title = "ChestPain against gender", y = "Proportion of cases") +
  scale_fill_manual(name = "gender", labels = c("F","M"),
                    values=c("#02AEAE", "#FF7068"))
# females tend to have more nonanginlical pain and nontypical pain
# also most females are in the age range of 60, but they dont have alot of Yes,
# the pain they experience might be a cause/result of the AHD 

# by comparing restbp against chestpain
ggplot(data, aes(x=RestBP, y=data$ChestPain)) + 
  geom_violin(trim=FALSE)
# most of the restbp is the same across the different pains, except typical. 
#This doesnt give us much inofrmation

#by comparing Age against REstBp and seeing the distribution of AHD cases
ggplot(data, 
       aes(x=Age, y=RestBP, color = AHD)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = c("#02AEAE", "#FF7068"), labels = c("No","Yes")) +
  labs(title = "Age against RestBp  on AHD")
plot(data$Age,data$RestBP)
cor(data$Age,data$RestBP)
# restbp doesnt seem to have a correlation between Age 
# and there cant be significant relationship observed between the restBP and HAD


#by comparing the restBP agianst the gender and age

ggplot(data, 
       aes(x=Age, y=RestBP, color = AHD, shape = gender)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = c("#02AEAE", "#FF7068"), labels = c("No","Yes")) +
  scale_shape_manual(name = "gender", values = c(17,15), labels = c("F","M")) +
  labs(title = "restbp against Age and gender on AHD")

ggplot(data, aes(Age, RestBP)) +
  geom_smooth(method=lm,aes(fill = AHD)) +
  labs(title = "Age Against Restbp Salary",
       y = "Restbp", x = "Age ") +
  scale_fill_manual(values = c("#02AEAE", "#FF7068"),
                    name = "AHD", labels = c("No","Yes"))
# this doesnt give us any additional information, therefore the restbp might not affect AHD and indepedant of age and gender

#age against chol
# from common sense, there might be a correlation
ggplot(data, 
       aes(x=Chol, y=Age, color = AHD, shape = gender)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = c("#02AEAE", "#FF7068"), labels = c("No","Yes")) +
  scale_shape_manual(name = "gender", values = c(17,15), labels = c("F","M")) +
  labs(title = "Age against Chol and gender on AHD")
cor(data$Age,data$Chol)
# there doesnt seem to have significance relationship shown here
# it seems that the cholestrol is indpendant of gender and age
# and there doesnt seem to be a significant change in the AHD cases when the chol is increased,

ggplot(data, aes(Age, Chol)) +
  geom_smooth(method=lm,aes(fill = AHD)) +
  labs(title = "Age Against Chol",
       y = "Chol", x = "Age") +
  scale_fill_manual(values = c("#02AEAE", "#FF7068"),
                    name = "AHD", labels = c("No","Yes"))
# there doesnt seem to have a siginficant impact as well
# from basic understanding we also know that the cholestrol levels will decrease with a increase in activity level
# 3 variables that are related to excersie Exang, slope and oldpeak

ggplot(data, 
       aes(x=Chol, y=Age, color = ExAng)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = c("#02AEAE", "#FF7068"), labels = c("No","Yes")) +
  labs(title = "Age against Chol on EXang")

ggplot(data, aes(x=Chol, y=data$ExAng)) + 
  geom_violin(trim=FALSE)
# this has the same distibution as Exang
#either exang affect cholestrol or cholestrol affect exang


ggplot(data, 
       aes(x=Chol, y=Oldpeak, color = AHD)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = c("#02AEAE", "#FF7068"), labels = c("No","Yes")) +
  labs(title = "Oldpeak against Chol  on AHD")
# when the chol levels are between 200 and 250 and oldpeak is lesser than 2-> no
# same range but oldpeak greater than 2-> yes
# both might affect
ggplot(data, aes(x=Chol, y=data$Slope)) + 
  geom_violin(trim=FALSE)
# the outiers are cuaed by slope 1 and 2, but the overall distribution is realtively the same

ggplot(data, aes(Oldpeak, Chol)) +
  geom_smooth(method=lm,aes(fill = AHD)) +
  labs(title = "oldPeak Against Chol",
       y = "Chol", x = "Age") +
  scale_fill_manual(values = c("#02AEAE", "#FF7068"),
                    name = "AHD", labels = c("No","Yes"))
# difference in correlation, oldpeak per chol might affect more
data$Oldpeak_per_Chol = data$Oldpeak/data$Chol

ggplot(data, aes(x=Oldpeak_per_Chol, y=data$AHD)) + 
  geom_violin(trim=FALSE)
# same distribution as oldpeak 

plot(data$ExAng,data$Chol)


#comparing RestECG across gender and age
ggplot(data, aes(RestECG)) +
  geom_bar(position = 'fill', aes(fill = gender)) +
  labs(title = "RestECg against gender", y = "Proportion of cases") +
  scale_fill_manual(name = "gender", labels = c("F","M"),
                    values=c("#02AEAE", "#FF7068"))
# males tend to have have a restecg have 0 and 2
# females tend to restEcg have 1 more cases
#however when restecg is 1 , there is a higher chance of getting yes for AHD
# what might have caused the females to have a less chance of getting no for AHD
# even though the age is greater than  60 and they have more on restECG =1
# how is restecg differeing across age
ggplot(data, aes(x=Age, y=data$RestECG)) + 
  geom_violin(trim=FALSE)
# for restecg at 1 and 2, the majority of the points are in 50-60 range which gives a more yes cases
# maybe the restecg is affect by the age 
# even if the points lie in age 50-60, the restecg at 0 does not give a high Yes cases 
# need to look more in detail what does rest ecg affected by




# looking at max hr,
# logiaclly speaking max hr should be affected by the age,
# and from the univariate we have observed that no AHD cases, tend to have a higher heartrate
ggplot(data, 
       aes(x=Age, y=MaxHR, color = AHD, shape = gender)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = c("#02AEAE", "#FF7068"), labels = c("No","Yes")) +
  scale_shape_manual(name = "gender", values = c(17,15), labels = c("F","M")) +
  labs(title = "MaxHR  against Age and gender on AHD")

ggplot(data, aes(Age, MaxHR)) +
  geom_smooth(method=lm,aes(fill = AHD)) +
  labs(title = "Age Against MAXhr",
       y = "MaxHr", x = "Age") +
  scale_fill_manual(values = c("#02AEAE", "#FF7068"),
                    name = "AHD", labels = c("No","Yes"))

# from the smooth graph we can observe that the maxhr has a negative correlation
# from the scatter plot we can see that even though maxhr might affect the AHD,
# it doesnt look like gender affects MAXhr




# looking at exang, which is excersised induced
#since more young people excersise generally, would they have a higher exang
# from the univariate analsyis we observed when exang is 0-> no and when exang =1 -> yes 
ggplot(data, aes(x=Age, y=data$ExAng)) + 
  geom_violin(trim=FALSE)
plot(data$ExAng,data$Age)
# the exang tends to be 1 when the age is 50-60 inline with our intial observation that the age when 50-60
# will result in a yes case, in this scenario when age was 50-60 it will result in a 1
# and when exang is 1 has a higher case of yes
# but why when age is between 50-60 it still give exang 1? need to look into that




# looking at oldpeak
# oldpeak refers to st depression induced by excersise relative to rest
# since it is realted to excersise, is it affected by exang, and age?
plot(data$ExAng,data$Oldpeak)
plot(data$Oldpeak,data$Age)

ggplot(data, aes(x=Oldpeak, y=ExAng)) + 
  geom_violin(trim=FALSE)
# the distribution of oldpeak against Exang gives the same result as oldpeak against AHD
# meaning oldpeak is affects by Exang

ggplot(data, aes(x=Oldpeak, y=gender)) + 
  geom_violin(trim=FALSE)

# the distribution is the same across the gender is the same implying that the gender does not affect oldpeak that much

ggplot(data, 
       aes(x=Age, y=Oldpeak, color = AHD, shape = ExAng)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = c("#02AEAE", "#FF7068"), labels = c("No","Yes")) +
  scale_shape_manual(name = "EXang", values = c(17,15), labels = c("0","1")) +
  labs(title = "Age against Oldpeak and ExAng on AHD")

# age above 55 or oldpeak above 2 will result in a yes case


#  when oldpeak greater than 2-> Exang is a 1-> more cases on yes
# most of the points lie on between 50-60 and when oldpeak is less than 2-> more on no cases
# another key observation that we can find is that, the age range for a specific value of oldpeak to occur
#decreases, for oldpeak to be 0, doesnt matter the age. But for oldpeak 1 its more from 40 to 65
ggplot(data, aes(Oldpeak, Age)) +
  geom_smooth(method=lm,aes(fill = AHD)) +
  labs(title = "Oldpeak  Against Exang ",
       y = "Age", x = "Oldpeak") +
  scale_fill_manual(values = c("#02AEAE", "#FF7068"),
                    name = "AHD", labels = c("No","Yes"))
#no changes in the smooth plot between the AHD cases
#what affects oldpeak? 



#looking at slope,
#does the value of slop affect oldpeak
# looking at the univariate analysis,we saw that when slope is a 2 or 3 it will result in a higher yes cases

ggplot(data, 
       aes(x=Age, y=Oldpeak, color = AHD, shape = Slope)) +
  geom_point(alpha = 1) +
    scale_color_manual(values = c("#02AEAE", "#FF7068"), labels = c("No","Yes")) +
  scale_shape_manual(name = "Slope", values = c(17,15,20), labels = c("1","2","3")) +
  labs(title = "Age against Oldpeak and Slope on AHD")
# from the scatter graph-> when age greater than 60 and slope lesser than  -> slope is 2 -> combining both factors-> yes
# slope is affected by oldpeak and age

ggplot(data, aes(x=Age, y=data$Slope)) + 
  geom_violin(trim=FALSE)
# higher portion of people in 50 -60 in slope 2 and 3 might have cause the AHD

ggplot(data, aes(x=Oldpeak, y=data$Slope)) + 
  geom_violin(trim=FALSE)

ggplot(data, 
       aes(x=Age, y=Oldpeak, color = gender, shape = Slope)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = c("#02AEAE", "#FF7068"), labels = c("F","M")) +
  scale_shape_manual(name = "Slope", values = c(17,15,20), labels = c("1","2","3")) +
  labs(title = "Age against Oldpeak and Slope and gender")

# even though most females have slope = 2 why are they not affected by AHD




# looking at Ca
# from the univariate analysis we saw that most of the Ca are aound 0 for No
# and a range for yes but more being for 2 and 3
# and we know yes is mainly caused by Age and Oldpeak/Exang

ggplot(data, aes(Ca)) +
  geom_bar(position = 'fill', aes(fill = gender)) +
  labs(title = "CA against gender", y = "Proportion of cases") +
  scale_fill_manual(name = "gender ", labels = c("F","M"),
                    values=c("#02AEAE", "#FF7068"))
# most of the femlaes have 0 or 2 . And 0 will not affec the yes cases but ca = 2 should result in more yes case 
# then why do females stil have a lesser cases


ggplot(data, 
       aes(x=Ca, y=Age, color = AHD, shape = gender)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = c("#02AEAE", "#FF7068"), labels = c("No","Yes")) +
  scale_shape_manual(name = "Gender", values = c(17,15), labels = c("F","M")) +
  labs(title = "Age against CA and gender on AHD")
#most of the females have 0  in CA and from the Ca anaylsis we know, most of the patients with 0 has a relatively lower chance of getting it
# another interesting factor is when age is greater than 55, and when ca is 0, we can see that there is alot of no cases
# implying the ca factor outweigh the effect of age this might be the reason to why femlaes have lesser yes cases

# check if chol made any changes to it


ggplot(data, 
       aes(x=Ca, y=Chol, color = AHD, shape = gender)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = c("#02AEAE", "#FF7068"), labels = c("No","Yes")) +
  scale_shape_manual(name = "gender", values = c(17,15), labels = c("F","M")) +
  labs(title = "Chol against CA and gender on AHD")

# the cholestrol level doesnt seem to affect the result on ca




# now focusing on Thal
# from the univariate anaylsis we saw that normal had the highest number of no cases then reversabe had the highest yes
# what is the distribution of this across the genders
ggplot(data, aes(Thal)) +
  geom_bar(position = 'fill', aes(fill = gender)) +
  labs(title = "Thal against gender", y = "Proportion of cases") +
  scale_fill_manual(name = "gender", labels = c("F","M"),
                    values=c("#02AEAE", "#FF7068"))
# females had a significantly higher cases of normal this might have been the other reason to as why
# females didnt get alot if Yes cases 

# but what affects Thal, to make normal to result in a No

# is age a significant factor
ggplot(data, aes(x=Age, y=data$Thal)) + 
  geom_violin(trim=FALSE)
# there are few points from age 50-60 in normal thal, so it might not be that

#is the type of pain result in Thal
ggplot(data, aes(ChestPain)) +
  geom_bar(position = 'fill', aes(fill = Thal)) +
  labs(title = "Chestpain against Thal", y = "Proportion of cases") +
  scale_fill_manual(name = "Thal", labels = c("F","N","R"),
                    values=c("#02AEAE", "#FF7068","red"))
# for thal to be normal, the chest pain experienced is nonanginal and nontypical
# also fall in line with our earlier finidng that when the pain is asumptomatic result in higher yes cases
# and since female had the higher number of nonanginal and nontypical -> more no cases

#-------------------------------------------------------------------------------------------------------
# excuting cart model
library(rpart)
library(rpart.plot)
#split the model to train and test
library(caTools)
set.seed(1953)

train = sample.split(Y = data$AHD, SplitRatio = 0.7)
trainset <- subset(data, train == T)

testset <- subset(data, train == F)
testset = drop_na(testset)
set.seed(1953)
max_tree = rpart(AHD~.,data = trainset, method = 'class',control = rpart.control(minsplit = 2,cp = 0))
rpart.plot(max_tree)
max_tree$variable.importance
# finding the optimal prune trigger
plotcp(max_tree)
# the cp value is 0.03125
# check with the automated code

cv_errorcap = max_tree$cptable[which.min(max_tree$cptable[,"xerror"]),"xerror"]+
  max_tree$cptable[which.min(max_tree$cptable[,"xerror"]),"xstd"]
i = 1
j = 4
while(max_tree$cptable[i,j]>cv_errorcap)
{
  i = i+1
}
printcp(max_tree)
optimal_cp = ifelse(i>1,sqrt(max_tree$cptable[i,1] * max_tree$cptable[i-1,1]),1)
optimal_cp

# prune the tree
prune_tree = prune(max_tree,optimal_cp)
rpart.plot(prune_tree)

# testing the model with the testset
predict_test <- predict(prune_tree, newdata = testset, type = "class")

table(testset$AHD, predict_test, deparse.level = 2)

#evaluation of model
overall_accuracy = mean(predict_test==testset$AHD)
overall_accuracy
#72.7% overall accuracy

#----------------------------------------------------------------------------------------------------------
# removing the Na values
set.seed(1953)
trainset = drop_na(trainset)

# the other model is using Logistic regression
logistic <- glm(AHD~.,family =binomial, data = trainset)


# Using the step function to reduce the variables to decrease overfitting & underfitting 
logistic = step(logistic)
summary(logistic)



# Prediction on test set using standard threshold---------------------------------------------------------
# Finding the probability of each case


prob_norm <- predict(logistic, newdata = testset, type = 'response')
testset$prob = prob_norm
plot(testset$AHD,testset$prob,xlab = "AHD", ylab = "Predicted Prob")

# finding a optimal threshold
set.seed(1953)
threshold_tree = rpart(AHD~prob,data = testset, method = 'class',control = rpart.control(minsplit = 2,cp = 0))
plotcp(threshold_tree)
threshold_tree = prune(threshold_tree,0.13)
rpart.plot(threshold_tree)
# Set the threshold for predicting Y = 1 based on probability.
threshold <- 0.26
y.hat_norm <- ifelse(prob_norm > threshold, "Yes", "No")
table(testset$AHD, y.hat_norm, deparse.level = 2)
overall_accuracy = mean(y.hat_norm==testset$AHD)
overall_accuracy
#accuracy 86.4

# standard threshold
threshold <- 0.5
y.hat_norm <- ifelse(prob_norm > threshold, "Yes", "No")
table(testset$AHD, y.hat_norm, deparse.level = 2)
overall_accuracy = mean(y.hat_norm==testset$AHD)
overall_accuracy

plot(logistic)

# Checking the odds ratio of siginificant independent variables-------------------------------------------
OR <- exp(coef(logistic))
OR

OR.CI <- exp(confint(logistic))
OR.CI



#logistic model performs better, higher accuracy

# recommed to hospital, since it has a higher sensitivity
# key finding -> the type of pain experienced by the patient is very significant
# the number of Ca identified is also important
#then your resting Bp

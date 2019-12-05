# R-Project-Code
To predict the survival rate of the passengers on-board using Titanic Dataset.
train<-read.csv(file="E:/Data/Downloads/train.csv")
test<-read.csv(file="E:/Data/Downloads/test.csv")
test_1<-read.csv(file="E:/Data/Downloads/test_1.csv")
test = merge(test, test_1, by = "PassengerId")
names(train)
str(train)
##Pclass from int to categorcial variable
train$Pclass = as.factor(train$Pclass)
test$Pclass = as.factor(test$Pclass)
train$Survived<-as.factor(train$Survived)
test$Survived = as.factor(test$Survived)

str(train)
summary(train)
summary(train$Age)
sapply(train,function(x) sum(is.na(x)))
sapply(test,function(x) sum(is.na(x)))

##fill in missing values for Age
train$Age[is.na(train$Age)] = mean(train$Age, na.rm = TRUE)
test$Age[is.na(test$Age)] = mean(test$Age, na.rm = TRUE)
names(train)

# missing value 
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

## removing that row which has missing vaue 
test = test[!is.na(test$Fare),]


## Just taking a subset of column for model building
names(train)
train1 = subset(train, select = c(2,3,5,6,7,8,10))
names(test)
test1 = subset(test, select = c(2,4,5,6,7,9,11))

# treatment of outlier for Fare
boxplot(train1$Fare)
summary(train1$Fare)
upper<-31+1.5*IQR(train1$Fare);upper
train1$Fare[train1$Fare > upper]<-upper
boxplot(train1$Fare)
summary(train1$Fare)

# treatment of outlier for AGE and Business Logic
boxplot(train1$Age)
summary(train1$Age)
upper<-35+1.5*IQR(train1$Age);upper
train1$Age[train1$Age > upper]<-upper
lower<-22.00-1.5*IQR(train1$Age);lower
train1$Age[train1$Age < lower]<-lower
boxplot(train1$Age)
summary(train1$Age)

## model Building
model = glm(Survived ~., family = 'binomial', data = train1)
summary(model)

### variable signifcance selection
reg.model = step(glm(Survived ~., family = 'binomial', data = train1)
                 ,direction = "both")
summary(reg.model)
anova(reg.model,test='Chisq')

# WAY OF GIVING OWN REFERENCE
table(train1$Pclass)
table(train1$Sex)
# Model Building 
reg.model1 = step(glm(Survived ~relevel(Pclass,ref = 2)
                      +relevel(Sex,ref ='female')
                      +Age+Fare+SibSp+Parch, 
                      family = 'binomial', data = train1)
                  ,direction = "both")
summary(reg.model1)


reg.model1 = step(glm(Survived ~relevel(Pclass,ref = 2)
                      +relevel(Sex,ref ='female')
                      +Age+SibSp+Fare, 
                      family = 'binomial', data = train1)
                  ,direction = "both")
summary(reg.model1)
anova(reg.model1,test='Chisq')
Acc(reg.model1)

# to check multicollinearity 
library(car)
vif(reg.model1)

# To get Odds Ratio 
exp(coef(reg.model1))

## Prediction
test1$probs <-predict(reg.model1, test1, type='response')
test1$Predict<-as.factor(ifelse(test1$probs>0.70,1,0))
table(test1$Survived, test1$Predict)
library(caret)
confusionMatrix(test1$Survived,test1$Predict)

library(ROCR)
library(ggplot2)
# Make predictions on test set(predict use for op in terms of probability)
predictTrain = predict(reg.model1,test1 ,type="response")
# Prediction function
ROCRpred = prediction(predictTrain, test1$Survived)
# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
##ploting Roc Curve
plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)

#AUC
pred = prediction(test1$probs, test1$Survived)
as.numeric(performance(pred, "auc")@y.values)

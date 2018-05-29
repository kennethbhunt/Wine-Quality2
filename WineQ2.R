#Data set: winequality.csv
#Predict wines type (type) using classification trees. Use the boosting technique to
#choose the best predictors from the following: fixed acidity, volatile acidity, citric acid,
#residual sugar, chlorides, free sulfur dioxide, total sulfur dioxide, density, pH, sulphates
#and alcohol.

WineQ <-read.csv('winequality.csv', stringsAsFactors = F)

####Boosting Regression trees 

library(gbm)

WineQ2 <-WineQ[complete.cases(WineQ),]

n <-sample(6497,3200)
wine_train <- WineQ[n, 1:13]
wine_test <-WineQ[-n, 1:13]

WineQ$type1 <- ifelse(WineQ$type=="red", 0, 1)
WineQ$type <-NULL
rm(WineQ2)

Boost_fit <-gbm(type1~., data = wine_train, distribution = "bernoulli", 
                interaction.depth = 3, n.trees = 2000, shrinkage = 0.1)
summary(Boost_fit)

##Compute prediction accuracy in the Test set 

Boost_pred <- predict(Boost_fit, wine_test, n.trees = 2000, "response")
head(Boost_pred)
## Round the probabilities 
Boost_pred <- round(predict(Boost_fit, wine_test, n.trees = 2000, "response"))
head(Boost_pred)

mean(Boost_pred==wine_test$type1)

##Goodness of fit training set 
Boost_pred2 <- round(predict(Boost_fit, wine_train, n.trees = 2000, "response"))
head(Boost_pred)

mean(Boost_pred2==wine_train$type1)

##Find optimal shrinkage parameter 
k <- -10:-1
shrink <- 10 ^ k
vect_acc <- c()

for (j in shrink){
  Boost_fit <-gbm(type1~., data = wine_test, distribution = "bernoulli", 
                  interaction.depth = 3, n.trees = 2000, shrinkage = j)

Boost_pred <- round(predict(Boost_fit, wine_test, n.trees = 2000, "response"))

acc <- mean(Boost_pred==wine_test$type1)

vect_acc <- c(vect_acc, acc)
}

which.max(vect_acc)
max(vect_acc)
shrink[which.max(vect_acc)]

####RF Regression Tree

library(randomForest)
RF_fit <-randomForest(factor(type1)~., data = wine_train, mtry=4)
str(RF_fit)

##Compute prediction accuracy in the test set 
RF_pred <-predict(RF_fit, wine_test)
head(RF_pred)

mean(RF_pred==wine_test$type1)
#99%

##Compute prediction accuracy in the training set 
RF_pred2 <-predict(RF_fit, wine_train)
head(RF_pred2)

mean(RF_pred2==wine_train$type1)
#99%

## Grow the regression tree with rpart()
##rpart() has build it cross validation, 10 fold cv 

fit <- rpart(type1~., data=wine_train, method="class")

#Plot the tree
prp(fit)

rpart.plot(fit)

## Print the complexity paremeter table 
printcp(fit)

#

#0.071338 * 0.24531
#1-0.01749992
#98% accuracy in the training set 

RF_pred2 <-predict(RF_fit, wine_train, type="class")
head(RF_pred2)
mean(RF_pred2==wine_train$type1)

#Accuracy in the test set 
RF_pred3 <-predict(RF_fit, wine_test, type="class")
head(RF_pred3)
mean(RF_pred3==wine_test$type1)










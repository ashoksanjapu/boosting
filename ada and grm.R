#gradient Boosting
install.packages("xgboost")
install.packages("mlbench")
install.packages("gbm")
library(mlbench)
library(gbm)
boost<- read.csv(file.choose())
boost1 <- boost[1:768,2:10]
my_data <- boost
head(my_data)
#Check the best iteration number
gbm.model = gbm(class~., data=my_data, shrinkage=0.01, distribution = 'bernoulli', cv.folds=5, n.trees=3000, verbose=F)
best.iter = gbm.perf(gbm.model, method="cv")
summary(gbm.model)
plot.gbm(gbm.model, 1, best.iter)
#Plots the marginal effect of the selected variables by "integrating" out the other variables.
plot.gbm(gbm.model, 2, best.iter) 
plot.gbm(gbm.model, 3, best.iter) 
plot.gbm(gbm.model, 4, best.iter) 
#Using the caret package the get the model preformance in the best iteration
install.packages("caret")
install.packages("plyr")
library(caret)
set.seed(123)
fitControl = trainControl(method="cv", number=5, returnResamp = "all")
model2 = train(class~., data=my_data, method="gbm",distribution="bernoulli", trControl=fitControl, verbose=F, tuneGrid=data.frame(.n.trees=best.iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))
model2
confusionMatrix(model2)
#  Accuracy =0.773
#ada boosting
install.packages("adabag")
library(adabag)
adaboost <- boosting(class~., data = my_data, boos = TRUE, mfinal = 10, control =  rpart.control(minsplit = 0))
importanceplot(my_data.adaboost)
my_data.adaboost$class
my_data.boostcv <- boosting.cv(Species ~ ., v=2, data=my_data, mfinal=10, control=rpart.control(cp=0.01))
my_data.boostcv[-1]
confusionMatrix(adaboost)
#accuracy= 0.761
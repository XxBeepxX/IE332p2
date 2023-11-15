install.packages("caret")
install.packages("ellipse")
install.packages("rpart")
install.packages("plyr")
install.packages("mlbench")
install.packages("MLeval")
library(caret)
library(rpart)
library(ellipse)
library(plyr)
library(mlbench)

i <- iris

# change to 1s and 0s
i$Species = as.factor(ifelse(i$Species == "setosa", "1", "0"))

# partition the data
index <- createDataPartition(i$Species,p = 0.75, list= FALSE)
train <- i[index,]
test <- i[-index,]

control <-trainControl(method="cv", savePredictions = TRUE, number = 10)
metric <- "Accuracy"

# we will try a linear classifier, and a decision tree created using the CART algorithm
set.seed(1234)
lossm <- matrix(c(0,5,2,0),nrow=2,ncol=2)
model <- train(Species~., data=i, method="rpart", preProcess = "pca", parms=list(loss=lossm), metric=metric, trControl=control, cp=0.5)

print(model)

#confusion matrix
print(model$pred)
predictions <- predict(model, test)
confusionMatrix(predictions, test$Species)

#roc curve (Training)
train_pred_probs <- predict(model,train)
roc_curve <- roc(as.numeric(train$Species), as.numeric(train_pred_probs))
plot(roc_curve, main="ROC Curve - Training", col = "blue", lwd = 2)

#roc curve (Testing)
test_pred_probs <- predict(model,test)
roc_curve <- roc(as.numeric(test$Species), as.numeric(test_pred_probs))
plot(roc_curve, main="ROC Curve - Testing", col = "blue", lwd = 2)

library(MLeval)
pred <- predict(model, newdata=testing, type="prob")
test1 <- evalm(data.frame(pred, testing$Class))

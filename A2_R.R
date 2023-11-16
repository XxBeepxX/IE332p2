install.packages("caret")
install.packages("e1071")
install.packages("pROC")
install.packages("MLeval")
library(caret)
library(e1071)
library(pROC)
library(MLeval)

dataset <- iris

dataset$Species = as.factor(ifelse(dataset$Species == "setosa", "1", "0"))

index <- createDataPartition(dataset$Species, p = 0.75, list = FALSE)
train <- dataset[index,]
test <- dataset[-index,]

control <- trainControl(method="cv", number=10, savePredictions = T)
metric <- "Accuracy"     # from contingency tables: ratio of #(correctly predicted):(total #)

# we will try a linear classifier, and a decision tree created using the CART algorithm
set.seed(2895)
lossm <- matrix(c(0,5,2,0),nrow=2,ncol=2)
fit.svm <- train(Species~., data=dataset, method="svmLinearWeights", preProcess = "pca", parms=list(loss=lossm), class_weight = 'balanced', metric=metric, trControl=control)

# how accurate was each model?
print(fit.svm) # to show summary results for a specific model, or can compare across models...

predictions <- predict(fit.svm, test)
confusionMatrix(predictions, test$Species)

train_pred_probs <- predict(fit.svm, train)

roc_curve <- roc(as.numeric(train$Species), as.numeric(train_pred_probs))

plot(roc_curve, main = "ROC Curve - SVM on Training Data", col = "blue", lwd = 2)

test_pred_probs <- predict(fit.svm, test)

roc_curve <- roc(as.numeric(test$Species), as.numeric(test_pred_probs))

plot(roc_curve, main = "ROC Curve - SVM on Testing Data", col = "blue", lwd = 2)



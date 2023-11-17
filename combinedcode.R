
# Imports =====================================================================
install.packages("caret")
install.packages("ellipse")
install.packages("C50")
install.packages("plyr")
install.packages("rpart")
install.packages("ROCR")
install.packages("e1071")
library(rpart)
library(caret)
library(C50)
library(ellipse)
library(e1071)

# Load Data ===================================================================
i <- iris
# change to 1s and 0s
i$Species = as.factor(ifelse(i$Species == "setosa", "1", "0"))
# partition the data
index <- createDataPartition(i$Species,p = 0.75, list= FALSE)
train <- i[index,]
test <- i[-index,]
length(test)
set.seed(2895)

# Train Models ================================================================
#C50 model
x <- train[,1:4]
y <- train[,5]
control <- trainControl(method="cv", number = 10,savePredictions = TRUE)#, classProbs=T)
grid <- expand.grid(trials = c(1),model = c("tree", "rules"),winnow = c(TRUE, FALSE),cost = c(1:2))
c50model <- train(x=x,y=y,tuneGrid=grid,trControl=control,method='C5.0Cost',verbose=FALSE, preProcess="pca")

#Rpart model
metric <- "Accuracy"
lossm <- matrix(c(0,5,2,0),nrow=2,ncol=2)
rpartmodel <- train(Species~., data=train, method="rpart", preProcess = "pca", parms=list(loss=lossm), metric=metric, trControl=control, cp=0.5)

#svmLinearWeights model
lossm <- matrix(c(0,5,2,0),nrow=2,ncol=2)
svmLinearWeightsmodel <- train(Species~., data=train, method="svmLinearWeights", preProcess = "pca", parms=list(loss=lossm), class_weight = 'balanced', metric=metric, trControl=control)

#plotting 

library(pROC)

#C50 training predictions
c50trp <- predict(c50model, train)
c50trroc <- roc(as.numeric(train$Species), as.numeric(c50trp))
#C50 testing predictions
c50tep <- predict(c50model, test)
c50teroc <- roc(as.numeric(test$Species), as.numeric(c50tep))

#Rpart training predictions
rpartrp <- predict(rpartmodel,train)
rpartrroc <- roc(as.numeric(train$Species), as.numeric(rpartrp))
#Rpart testing predictions
rparttep <- predict(rpartmodel,test)
rpartteroc <- roc(as.numeric(test$Species), as.numeric(rparttep))

#svmLinearWeights training predictions
svmLinearWeightstrp <- predict(svmLinearWeightsmodel, train)
svmLinearWeightstrroc <- roc(as.numeric(train$Species), as.numeric(svmLinearWeightstrp))
#svmLinearWeights testing predictions
svmLinearWeightstep <- predict(svmLinearWeightsmodel, test)
svmLinearWeightsteroc <- roc(as.numeric(test$Species), as.numeric(svmLinearWeightstep))

#combined
# Plot ROC curves on the same graph for training
plot(c50trroc, col = "blue", lwd = 2, main = "ROC Curves for training", col.main = "black")
lines(rpartrroc, col = "red", lwd = 2)
lines(svmLinearWeightstrroc, col = "green", lwd = 2)
legend("bottomright", legend = c("c50", "rpart", "svmLinearWeights"), col = c("blue", "red", "green"), lty = 1, lwd = 2)

# Plot ROC curves on the same graph for testing
plot(c50teroc, col = "blue", lwd = 2, main = "ROC Curves for testing", col.main = "black")
lines(rpartteroc, col = "red", lwd = 2)
lines(svmLinearWeightsteroc, col = "green", lwd = 2)
legend("bottomright", legend = c("c50", "rpart", "svmLinearWeights"), col = c("blue", "red", "green"), lty = 1, lwd = 2)

#printing all the confusion matrixs

results <- resamples(list(c50=c50model, rpart=rpartmodel, svmLinearWeights=svmLinearWeightsmodel))
summary(results)

#C50
confusionMatrix(c50tep, test$Species)
confusionMatrix(c50trp, train$Species)

#rpart
confusionMatrix(rparttep, test$Species)
confusionMatrix(rpartrp, train$Species)

#svmLinearWeights
confusionMatrix(svmLinearWeightstep, test$Species)
confusionMatrix(svmLinearWeightstrp, train$Species)


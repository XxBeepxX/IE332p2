
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

save.seed=.Random.seed
# Train Models ================================================================
#C50 model
x <- train[,1:4]
y <- train[,5]
control <- trainControl(method="cv", number = 10,savePredictions = TRUE)#, classProbs=T)
grid <- expand.grid(trials = c(1),model = c("tree", "rules"),winnow = c(TRUE, FALSE),cost = c(1:2))
model<- train(x=x,y=y,tuneGrid=grid,trControl=control,method='C5.0Cost',verbose=FALSE, preProcess="pca")

#Rpart model
metric <- "Accuracy"
lossm <- matrix(c(0,5,2,0),nrow=2,ncol=2)
model <- train(Species~., data=i, method="rpart", preProcess = "pca", parms=list(loss=lossm), metric=metric, trControl=control, cp=0.5)

#e1071 model
lossm <- matrix(c(0,5,2,0),nrow=2,ncol=2)
fit.svm <- train(Species~., data=i, method="svmLinearWeights", preProcess = "pca", parms=list(loss=lossm), class_weight = 'balanced', metric=metric, trControl=control)




#plotting 

library(pROC)

#C50 training predictions
c50trp <- predict(model, train)
c50trroc <- roc(as.numeric(train$Species), as.numeric(c50trp))
#C50 testing predictions
c50tep <- predict(model, test)
c50teroc <- roc(as.numeric(test$Species), as.numeric(c50tep))

#Rpart training predictions
rpartrp <- predict(model,train)
rpartrroc <- roc(as.numeric(train$Species), as.numeric(rpartrp))
#Rpart testing predictions
rparttep <- predict(model,test)
rpartteroc <- roc(as.numeric(test$Species), as.numeric(rparttep))

#e1071 training predictions
e1071trp <- predict(fit.svm, train)
e1071trroc <- roc(as.numeric(train$Species), as.numeric(e1071trp))
#e1071 testing predictions
e1071tep <- predict(fit.svm, test)
e1071teroc <- roc(as.numeric(test$Species), as.numeric(e1071tep))


#combined
# Plot ROC curves on the same graph for training
plot(c50trroc, col = "blue", lwd = 2, main = "ROC Curves for training", col.main = "black")
lines(rpartrroc, col = "red", lwd = 2)
lines(e1071trroc, col = "green", lwd = 2)
legend("bottomright", legend = c("c50", "rpart", "e1071"), col = c("blue", "red", "green"), lty = 1, lwd = 2)

# Plot ROC curves on the same graph for testing
plot(c50teroc, col = "blue", lwd = 2, main = "ROC Curves for tesing", col.main = "black")
lines(rpartteroc, col = "red", lwd = 2)
lines(e1071teroc, col = "green", lwd = 2)
legend("bottomright", legend = c("c50", "rpart", "e1071"), col = c("blue", "red", "green"), lty = 1, lwd = 2)

#printing all the confusion matrixs
#C50
confusionMatrix(c50tep, test$Species)
confusionMatrix(c50trp, train$Species)
#rpart
confusionMatrix(rparttep, test$Species)
confusionMatrix(rpartrp, train$Species)
#e1071
confusionMatrix(e1071tep, test$Species)
confusionMatrix(e1071trp, train$Species)


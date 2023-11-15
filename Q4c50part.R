
# Imports =====================================================================
install.packages("caret")
install.packages("ellipse")
install.packages("C50")
install.packages("MLeval")
install.packages("plyr")
library(caret)
library(C50)
library(ellipse)


# Load Data ===================================================================
i <- iris

# change to 1s and 0s
i$Species = as.factor(ifelse(i$Species == "setosa", "1", "0"))

# partition the data
index <- createDataPartition(i$Species,p = 0.75, list= FALSE)
train <- i[index,]

test <- i[-index,]
length(test)

# Train Models ================================================================
set.seed(11)

x <- train[,1:4]
y <- train[,5]
control <- trainControl(method="cv", number = 10,savePredictions = TRUE)#, classProbs=T)
grid <- expand.grid(trials = c(1),model = c("tree", "rules"),winnow = c(TRUE, FALSE),cost = c(1:2))
model<- train(x=x,y=y,tuneGrid=grid,trControl=control,method='C5.0Cost',verbose=FALSE, preProcess="pca")
predictions <- model$pred

<<<<<<< HEAD
t_predictions <- predict(model, test)
confusionMatrix(t_predictions,test$Species)

test$Species
model$pred$pred
library(pROC)
plot.roc(as.numeric(t_predictions), as.numeric(test$Species))
length(t_predictions)
length(test$Species)


=======
fitControl <- trainControl(method = "repeatedcv",number = 10, repeats = 10, returnResamp="all")


control <-trainControl(method="cv", number = 10)
grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )


# Train the model using train() function from caret
model<- train(x=x,y=y,tuneGrid=grid,trControl=control,method="C5.0",verbose=FALSE)

# Use the test data to find model predictions
predictions <- predict(model, test)

# Confusion Matrix to show the accuracy of the predicted results vs actual results
table(predictions, test$Species)

# Print the best model
print(model)

model

# visualize the resample distributions
dotplot(model)

#xyplot(model,type = c("g", "p", "smooth"))
plot(model)
>>>>>>> 184548206a165bd3f2984b5a98d981ef91f6c3db

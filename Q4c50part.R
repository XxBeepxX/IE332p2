
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

t_predictions <- predict(model, test)
confusionMatrix(t_predictions,test$Species)

test$Species
model$pred$pred
library(pROC)
plot.roc(as.numeric(t_predictions), as.numeric(test$Species))
length(t_predictions)
length(test$Species)



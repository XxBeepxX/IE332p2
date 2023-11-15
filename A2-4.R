install.packages("caret")
install.packages("ellipse")
install.packages("rpart")
install.packages("plyr")
library(caret)
library(rpart)
library(ellipse)
library(plyr)

i <- iris

# change to 1s and 0s
i$Species = as.factor(ifelse(i$Species == "setosa", "1", "0"))

# partition the data
index <- createDataPartition(i$Species,p = 0.75, list= FALSE)
train <- i[index,]
test <- i[-index,]

control <-trainControl(method="cv", number = 10)
metric <- "Accuracy"

# we will try a linear classifier, and a decision tree created using the CART algorithm
set.seed(1234)
lossm <- matrix(c(0,5,2,0),nrow=2,ncol=2)
model <- train(Species~., data=i, method="rpart", preProcess = "pca", parms=list(loss=lossm), metric=metric, trControl=control, cp=0.5)

print(model)

install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)

dataset <- iris

dataset$Species = as.factor(ifelse(dataset$Species == "setosa", "1", "0"))

index <- createDataPartition(dataset$Species, p = 0.75, list = FALSE)
train <- dataset[index,]
test <- dataset[-index,]

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"     # from contingency tables: ratio of #(correctly predicted):(total #)

# we will try a linear classifier, and a decision tree created using the CART algorithm
set.seed(1234)
lossm <- matrix(c(0,5,2,0),nrow=2,ncol=2)
fit.svm <- train(Species~., data=dataset, method="svmLinearWeights", preProcess = "pca", parms=list(loss=lossm), class_weight = 'balanced', metric=metric, trControl=control)

# how accurate was each model?
print(fit.svm) # to show summary results for a specific model, or can compare across models...



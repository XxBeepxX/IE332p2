# Imports =====================================================================
install.packages("caret")
install.packages("ellipse")
install.packages("C50")
library(caret)
library(ellipse)

# Load Data ===================================================================
i <- iris

# change to 1s and 0s
i$Species = as.factor(ifelse(i$Species == "setosa", "1", "0"))

# partition the data
index <- createDataPartition(i$Species,p = 0.75, list= FALSE)
train <- i[index,]
test <- i[-index,]

# Train Models ================================================================
set.seed(11)

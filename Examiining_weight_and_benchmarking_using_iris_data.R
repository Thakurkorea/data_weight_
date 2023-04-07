# Load the caret package
rm(list=ls())
library(ggplot2)
library(caret)


# Load the iris dataset as an example
data(iris)
##################Species class given ############
# Fit a random forest model to the dataset
rf_model <- train(Species ~ ., data = iris, method = "rf")

# Compute feature importance weights
importance_weights <- varImp(rf_model)

# Define feature importance weights for each column
feature_weights <- t(importance_weights[["importance"]]) # transpose

##################Species class  not given given ############


# Calculate the variance of each variable
variances <- apply(iris[, 1:4], 2, var)

# Calculate the weights of each variable
importance_weights <- variances / sum(variances)
feature_weights=matrix(t(c(importance_weights)))
########################################
# Print the importance weights
print(importance_weights)

# Compute the weighted row strength for each row
row_strength <- rowSums(as.matrix(iris[,1:4]) %*% feature_weights)

# Print the weighted row strength
print(row_strength)
# scale(row_strength,center = FALSE, scale = diff(range(row_strength)))
# normalize(iris[,1:4])
str(row_strength)

scale_weight<-(row_strength-min(row_strength,na.rm=T))/
  ((max(row_strength,na.rm=T))-min(row_strength,na.rm=T))

plot(sort(scale_weight))
abline(h=0.4)
abline(h=0.8)
iris$score<-scale_weight









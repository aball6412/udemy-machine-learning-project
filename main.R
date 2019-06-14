library(randomForest)
library(rpart)
library(rpart.plot)
library(ISLR)
library(ggplot2)
library(dplyr)
library(caTools)

# EXPLORATORY DATA

# Scatterplot
ggplot(College, aes(x=Room.Board, y=Grad.Rate, color=Private)) +
  geom_point()

# Histograms
ggplot(College, aes(F.Undergrad, fill=Private)) +
  geom_histogram(bins = 50, color = 'black', position = position_stack(reverse=T))

ggplot(College, aes(x=Grad.Rate, fill=Private)) +
  geom_histogram(bins = 50, color = 'black', position = position_stack(reverse=T))

# College with graduation rate above 100 (Cazenovia College)
College %>%
  filter(Grad.Rate > 100)

# Change that value to 100
College$Grad.Rate[College$Grad.Rate > 100] <- 100


# MODEL BUILDING


# Train/test split
index <- sample.split(College$Private, SplitRatio = .70)
train <- College[index,]
test <- College[!index,]

# Decision Tree
d_tree <- rpart(Private ~ ., method='class', data=train)

# Training Set Accuracy
train_pred <- predict(d_tree, train, type='class')
train_cm <- table(train$Private, train_pred)
train_accuracy <- sum(diag(train_cm)/sum(train_cm))
print(train_accuracy)

# Test Set Accuracy
test_pred <- predict(d_tree, test, type='class')
test_cm <- table(test$Private, test_pred)
test_accuracy <- sum(diag(test_cm)/sum(test_cm))
print(test_accuracy)

# Print Confusion Matrix
print(test_cm)

# Plot Decision Tree
prp(d_tree)

# Random Forest Model
rf_model <- randomForest(Private ~ ., data = train, importance = T)

# Training Set Accuracy
train_cm = rf_model$confusion
train_accuracy <- sum(diag(train_cm)/sum(train_cm))
print(train_accuracy)

# Test Set Accuracy
test_pred <- predict(rf_model, test, type='class')
test_cm <- table(test$Private, test_pred)
test_accuracy <- sum(diag(test_cm)/sum(test_cm))
print(test_accuracy)

# Feature Importance
rf_model$importance
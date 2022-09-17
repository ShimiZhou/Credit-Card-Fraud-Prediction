install.packages("e1071")
data <- read.csv("/Users/zhoushimi/Desktop/creditcard.csv")
sum(is.na(data))
data$Class <- factor(data$Class)
table(data$Class)
set.seed(100)
train_id <- sample(1:nrow(data), nrow(data)*0.8)
train <- data[train_id,]
test <- data[-train_id,]

table(train$Class)
mean(as.integer(train$Class) - 1)

glm.model <- glm(Class ~ ., data = train, family = "binomial")
glm.prob <- predict(glm.model, newdata = test, type = "response")

glm.pred <- ifelse(glm.prob > 0.5, 1, 0)
table(test$Class, glm.pred)

mean(test$Class == glm.pred)

install.packages("randomForest")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caTools")
install.packages("readr")
install.packages("caret")
install.packages("rattle")
install.packages("RColorBrewer")

remove.packages("rpart.plot")
remove.packages("rpart")
remove.packages("rattle")
remove.packages("RColorBrewer")

install.packages("rattle")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("RColorBrewer")

library(e1071)
library(caret)
svm.model <- svm(Class ~ ., data = train[1:10000,], kernel = "radial", cost = 1, gamma = 0.1)
svm.predict <- predict(svm.model, test)
confusionMatrix(test$Class, svm.predict)

mean(test$Class == svm.predict)


install.packages("gbm")
library(gbm, quietly=TRUE)

system.time(
  model_gbm <- gbm(Class ~ .
                   , distribution = "bernoulli"
                   , data = rbind(train_data, test_data)
                   , n.trees = 500
                   , interaction.depth = 3
                   , n.minobsinnode = 100
                   , shrinkage = 0.01
                   , bag.fraction = 0.5
                   , train.fraction = nrow(train_data) / (nrow(train_data) + nrow(test_data))
  )
)
gbm.iter = gbm.perf(model_gbm, method = "test")

model.influence = relative.influence(model_gbm, n.trees = gbm.iter, sort. = TRUE)
plot(model_gbm)

gbm_test = predict(model_gbm, newdata = test_data, n.trees = gbm.iter)
gbm_auc = roc(test_data$Class, gbm_test, plot = TRUE, col = "red")
print(gbm_auc)


#Ucitavanje Dataseta
HrEmployeeAttrition <- readRDS("HR-Employee-Attrition.rds")
dataSet <- HrEmployeeAttrition

#ucitavanje funkcija
source("Functions.R")

#Ucitavanje biblioteka
library(caret)
library(ROSE)
library(themis)
library(pROC)
library(randomForest)

set.seed(1010)
indexes <- createDataPartition(dataSet$Attrition, p= 0.80, list = F)
train.data <- dataSet[indexes,]
test.data <- dataSet[-indexes,]

#MODEL 1

rf1 <- randomForest(Attrition~., data = train.data)

rf1.pred <- predict(object = rf1, newdata = test.data, type = "class")

rf1.cm <- table(true = test.data$Attrition, predicted = rf1.pred)
rf1.cm

rf1.eval <- getEvaluationMetrics(rf1.cm)
rf1.eval

#SUBSAMPLING

#control
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

#downSample
set.seed(1010)

down_inside <- train(x = train.data[,-24], 
                     y = train.data$Attrition,
                     method = "rf",
                     metric = "ROC",
                     trControl = ctrl)

#up
ctrl$sampling <- "up"

set.seed(1010)
up_inside <- train(x = train.data[,-24], 
                   y = train.data$Attrition,
                   method = "rf",
                   metric = "ROC",
                   trControl = ctrl)

#rose
ctrl$sampling <- "rose"

set.seed(1010)
rose_inside <- train(x = train.data[,-24], 
                     y = train.data$Attrition,
                     method = "rf",
                     metric = "ROC",
                     trControl = ctrl)

#original
ctrl$sampling <- NULL

set.seed(1010)
orig_fit <- train(x = train.data[,-24], 
                  y = train.data$Attrition, 
                  method = "rf",
                  metric = "ROC",
                  trControl = ctrl)


inside_models <- list(original = orig_fit,
                      down = down_inside,
                      up = up_inside,
                      ROSE = rose_inside)

inside_resampling <- resamples(inside_models)
summary(inside_resampling, metric = "ROC")

#orig_fit is best

#MODEL 2

rf2.pred <- predict(object = orig_fit$finalModel, newdata = test.data, type = "class")

rf2.cm <- table(true = test.data$Attrition, predicted = rf2.pred)
rf2.cm

rf2.eval <- getEvaluationMetrics(rf2.cm)
rf2.eval

#MODEL 3

grid <- expand.grid(.mtry = 2:4)

ctrl <- trainControl(method = "CV",
                     number = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

mtry_rf <- train(x = train.data[,-24],
             y = train.data$Attrition,
             method = "rf",
             metric = "ROC",
             tuneGrid = grid,
             trControl = ctrl)

mtry_rf$finalModel
best_mtry <- mtry_rf$bestTune$mtry

rf3 <- randomForest(Attrition~., data = train.data, mtry = best_mtry)

rf3.pred <- predict(object = rf3, newdata = test.data, type = "class")

rf3.cm <- table(true = test.data$Attrition, predicted = rf3.pred)
rf3.cm

rf3.eval <- getEvaluationMetrics(rf3.cm)
rf3.eval

data.frame(rbind(rf1.eval, rf2.eval, rf3.eval), row.names = c("Forest1", "Forest2", "Forest3"))


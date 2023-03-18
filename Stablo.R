#Ucitavanje Dataseta
HrEmployeeAttrition <- readRDS("HR-Employee-Attrition.rds")
dataSet <- HrEmployeeAttrition

#ucitavanje funkcija
source("Functions.R")

#Ucitavanje paketa u okviru biblioteke
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(ROSE)
library(pROC)

set.seed(1010)

#kreiranje train i test dataseta
indexes <- createDataPartition(dataSet$Attrition, p = 0.8, list = FALSE)
train.data <- dataSet[indexes, ]
test.data <- dataSet[-indexes, ]

#MODEL 1

#kreiranje stabla - nebalansiranog
tree1 <- rpart(Attrition~., data = train.data, method = "class")

rpart.plot(tree1, extra = 104)

tree1.pred <- predict(tree1, newdata = test.data, type = "class")

#matrica konfuzije
tree1.cm <- table(true = test.data$Attrition, predicted = tree1.pred)
tree1.cm

#evaluacione metrike
eval.tree1 <- getEvaluationMetrics(tree1.cm)
eval.tree1

#MODEL 2

#krosvalidacija nebalansiranog stabla
numFolds = trainControl(method = "cv", number = 10)

cpGrid = expand.grid(.cp=seq(from = 0.001, to = 0.05,by = 0.001))

set.seed(1010)

str(dataSet)

crossvalidation <- train(x = train.data[,-24],
                         y = train.data$Attrition,
                         method = "rpart",
                         control = rpart.control(minsplit = 10),
                         trControl = numFolds,
                         tuneGrid = cpGrid)

crossvalidation
plot(crossvalidation)

#best cp = 0.026
cpValue <- crossvalidation$bestTune$cp

#kreiranje stabla nakon krosvalidacije
tree2 <- rpart(Attrition~.,
               data = train.data,
               method = "class",
               control = rpart.control(cp = cpValue))

tree2.pred <- predict(tree2, newdata = test.data, type = "class")

tree2.cm <- table(true = test.data$Attrition, predicted = tree2.pred)
tree2.cm

eval.tree2 <- getEvaluationMetrics(tree2.cm)
eval.tree2

data.frame(rbind(tree1.cm, tree2.cm))

data.frame(rbind(eval.tree1, eval.tree2), row.names = c("prvi","drugi"))

#SUBSAMPLING
#Optimalan cp uz balansiranje kroz kros validaciju

#control
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

#downSample
set.seed(1010)
down_inside <- train(x = train.data[,-24], 
                     y = train.data$Attrition,
                     method = "rpart",
                     metric = "ROC",
                     trControl = ctrl,
                     tuneGrid = cpGrid)
#up
ctrl$sampling <- "up"

set.seed(1010)
up_inside <- train(x = train.data[,-24], 
                   y = train.data$Attrition,
                   method = "rpart",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid = cpGrid)

#rose
ctrl$sampling <- "rose"

set.seed(1010)
rose_inside <- train(x = train.data[,-24], 
                     y = train.data$Attrition,
                     method = "rpart",
                     metric = "ROC",
                     trControl = ctrl,
                     tuneGrid = cpGrid)

#original fit
ctrl$sampling <- NULL

set.seed(1010)
orig_fit <- train(x = train.data[,-24], 
                  y = train.data$Attrition, 
                  method = "rpart",
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid = cpGrid)

inside_models <- list(original = orig_fit,
                      down = down_inside,
                      up = up_inside,
                      ROSE = rose_inside)

inside_resampling <- resamples(inside_models)

summary(inside_resampling, metric = "ROC")

#MODEL 3
rpart.plot(up_inside$finalModel)
tree3.pred <- predict(object = up_inside$finalModel, newdata = test.data, type = "class")

#Kreiranje matrice konfuzije
tree3.cm <- table(true=test.data$Attrition, predicted=tree3.pred)
tree3.cm

eval.tree3 <- getEvaluationMetrics(tree3.cm)
eval.tree3

data.frame(rbind(eval.tree1,eval.tree2, eval.tree3),
           row.names = c("Model1","Model2","Model3"))

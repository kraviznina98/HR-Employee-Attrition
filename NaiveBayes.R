#Ucitavanje Dataseta
HrEmployeeAttrition <- readRDS("HR-Employee-Attrition.rds")
dataSet <- HrEmployeeAttrition

#ucitavanje funkcija
source("Functions.R")

#ucitavanje paketa
library(bnlearn)
library(caret)
library(e1071)
library(pROC)
library(ROSE)
library(rpart)
library(naivebayes)

#Posto nijedna numericka varijabla nema normalnu raspodelu
#(To smo zakljucili prilikom sredjivanja dataseta)
#potrebno je da te varijable diskretizujemo

#Koristicemo kvantilnu diskretizaciju
#podelicemo varijablu u intervale iste frekvencije

str(dataSet)
numericColumns <- unlist(lapply(dataSet, is.numeric))

numericData <- dataSet[,numericColumns]
str(numericData)

numericDF <- as.data.frame(apply(numericData,2,as.numeric))

#EnvironmentSatisfaction diskretizujemo u 3 intervala, sa 4 intervala javlja se interval duzine 0

#JobInvolvement diskretizujemo u 2 intervala, sa 3 intervala javlja se interval duzine 0

#JobLevel diskretizujemo u 2 intervala, sa 3 intervala javlja se interval duzine 0

#JobSatisfaction diskretizujemo u 3 intervala, sa 4 intervala javlja se interval duzine 0

#NumCompaniesWorked diskretizujemo u 4 intervala, sa 5 intervala javlja se interval duzine 0

#StockOptionLevel diskretizujemo u 2 intervala, sa 3 intervala javlja se interval duzine 0

#TrainingTimesLastYear diskretizujemo u 3 intervala, sa 4 intervala javlja se interval duzine 0

#WorkLifeBalance diskretizujemo u 2 intervala, sa 3 intervala javlja se interval duzine 0

#YearsSinceLastPromotion diskretizujemo u 2 intervala, sa 3 intervala javlja se interval duzine 0

discretized <- discretize(data = numericDF,
                          method = 'quantile',
                          breaks = c(5,5,5,4,2,2,4,
                                     5,4,2,5,3,2,5,5,2,5))
str(numericDF)

summary(discretized)
newData <- as.data.frame(cbind(dataSet[,c(2,4,6,10,12,15)], discretized, dataSet$Attrition))

newData$Attrition <- newData$`dataSet$Attrition`
newData$`dataSet$Attrition` <- NULL

str(newData)

set.seed(1010)
indexes <- createDataPartition(newData$Attrition, p= 0.80, list = F)
train.data <- newData[indexes,]
test.data <- newData[-indexes,]

#MODEL 1

nb1 <- naiveBayes(Attrition ~., data = train.data)
nb1

nb1.pred <- predict(nb1, newdata = test.data, type = "class")

nb1.cm <- table(true = test.data$Attrition, predicted = nb1.pred)
nb1.cm

eval.nb1 <- getEvaluationMetrics(nb1.cm)
eval.nb1

#MODEL 2

nb2.pred.prob <- predict(nb1, newdata = test.data, type = "raw")
nb2.pred.prob

nb2.roc <- roc(response = as.integer(test.data$Attrition),
               predictor = nb2.pred.prob[,1],
               levels = c(2,1))
plot.roc(nb2.roc)
nb2.roc$auc

plot.roc(nb2.roc, print.thres = TRUE, print.thres.best.method = "youden")

nb2.coords.youden <- coords(nb2.roc,
                            ret = c("accuracy","spec","sens","threshold"),
                            x = "best",
                            best.method = "youden")

youden.threshold <- nb2.coords.youden[1,'threshold']

nb2.pred <- ifelse(test = nb2.pred.prob[,1]>= youden.threshold,
                   yes = "Yes", no = "No")
nb2.pred <- factor(nb2.pred, levels = c("Yes","No"))

nb2.cm<- table(true = test.data$Attrition, predicted = nb2.pred)
nb2.cm

eval.nb2 <- getEvaluationMetrics(nb2.cm)

data.frame(rbind(eval.nb1, eval.nb2), row.names = c("priv","drugi"))

#SUBSAMPLING

#control
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

laplace <- expand.grid(laplace = 1, usekernel= FALSE, adjust = 1)

#downSample
set.seed(1010)

down_inside <- train(x = train.data[,-24], 
                     y = train.data$Attrition,
                     method = "naive_bayes",
                     metric = "ROC",
                     trControl = ctrl,
                     tuneGrid = laplace)

#up
ctrl$sampling <- "up"

set.seed(1010)
up_inside <- train(x = train.data[,-24], 
                   y = train.data$Attrition,
                   method = "naive_bayes",
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid = laplace)

#rose
ctrl$sampling <- "rose"

set.seed(1010)
rose_inside <- train(x = train.data[,-24], 
                     y = train.data$Attrition,
                     method = "naive_bayes",
                     metric = "ROC",
                     trControl = ctrl,
                     tuneGrid = laplace)

#original
ctrl$sampling <- NULL

set.seed(1010)
orig_fit <- train(x = train.data[,-24], 
                  y = train.data$Attrition, 
                  method = "naive_bayes",
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid = laplace)


inside_models <- list(original = orig_fit,
                      down = down_inside,
                      up = up_inside,
                      ROSE = rose_inside)

inside_resampling <- resamples(inside_models)
summary(inside_resampling, metric = "ROC")

#down_inside best

down_inside$finalModel

#MODEL 3

nb3.pred <- predict(rose_inside$finalModel, newdata = test.data[,-24], type = 'class')

nb3.cm <- table(true = test.data$Attrition ,predicted = nb3.pred)
nb3.cm

eval.nb3 <- getEvaluationMetrics(nb3.cm)
eval.nb3

data.frame(rbind(eval.nb1,eval.nb2, eval.nb3),
           row.names = c("Model1","Model2","Model3"))
#       Accuracy  Precision Recall    F1
#down
#Model3 0.6791809 0.9175824 0.6788618 0.7803738
#up
#Model3 0.6655290 0.9065934 0.6707317 0.7710280
#rose
#Model3 0.6723549 0.9166667 0.6707317 0.7746479


#wczytanie i prognoza danych

library("readxl")
path <- "C:/dane_gus_uzupelnione.xlsx"
my_data <- read_excel(path, 2)
path1 <- "C:/dane_guss.xlsx"
View(my_data)
my_data_s<-read_excel(path1, 1)
r2006<-read_excel(path1,1)
r2006<-subset(r2006,select=c(1:6))
r2007<-read_excel(path1,2)
mydata_siec<-subset(my_data_s,select=-c(7:98))

library(forecast)
library(caret)
library(e1071)
#przewidywanie na podst sredniej
test_data<-r2006
train_data<-r2007
#tworzenie i testowanie modelu
set.seed(1234)
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
rf_default <- train(Smierc~.,train_data,"rf",
                    metric = "RMSE",
                    trControl = trControl)
print(rf_default)
rf_default$results
#best mtry
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(Smierc~.,
                 data = train_data,
                 method = "rf",
                 metric = "RMSE",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry)
best_mtry<-rf_mtry$bestTune$mtry
max_mtry<-max(rf_mtry$results$RMSE)
#best maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 15)) {
  set.seed(1234)
  rf_maxnode <- train(Smierc~.,
                      data = train_data,
                      method = "rf",
                      metric = "RMSE",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)
#best ntrees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(Smierc~.,
                       data = train_data,
                       method = "rf",
                       metric = "RMSE",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 24,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)
#best parameters
fit_rf <- train(Smierc~.,
                train_data,
                method = "rf",
                metric = "RMSE",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 250,
                maxnodes = 5)
#prediction
prediction <-predict(fit_rf, test_data)
#confusion matrix
smierc_vec<-as.numeric(unlist(test_data$Smierc))
confusionMatrix(
  factor(prediction, levels = 1:16),
  factor(smierc_vec, levels=1:16)
)

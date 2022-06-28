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

#klasyfikacja
library(forecast)
library(caret)
library(e1071)
#przewidywanie na podst sredniej
test_data<-r2007
train_data<-r2006
#tworzenie i testowanie modelu
set.seed(1)
nnetFit <- train(x=test_data, y=train_data$Smierc,
                 method = "nnet",
                 preProcess = "range", 
                 tuneLength = 2,
                 trace = FALSE,
                 maxit = 100)
#efekt
table(train_data$Smierc, predict(nnetFit)) 
table(test_data$Smierc,  predict(nnetFit,test_data))  


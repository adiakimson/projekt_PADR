#testy

#---------wczytanie-------------

library(readxl)
path <- "C:/dane_gus_uzupelnione.xlsx"
path1 <- "C:/dane_guss.xlsx"
r2006<-read_excel(path1,1)
r2006<-subset(r2006,select=c(1:6))
r2007<-read_excel(path1,2)

#----------lm-------------------

head(r2006)

r2006.lm<-lm(Smierc~Jedendzien+Srednielozka+Leczeniogolem+Szpitalne+Pobyt, data=r2006)
summary(r2006.lm)

names(r2006.lm)
summary(r2006.lm$coefficients)

r2006$Smierc.lm<-r2006.lm$fitted.values

plot(x = r2006$Smierc,                          # True values on x-axis
     y = r2006.lm$fitted.values,               # fitted values on y-axis
     xlab = "Prawdziwe wartości",
     ylab = "Wartości wg. modelu",
     main = "Regresja")

r2006.new<-data.frame(r2007)

predict(object = r2006.lm,     # The regression model
        newdata = r2006.new) 

#projekt PADR
#Karolina Joachimczyk, WEiTI
#Katarzyna Latos, WEiTI

#--wczytywanie danych--
#install.packages("xlsx") - zainstalowany
library("xlsx")
path <- "d:/dane_gus.xlsx"
my_data <- read.xlsx(path, 2)
View(my_data)

#--przygtowanie danych
#usuwamy kolumne lozek 18 i kolumne kod
bez_kolumny_1<-subset(my_data, select=-c(Kod))
View(bez_kolumny_1)
bez_kolumny_lozek_18<-subset(bez_kolumny_1,select=-c(łóżka.dla.dzieci.i.młodzieży.w.wieku.do.18.lat))
View(bez_kolumny_lozek_18)
#usuwamy wiersz drugi
library(dplyr)
bez_wiersza_2<-bez_kolumny_lozek_18%>%slice(-c(2))
View(bez_wiersza_2)

#--obróbka danych--
#install.packages("mice") - zainstalowany; instaluje także dplyr, ggplot2
#kolumny 8-14 i 27-29 trzeba wypełnić danymi (pakiet mice)
#bez_wiersza_2[is.na(bez_wiersza_2)]<-'0'

#poprawic na lepsze rozwiazanie - najpierw wszedzie zera tam gdzie imputation 
bez_wiersza_2$NA..8[bez_wiersza_2$NA..8=="-"]<-'0'
bez_wiersza_2$NA..9[bez_wiersza_2$NA..9=="-"]<-'0'
bez_wiersza_2$NA..10[bez_wiersza_2$NA..10=="-"]<-'0'
bez_wiersza_2$NA..11[bez_wiersza_2$NA..11=="-"]<-'0'
bez_wiersza_2$NA..12[bez_wiersza_2$NA..12=="-"]<-'0'
bez_wiersza_2$NA..13[bez_wiersza_2$NA..13=="-"]<-'0'
bez_wiersza_2$NA..14[bez_wiersza_2$NA..14=="-"]<-'0'
bez_wiersza_2$NA..27[bez_wiersza_2$NA..27=="-"]<-'0'
bez_wiersza_2$NA..28[bez_wiersza_2$NA..28=="-"]<-'0'
bez_wiersza_2$NA..29[bez_wiersza_2$NA..29=="-"]<-'0'
bez_wiersza_2$NA..8[bez_wiersza_2$NA..8=="0"]<-NA
bez_wiersza_2$NA..9[bez_wiersza_2$NA..9=="0"]<-NA
bez_wiersza_2$NA..10[bez_wiersza_2$NA..10=="0"]<-NA
bez_wiersza_2$NA..11[bez_wiersza_2$NA..11=="0"]<-NA
bez_wiersza_2$NA..12[bez_wiersza_2$NA..12=="0"]<-NA
bez_wiersza_2$NA..13[bez_wiersza_2$NA..13=="0"]<-NA
bez_wiersza_2$NA..14[bez_wiersza_2$NA..14=="0"]<-NA
bez_wiersza_2$NA..27[bez_wiersza_2$NA..27=="0"]<-NA
bez_wiersza_2$NA..28[bez_wiersza_2$NA..28=="0"]<-NA
bez_wiersza_2$NA..29[bez_wiersza_2$NA..29=="0"]<-NA
View(bez_wiersza_2)
#POPRAWIC!
bez_wiersza_2<-bez_wiersza_2%>%mutate(Nazwa=as.factor(Nazwa))
bez_wiersza_2<- mice(bez_wiersza_2, m=5, method = 'pmm')
View(bez_wiersza_2)

#library(mice)
#imputed_Data.mice <- mice(bez_wiersza_2, m=5, method = 'pmm')
#summary(imputed_Data)


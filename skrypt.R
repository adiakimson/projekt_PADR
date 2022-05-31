#projekt PADR
#Karolina Joachimczyk, WEiTI
#Katarzyna Latos, WEiTI

#tymczasowo dane uzupełnione w excelu

#--wczytywanie danych--
#install.packages("xlsx") - zainstalowany
library("xlsx")
path <- "D:\\dane_gus_uzupelnione.xlsx"
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
View(bez_wiersza_2$data)

#--wizualizacja danych--
#przyklad zależności zgonów od 1 cechy w 1 roku
library(ggplot2)
#problem z subset
przyklad_wiz<-subset(bez_wiersza_2,select=c(20,181))
wiz1<-ggplot(data = przyklad_wiz, aes(x=leczeni_w_trybie_1_dnia, y=liczba_zgonow))+
  geom_point(mapping = aes(data=przyklad_wiz, "zaleznosc zgonow od leczonych w trybie 1 dnia"), color = "red")+
  ggtitle("Wykres zaleznosci zgonow od leczonych w trybie 1 dnia w 2007 roku")


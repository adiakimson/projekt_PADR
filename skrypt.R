#projekt PADR
#Karolina Joachimczyk, WEiTI
#Katarzyna Latos, WEiTI
#14.06 prezentacja w RStudio tego co mamy

#tymczasowo dane uzupełnione w excelu

#------------WCZYTYWANIE DANYCH----------

getwd()
library("readxl")
path <- "D:/dane_gus_uzupelnione.xlsx"
my_data <- read_excel(path, 2)
path1 <- "D:/dane_guss.xlsx"
View(my_data)
my_data_s<-read_excel(path1, 1)
mydata_siec<-subset(my_data_s,select=-c(7:98))

#---------------PRZYGOTOWANIE DANYCH----------------------------

#usuwamy kolumne lozek 18 i kolumne kod
bez_kolumny_1<-subset(my_data, select=-c(Kod))
View(bez_kolumny_1)
bez_kolumny_lozek_18<-subset(bez_kolumny_1,select=-c(50))
View(bez_kolumny_lozek_18)

#usunięcie MIEJSCA DZIENNE
bez_nazwy<-subset(bez_kolumny_lozek_18,select=-c(2))
View(bez_nazwy)
bezreszty1<-subset(bez_nazwy,select=-c(3:15))
View(bezreszty1)
bezcalego1<-subset(bezreszty1,select=-c(2:3))
View(bezcalego1)

#usunięcie ŁÓŻKA OGÓŁEM + ŁÓŻKA DLA MŁODZIEŻY + ŁÓŻKA DLA KLINIK
bez3calych<-subset(bezcalego1,select=-c(19:63))
View(bez3calych)
bez3calych1<-subset(bez3calych,select=-c(18:19))
View(bez3calych1)

#USUWAMY PACJENCI DO 18 + PACJENCI POWYŻEJ 65
bezpacjentow<-subset(bez3calych1,select=-c(51:80))
View(bezpacjentow)
bezpacjentow1<-subset(bezpacjentow,select=-c(50:51))
View(bezpacjentow1)

#usuwamy wiersz drugi - [osoba]
library(dplyr)
bezpacjentow1wiersza<-bezpacjentow1%>%slice(-c(2))
View(bezpacjentow1wiersza)



#-------------------------WIZUALIZACJA DANYCH----------------------------------------

#przyklad zależności zgonów od 1 cechy w 1 roku
library(ggplot2)

#ŚMIERĆ A ŚREDNIA LICZBA ŁÓŻEK NA PRZESTRZENI LAT W CAŁEJ POLSCE
smierccc<-subset(bezpacjentow1wiersza, select=-c(1:81))
#TO NAS INTERESUJE BO KORZYSTAMY CZĘSTO W PRZSZŁOŚCI
smierc3<-smierccc%>%slice(-c(3:18))

#lata<-smierc3%>%slice(c(1)) #NO WIEKI WIEKÓW AMEN
danesmierc<-smierc3%>%slice(c(2))
#data <- data.frame(lata,danesmierc)

plot(2006:2021,danesmierc,col="red",xlab = "Lata", ylab = "Liczba zgonów",pch=20,cex=2, main="Osoby ze stwierdzonym zgonem przed podjęciem leczenia \n lub w trakcie na przestrzeni lat")


#ŚMIERĆ A LICZBA DNI POBYTU

#leczeni 1 dnia
dzien1<-subset(bezpacjentow1wiersza, select=-c(1,18:97))
dzien1wiersz<-dzien1%>%slice(c(2))

#leczeni klinicznie
klinicznie<-subset(bezpacjentow1wiersza, select=-c(1:49,66:97))
kliniczniewiersz<-klinicznie%>%slice(c(2))

#ŚMIERĆ A 1 DZIEŃ
plot(2006:2021,danesmierc,col="red",xlab = "Lata", ylab = "Liczba zgonów",pch=20,cex=2, main="Liczba osób zmarłych przed podjęciem leczenia \n a liczba pacjentów wyleczonych 1-go dnia")
par(new=TRUE)
plot(2006:2021,dzien1wiersz,col="green",pch=20,cex=2,axes=FALSE,ann=FALSE)
axis(4)
mtext("Liczba wyleczonych 1-go dnia", side=4) #NAPRAW ODSTĘP!

#ŚMIERĆ A LECZENI KLINICZNIE
plot(2006:2021,danesmierc,col="red",xlab = "Lata", ylab = "Liczba zgonów",pch=20,cex=2, main="Liczba osób zmarłych przed podjęciem leczenia \n a liczba pacjentów leczonych klinicznie")
par(new=TRUE)
plot(2006:2021,kliniczniewiersz,col="green",pch=20,cex=2,axes=FALSE,ann=FALSE)
axis(4)
mtext("Liczba przyjętych klinicznie", side=4) #NAPRAW ODSTĘP!

#PORÓWNANIE LECZENIA A ŚMIERĆ
plot(2006:2021,danesmierc,col="red",xlab = "Lata", ylab = "Liczba zgonów",pch=20,cex=2, main="Liczba pacjentów leczonych klinicznie \n oraz liczba osób wyleczonych 1-go dnia \n a liczba zmarłych przed podjęciem leczenia")
par(new=TRUE)
plot(2006:2021,kliniczniewiersz,col="green",pch=20,cex=2,axes=FALSE,ann=FALSE)
axis(4)
par(new=TRUE)
plot(2006:2021,dzien1wiersz,col="blue",pch=20,cex=2,axes=FALSE,ann=FALSE)
mtext("Liczba pacjentów", side=4)

#LEGENDA NIE DZIAŁA
#legend(100, 950, legend=c("Zgony", "Klinicznie", "1-go dnia"),
#col=c("red", "blue","green"), lty=1:2, cex=0.8)


#----------------- SIEĆ --------------------------------

library(neuralnet)
#model1=neuralnet(Jedendzien~.~Pobyt,data=mydata_siec, 
                 #hidden=2, err.fct="sse",treshold=0.05,linear.output = TRUE)
#model1$result.matrix
#plot(model1)


#nowa strona https://www.r-bloggers.com/2021/04/deep-neural-network-in-r/
str(mydata_siec)
#mydata_siec %<>% mutate_if(is.factor, as.numeric)

library(neuralnet)
n <- neuralnet(medv~Jedendzien +Srednielozka+Leczeniogolem+Szpitalne+Pobyt,
               data = mydata_siec,
               hidden = c(6,2),
               linear.output = F,
               lifesign = 'full',
               rep=1)

plot(n,col.hidden = 'darkgreen',     
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

data3 <- as.matrix(mydata_siec)
#dimnames(mydata_siec) <- NULL

#TESTY SIECI

set.seed(124)
ind <- sample(2, nrow(data3), replace = T, prob = c(.7, .3))
training <- data3[ind==1,1:13]
test <- data3[ind==2, 1:13]
trainingtarget <- data3[ind==1, 14]
testtarget <- data3[ind==2, 14]

#str(trainingtarget)
#num [1:363] 24 34.7 28.7 22.9 16.5 18.9 18.9 21.7 20.4 18.2 ...
#str(testtarget)
#num [1:143] 21.6 33.4 36.2 27.1 15 19.9 18.2 13.6 14.5 13.9 ...

#------------------SHINY-----------------------------

library(rsconnect)
rsconnect::deployApp('path/to/your/app')
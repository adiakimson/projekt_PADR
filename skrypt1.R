#projekt PADR
#Karolina Joachimczyk, WEiTI
#Katarzyna Latos, WEiTI

#------------WCZYTYWANIE DANYCH----------

getwd()
library(readxl)
path <- "C:/dane_gus_uzupelnione.xlsx"
my_data <- read_excel(path, 2)
path1 <- "C:/dane_guss.xlsx"
my_data_s<-read_excel(path1, 1)
mydata_siec<-subset(my_data_s,select=-c(7:98))

#---------------PRZYGOTOWANIE DANYCH----------------------------

#usuwamy kolumne lozek 18 i kolumne kod
bez_kolumny_1<-subset(my_data, select=-c(Kod))
bez_kolumny_lozek_18<-subset(bez_kolumny_1,select=-c(50))

#usunięcie MIEJSCA DZIENNE
bez_nazwy<-subset(bez_kolumny_lozek_18,select=-c(2))
bezreszty1<-subset(bez_nazwy,select=-c(3:15))
bezcalego1<-subset(bezreszty1,select=-c(2:3))

#usunięcie ŁÓŻKA OGÓŁEM + ŁÓŻKA DLA MŁODZIEŻY + ŁÓŻKA DLA KLINIK
bez3calych<-subset(bezcalego1,select=-c(19:63))
bez3calych1<-subset(bez3calych,select=-c(18:19))

#USUWAMY PACJENCI DO 18 + PACJENCI POWYŻEJ 65
bezpacjentow<-subset(bez3calych1,select=-c(51:80))
bezpacjentow1<-subset(bezpacjentow,select=-c(50:51))

#usuwamy wiersz drugi - [osoba]
library(dplyr)
bezpacjentow1wiersza<-bezpacjentow1%>%slice(-c(2))

#-------------------------WIZUALIZACJA DANYCH----------------------------------------

#przyklad zależności zgonów od 1 cechy w 1 roku
library(ggplot2)

#ŚMIERĆ A ŚREDNIA LICZBA ŁÓŻEK NA PRZESTRZENI LAT W CAŁEJ POLSCE
smierccc<-subset(bezpacjentow1wiersza, select=-c(1:81))
smierc3<-smierccc%>%slice(-c(3:18))
danesmierc<-smierc3%>%slice(c(2))

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
mtext("Liczba wyleczonych 1-go dnia", side=4)

#ŚMIERĆ A LECZENI KLINICZNIE
plot(2006:2021,danesmierc,col="red",xlab = "Lata", ylab = "Liczba zgonów",pch=20,cex=2, main="Liczba osób zmarłych przed podjęciem leczenia \n a liczba pacjentów leczonych klinicznie")
par(new=TRUE)
plot(2006:2021,kliniczniewiersz,col="green",pch=20,cex=2,axes=FALSE,ann=FALSE)
axis(4)
mtext("Liczba przyjętych klinicznie", side=4)

#PORÓWNANIE LECZENIA A ŚMIERĆ
plot(2006:2021,danesmierc,col="red",xlab = "Lata", ylab = "Liczba zgonów",pch=20,cex=2, main="Liczba pacjentów leczonych klinicznie \n oraz liczba osób wyleczonych 1-go dnia \n a liczba zmarłych przed podjęciem leczenia")
par(new=TRUE)
plot(2006:2021,kliniczniewiersz,col="green",pch=20,cex=2,axes=FALSE,ann=FALSE)
axis(4)
par(new=TRUE)
plot(2006:2021,dzien1wiersz,col="blue",pch=20,cex=2,axes=FALSE,ann=FALSE)
mtext("Liczba pacjentów", side=4)

#----------------- REGRESJA ----------------------------

library(readxl)
r2006<-read_excel(path1,1)
r2006<-subset(r2006,select=c(1:6))
r2007<-read_excel(path1,2)
test_data<-r2006
train_data<-r2007

head(r2006)

r2006.lm<-lm(Smierc~Jedendzien+Srednielozka+Leczeniogolem+Szpitalne+Pobyt, data=r2006)
summary(r2006.lm)

names(r2006.lm)
summary(r2006.lm$coefficients)

r2006$Smierc.lm<-abs(r2006.lm$fitted.values)

plot(x = r2006$Smierc,                          
     y = abs(r2006.lm$fitted.values),               
     xlab = "True Values",
     ylab = "Model Fitted Values",
     main = "Regression fits of number of deaths")

r2006.new<-data.frame(r2007)

predreg<-predict(object = r2006.lm,     
        newdata = r2006.new) 

#----------------- SIEĆ --------------------------------

library(neuralnet)

str(mydata_siec)
data3 <- as.matrix(mydata_siec)
set.seed(124)
ind <- sample(2, nrow(data3), replace = T, prob = c(0.7, 0.3))
training <- data3[ind==1, ]
test <- data3[ind==2, ]
trainingtarget <- data3[ind==8]
testtarget <- data3[ind==16]

n <- neuralnet(Smierc~Jedendzien +Srednielozka+Leczeniogolem+Szpitalne+Pobyt,
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

#TESTY SIECI

n$result.matrix
output <- compute(n, rep = 1, training)
head(output$net.result)

#------------------SHINY-----------------------------

library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cerulean"),
                
                navbarPage("Projekt PADR",
                           
                           tabPanel("Podsumowanie danych",
                                    
                                    # Application title
                                    titlePanel("Przewidywanie liczby zgonów na podstawie wybranych parametrów"),
                                    
                                    # Sidebar with a slider input
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("var", "Rozwiń roletkę:", 
                                                    c("Regresja" = "reg",
                                                      "Sieć" = "net",
                                                      "Wykresy" = "plots"))
                                      ),
                                      
                                      # Show a plot
                                      mainPanel(
                                        plotOutput("regPlot"),
                                        plotOutput("siecPlot"),
                                        plotOutput("wykPlot")
                                      )
                                    )
                                    
                           ),
                           
                           tabPanel("Regresja",
                                    
                                    titlePanel("Rezultaty Wykorzystanego modelu rf z parametrem RMSE"),
                                    
                                    # Sidebar with a checkbox input
                                    sidebarLayout(
                                      sidebarPanel(
                                        checkboxInput("var1", "Predykcja", TRUE)
                                        
                                      ),
                                      
                                      # Show a plot
                                      mainPanel(
                                        plotOutput("pred1Plot"),
                                        
                                      )
                                    )
                                    
                           ),
                           
                           tabPanel("Sieć neuronowa",
                                    
                                    titlePanel("Rezultaty wykorzystanej sieci"),
                                    
                                    # Sidebar with a slider input
                                    sidebarLayout(
                                      sidebarPanel(
                                        checkboxInput("var2", "Predykcja", TRUE)
                                        
                                        
                                      ),
                                      
                                      # Show a plot
                                      mainPanel(
                                        plotOutput("pred2Plot"),
                                      )
                                    )
                                    
                           ),
                           
                           tabPanel("Wykresy",
                                    
                                    titlePanel("Pozostałe wykresy"),
                                    
                                    # Sidebar with a slider input
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("var3", "Rozwiń roletkę:", 
                                                    c("Wykres 1" = "wyk1",
                                                      "Wykres 2" = "wyk2",
                                                      "Wykres 3" = "wyk3"))
                                      ),
                                      
                                      # Show a plot
                                      mainPanel(
                                        plotOutput("wyk1Plot"),
                                        plotOutput("wyk2Plot"),
                                        plotOutput("wyk3Plot")
                                      )
                                    )
                                    
                           ),
                           
                           tabPanel("O projekcie",
                                    
                                    titlePanel("O projekcie"),
                                    
                                    mainPanel("Projekt zakłada przewidywanie liczby zgonów w zależności od wybranych cech. Wykorzystane dane pochodzą z GUS. Przygotowały: Katarzyna Latos i Karolina Joachimczyk.","Źródło danych: GłóWny Urząd Statystyczny - https://bdl.stat.gov.pl/bdl/dane/podgrup/tablica")
                                    
                           )
                )
                
)                       

server <- function(input, output) {
  
  output$regPlot <- renderPlot({
    
    if(input$var=="reg")
    {
      #regresja
      plot(x = r2006$Smierc,                        
           y = r2006.lm$fitted.values,               
           xlab = "Prawdziwe wartości",
           ylab = "Wartości wg. modelu",
           main = "Regresja")
    }
    
  })
  
  output$siecPlot <- renderPlot({
    
    if(input$var=="net")
    {
      #sieć
      plot(n,col.hidden = 'darkgreen',     
           col.hidden.synapse = 'darkgreen',
           show.weights = F,
           information = F,
           fill = 'lightblue')
    }
    
  })
  
  output$wykPlot <- renderPlot({
    
    if(input$var=="plots")
    {
      #wykresy
      plot(2006:2021,danesmierc,col="red",xlab = "Lata", ylab = "Liczba zgonów",pch=20,cex=2, main="Liczba pacjentów leczonych klinicznie \n oraz liczba osób wyleczonych 1-go dnia \n a liczba zmarłych przed podjęciem leczenia")
      par(new=TRUE)
      plot(2006:2021,kliniczniewiersz,col="green",pch=20,cex=2,axes=FALSE,ann=FALSE)
      axis(4)
      par(new=TRUE)
      plot(2006:2021,dzien1wiersz,col="blue",pch=20,cex=2,axes=FALSE,ann=FALSE)
      mtext("Liczba pacjentów", side=4)
    }
    
  })
  
  output$pred1Plot <- renderPlot({
    
    if(input$var1==TRUE)
    {
      #regresja
      plot(1:16,predreg,col="red",xlab = "Numer województwa", ylab = "Liczba zgonów",pch=20,cex=2, main="Predykowane zgony dla kolejnych numerów województw")
    }
    
  })
  
  output$pred2Plot <- renderPlot({
    
    if(input$var2==TRUE)
    {
      #sieć
      plot(1:16,n$response,col="blue",xlab = "Numer województwa", ylab = "Liczba zgonów",pch=20,las=2,cex.names=0.7,cex=2, main="Predykowane zgony dla kolejnych numerów województw")
    }
    
  })
  
  output$wyk1Plot <- renderPlot({
    
    if(input$var3=="wyk1")
    {
      #wyk1
      plot(2006:2021,danesmierc,col="red",xlab = "Lata", ylab = "Liczba zgonów",pch=20,cex=2, main="Osoby ze stwierdzonym zgonem przed podjęciem leczenia \n lub w trakcie na przestrzeni lat")
    }
    
  })
  
  output$wyk2Plot <- renderPlot({
    
    if(input$var3=="wyk2")
    {
      #wyk2
      plot(2006:2021,danesmierc,col="red",xlab = "Lata", ylab = "Liczba zgonów",pch=20,cex=2, main="Liczba osób zmarłych przed podjęciem leczenia \n a liczba pacjentów leczonych klinicznie")
      par(new=TRUE)
      plot(2006:2021,kliniczniewiersz,col="green",pch=20,cex=2,axes=FALSE,ann=FALSE)
      axis(4)
      mtext("Liczba przyjętych klinicznie", side=4)
    }
    
  })
  
  output$wyk3Plot <- renderPlot({
    
    if(input$var3=="wyk3")
    {
      #wyk3
      plot(2006:2021,danesmierc,col="red",xlab = "Lata", ylab = "Liczba zgonów",pch=20,cex=2, main="Liczba osób zmarłych przed podjęciem leczenia \n a liczba pacjentów wyleczonych 1-go dnia")
      par(new=TRUE)
      plot(2006:2021,dzien1wiersz,col="green",pch=20,cex=2,axes=FALSE,ann=FALSE)
      axis(4)
      mtext("Liczba wyleczonych 1-go dnia", side=4)
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
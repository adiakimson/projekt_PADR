#projekt PADR
#Karolina Joachimczyk, WEiTI
#Katarzyna Latos, WEiTI

#--wczytywanie danych--
#install.packages("xlsx") - zainstalowany
library("xlsx")
my_data <- read.xlsx(dane_gus, 2)
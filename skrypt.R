#projekt PADR
#Karolina Joachimczyk, WEiTI
#Katarzyna Latos, WEiTI

#--wczytywanie danych--
#install.packages("xlsx") - zainstalowany
library("xlsx")
path <- "d:/dane_gus.xlsx"
my_data <- read.xlsx(path, 2)

#--obróbka danych--
#install.packages("mice") - zainstalowany; instaluje także dplyr, ggplot2
View(my_data)
#kolumny 8-14 i 27-29 trzeba wypełnić danymi (pakiet mice)





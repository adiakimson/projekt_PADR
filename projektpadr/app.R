#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                

    # Application title
    titlePanel("Przewidywanie liczby zgonów na podstawie wybranych parametrów"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("var", "Rozwiń roletkę:", 
                      c("Regresja" = "reg",
                        "Sieć" = "net",
                        "Wykresy" = "plots"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
      if(input$var=="Regresja")
        
        #regresja
        
      if(input$var=="Sieć")
        
        #sieć
      
      if(input$var=="Wykresy")

        # draw the plot
        plot(2006:2021,danesmierc,col="red",xlab = "Lata", ylab = "Liczba zgonów",pch=20,cex=2, main="Osoby ze stwierdzonym zgonem przed podjęciem leczenia \n lub w trakcie na przestrzeni lat")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

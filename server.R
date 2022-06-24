

# Load libraries, data ------------------------------------------------
library(ggplot2)
library("readxl")
path <- "D:/dane_gus_uzupelnione.xlsx"
my_data<-read_excel(path, 2)


# Create server -------------------------------------------------------
server <- function(input, output) {
  output$plot <- renderPlot({
    # sort by class
    characters$Character <- factor(characters$Character, 
                                   levels = characters$Character[order(characters$Class)])
    
    ggplot(data=characters, aes_string(x='Character', y=input$y_var, fill="Class")) +
      geom_bar(stat="identity", width=0.8) +
      labs(x="Character", y=input$y_var) + coord_flip()
    
  })
}

library(shiny)

# Load data ----
hack <- readRDS("/mnt/data1/Bex/Shiny/census-app/data/hackathon.rds")


ui <- fluidPage(
  selectInput("p","X-axis",choices = names(hack)),
  selectInput("q","Y-axis",choices = names(hack)),
  plotOutput("myplot"))

server <- function(input, output, session) {

  output$myplot <- 
  renderPlot({if(is.numeric(input$p) == "TRUE"){
    boxplot(get(input$q) ~ get(input$p) , data=hack)
  }else{
  plot(get(input$q) ~ get(input$p) , data=hack)
  }
  })
  }

shinyApp(ui, server)

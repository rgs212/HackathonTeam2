library(shiny)
library(dplyr)

# load data from knight (need manual input for now..)
MyDat <- read.csv("/mnt/data1/Szi/hackathon/Hackathon.csv")
Human <- MyDat[MyDat$Species == "Human",]

# user interface (simple for testing; Sex and Age)
ui <- fluidPage(
  title = "Examples of DataTables",
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "Sex", 
                         label = h4("Sex"), 
                         choices = levels(Human$Sex),
                         selected = "F"),
      sliderInput("Age", label = h4("Age"), min = 0, 
                  max = 100, value = c(20, 60)))
    ,
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Human", DT::dataTableOutput("mytable4"))
      )
    )
  )
)

# Server reactive to Sex and Age input 
server <- function(input, output) {
  
  df <- reactive({
    req(input$Sex)
    req(input$Age)
    filter(Human, Sex %in% input$Sex & between(Human_AgeYears, input$Age[1], input$Age[2]))
    })
  
  output$mytable4 <- DT::renderDataTable({
    DT::datatable(df(), options = list(scrollX = TRUE))
  })
  
}

shinyApp(ui, server)

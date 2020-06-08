### notes

library(shiny)
library(DT)
library(shinyalert)
library(shinyBS)
library(dplyr)

# Define UI ----

Human <- read.csv("~/Hackathon/Example.csv")[,-1]


ui <- fluidPage(
  titlePanel(p(column(2,img(src = "Exeter.png", height = 100)),
               column(8,h1(strong("Complex Disease Epigenetics Group Database", align = "center")),
                      p(h3(helpText("The interactive database to browse all available datasets of the group"))), align = "center"),
               column(2,p(img(src = "CDEG.png", height = 100)),
                      p(actionButton("UploadPopup", "Upload Data",
                                     style="color: #000000; background-color: #8bbbee; border-color: #000000", width = "100%"), 
                        align = "left")),
               bsModal("UploadModal","Upload new Data into Database","UploadPopup",
                       h3("Upload Human Data"),
                       h5(helpText("Please refer to the Upload Guidelines:"),a("humandataguideline.weblink.ac.uk")),
                       fileInput("UploadHuman", h5("Choose CSV File"), multiple = FALSE, 
                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                       h3("Upload Mouse Data"),
                       h5(helpText("Please refer to the Upload Guidelines:"),a("mousedataguideline.weblink.ac.uk")),
                       fileInput("UploadMouse", h5("Choose CSV File"), multiple = FALSE, 
                                 accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))))),
  tabsetPanel(
    tabPanel("Human Data",
             sidebarLayout(
               sidebarPanel(width = 3,
                            helpText(h2("Filter  database")),
                            selectizeInput("hDatatype", label =  "Data Type", sort(unique(Human$DataType)), selected = NULL, multiple = TRUE,
                                           options = NULL),
                            selectizeInput("hPlatform", label =  "Platform", sort(unique(Human$Platform)), selected = NULL, multiple = TRUE,
                                           options = NULL),
                            selectizeInput("hProject", label =  "Project", sort(unique(Human$Project)), selected = NULL, multiple = TRUE,
                                           options = NULL),
                            selectizeInput("hCohort", label =  "Cohort", sort(unique(Human$HumanCohort)), selected = NULL, multiple = TRUE,
                                           options = NULL),
                            checkboxGroupInput("hSex", label = "Sex", 
                                               choices = sort(unique(Human$Sex)),
                                               selected = sort(unique(Human$Sex))),
                            checkboxGroupInput("hTissue", label = "Tissue", 
                                               choices = sort(unique(Human$Tissue)),
                                               selected = sort(unique(Human$Tissue))),
                            sliderInput("hAge", label = ("Age"), min = min(na.omit(Human$Human_AgeYears)), 
                                        max = max(na.omit(Human$Human_AgeYears)), value = c(min(na.omit(Human$Human_AgeYears)), 
                                                                                            max(na.omit(Human$Human_AgeYears)))),
                            actionButton("hExtraFilter", "More",style="color: #000000; background-color: #D3D3D3; border-color: #000000"),
                            bsModal("ExtraModal","Apply more filter","hExtraFilter",
                                    selectizeInput("hCellType", label =  "Cell Type", sort(unique(Human$CellType)), selected = NULL, multiple = TRUE,
                                                   options = NULL),
                                    selectizeInput("hRegionFraction", label =  "Brain Region or Blood Fraction", sort(unique(Human$BrainRegion_BloodFraction)), selected = NULL, multiple = TRUE,
                                                   options = NULL),
                                    h4(helpText("and more!"))),
                            br(),
                            br(),
                            br(),
                            actionButton("hResetFilter", "Reset Filter",style = "color: #000000; background-color: #f88282; border-color: #000000")
               ),
               mainPanel(
                 column(10,
                        br(),
                        DT::dataTableOutput("data", width = 850) , 
                        br(),
                        p(actionButton(inputId = "gen_report", label = "Generate Report",style="color: #000000; background-color: #D3D3D3; border-color: #000000"), align = "right",
                          actionButton(inputId = "DownloadButton", label = strong("Download Data"),style="color: #000000; background-color: #a9a9a9; border-color: #000000")),
                        p(helpText("Generate QC Report or Download Selected Data"), align = "right"),
                        h2("Plots"),
                        helpText("Select Variables for Plotting"),
                        p(column(3,selectInput("hX","X-axis",choices = sort(names(Human)), selected = "Sex")),
                          column(3,selectInput("hY","Y-axis",choices = sort(names(Human)), selected = "Human_AgeYears"))),
                        
                        plotOutput("myplot", width = 850),
                        br(),br(), br(),br(),br(),br(),br(),
                        actionButton("REDBUTTON", strong("Press here, Jon"), style="color: #ffffff; background-color: #ff0000; border-color: #ffffff"),
                        bsModal("JonModal", "Oh no, that's not good!", "REDBUTTON", 
                                img(src = "muh.jpg", height = 425),
                                p(h3("The cow has eaten all the data. It is gone forever!"))),
                        br(),br()
                 ),
                 column(2,
                        br(),
                        actionButton("hColumnButton", "Column Selection",style="color: #000000; background-color: #D3D3D3; border-color: #000000"),
                        bsModal("ColumnModal","Columns to be display","hColumnButton",
                                checkboxGroupInput("hColumn", label = "Columns", choices = colnames(Human), 
                                                   selected = colnames(Human)),
                                actionButton("hResetColumn", "Reset",style="color: #000000; background-color: #f88282; border-color: #000000")),
                        p(helpText("Define shown columns")),
                        br(), br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                        )
                 
                 
               )
             )
    ),
    tabPanel("Mouse Data", h3("Ctrl+C - Ctrl+V "))
  )
)

# Define server logic ----
server <- function(input, output, session) {
  df <- reactive({
    req(input$hSex)
    req(input$hAge)

    Human <- filter(Human, Sex %in% input$hSex & 
                      between(Human_AgeYears, input$hAge[1], input$hAge[2])&
                      Tissue %in% input$hTissue
    )
    if (length(input$hDatatype > 0)){Human <- filter(Human,DataType %in% v$hDatatype)}
    else {Human}
    if (length(input$hPlatform > 0)){Human <- filter(Human,Platform %in% input$hPlatform)}
    else {Human}
    if (length(input$hProject > 0)){Human <- filter(Human,Project %in% input$hProject)}
    else {Human}
    if (length(input$hCohort > 0)){Human <- filter(Human,HumanCohort %in% input$hCohort)}
    else {Human}
    if (length(input$hCellType > 0)){Human <- filter(Human,CellType %in% input$hCellType)}
    else {Human}
    if (length(input$hRegionFraction > 0)){Human <- filter(Human,BrainRegion_BloodFraction %in% input$hRegionFraction)}
    else {Human}
  })
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$hDatatype, {
    v$hDatatype <- input$hDatatype
  })
  observeEvent(input$hResetFilter, {
    v$hDatatype <- sort(unique(Human$DataType))
  })  
  output$data <- DT::renderDataTable({
    DT::datatable(df()[, input$hColumn, drop = FALSE], options = list(scrollX = TRUE))
  })
  output$myplot <- 
    renderPlot({if(is.numeric(input$hX) == "TRUE"){
      boxplot(get(input$hY) ~ get(input$hX) , data=Human)
    }else{
      plot(get(input$hY) ~ get(input$hX) , data=Human)
    }
    })
  
  { "example second tab" }
}


# Run the app ----
shinyApp(ui = ui, server = server)

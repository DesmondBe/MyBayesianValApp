# Module for data input 

datasetInput <- function(id) {
  fileInput(NS(id, "file1"), label="Choose CSV file", accept=".csv")
}

datasetServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    Ufile <- reactive({
      if(is.null(input$file1)){return()}
      else {
        input$file1
      }
    })
    dt <- reactive({
      if(is.null(Ufile())){return()}
      else {
        read.csv(Ufile()$datapath)
      }
    })
    return(dt)
  })
}

#  ui <- fluidPage(
#    datasetInput("dataset"),
#    tableOutput("data")
#  )
#  server <- function(input, output, session) {
#    data <- datasetServer("dataset")
#    output$data <- renderTable(data())
#  }
#  shinyApp(ui=ui, server=server)

###################

################################################################################
##
## To work the app needs the following additional files
##                        1. Functions_0_1.R
##                        2. report.Rmd 
##                        3. module_data_input.R, module_plot.R, module_priors.R
##
##  An example of data file is: ValDta01.csv
##
################################################################################


####Libraries
library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(brms)
library(bayesplot)
library(tinytex)
library(shinycssloaders)

####Source files
source("Functions_0_1.R")
source("module_data_input.R")
source("module_plot.R")
source("module_priors.R")




# Define UI ----
ui <- fluidPage( theme = bslib::bs_theme(bootswatch = "cerulean"),
  titlePanel("My Bayesian Validation App"),
  
  sidebarLayout(
    sidebarPanel(
      datasetInput("dataset"),
      checkboxInput("header", "Header", TRUE),
      numericInput(
        inputId = "L",
        label = "Acceptance limit of Total Error (%):",
        value = 30,
        min = 1, max = 50,
        step = 1
      ),
      numericInput(
        inputId = "Prob",
        label = "Proba to be within acceptance limits:",
        value = 0.90,
        min = 0.1, max = 1,
        step = 0.01
      ),
      numericInput(
        inputId = "nRun",
        label = "Number of Runs for the Assay format:",
        value = 1,
        min = 1, max = 66,
        step = 1
      ),
      numericInput(
        inputId = "nRep",
        label = "Number of Replicates per run for the Assay format:",
        value = 1,
        min = 1, max = 66,
        step = 1
      ),
      downloadButton("report", "Create report",),
      
    ),
    
    mainPanel(
      navset_card_underline(
        nav_panel("Read Me",
                  h1('Read Me First'),
                  p(),
                  htmlOutput("ReadMeTxt"),
                  p()),
        nav_panel("Raw Data",
                      h1('Raw data for validation of Bioassay'),
                      p('The data used for the validation of the bioassay are 
                        displayed here after as provided by the user.'),
                      p(),
                  tableOutput("data")),
        nav_panel("Raw Data Plot",
                    h1('Raw data for validation of Bioassay'),
                    p('This graph shows the data used for the validation of the 
                    bioassay.'),
                    p(),
                    plot_ui(id="plot1"),
                    textOutput("text1")),
        nav_panel("Linearity (Log10 trsf)", 
                  h1('Linearity Profile after log10 transformation'),
                  p('This graph shows the linearity profile of the validation of
                  the bioassay after log10 transformation.'),
                  p(),
                  withSpinner(plot1_ui(id="plot2"),type=1, color = "blue",
                              caption=div(strong("Loading"), br(), em("Please wait") )),
                  textOutput("text2")),
        nav_panel("Linearity",
                  h1('Linearity Profile'),
                  p('This graph shows the linearity profile of the validation of
                  the bioassay in natural scale (no transformation).'),
                  p(),
                  withSpinner(plot1_ui(id="plot3"),type=1, color = "blue",
                              caption=div(strong("Loading"), br(), em("Please wait") ) ),
                  textOutput("text3")),
        nav_panel("Total Error Profile",
                  h1('Total Error Profile'),
                  textOutput("text5"),
                  p(),
                  withSpinner(plot3_ui("plot5"),type=1, color = "blue",
                              caption=div(strong("Loading"), br(), em("Please wait") ) )),
        nav_panel("Probability", 
                  h1('Probability Profile'),
                  textOutput("text4"),
                  p(),
                  withSpinner(plot2_ui("plot4"),type=1, color = "blue",
                              caption=div(strong("Loading"), br(), em("Please wait") ) )),
        nav_panel("Advanced Settings",
                  h1('Advanced Settings'),
                  
                layout_columns(
                  h2('Priors definition'),
                  p(),
                  card(
                    numericInput(
                    inputId = "varSlope",
                    label = "Variance of the slope prior:",
                    value = 1,
                    min = 0.01, max = 2,
                    step = 0.01
                  )),
                  card(
                    plot4_ui("Prior1")
                  ),
                  
                  card(
                    numericInput(
                    inputId = "varInterc",
                    label = "Variance of the intercept prior:",
                    value = 1,
                    min = 0.01, max = 2,
                    step = 0.01
                  )),
                  card(
                    plot4_ui("Prior2")
                    ),
                  
                  card(
                    numericInput(
                    inputId = "IgammaMean",
                    label = "Mean for the repeatability variance prior:",
                    value = 0.02,
                    min = 0.0001, max = 10,
                    step = 0.0001
                  ),
                  numericInput(
                    inputId = "IgammaVar",
                    label = "Variance for the repeatability variance prior:",
                    value = 0.0001,
                    min = 0.0001, max = 10,
                    step = 0.0001
                  )),
                  card(
                    plot4_ui("Prior3")
                       ),
                  col_widths = c(4, 8, 4, 8, 4, 8),
                  actionButton("InfP", "Apply Priors"),
                  actionButton("FlatP", "Reset to Flat Priors"),
                  
                )
              )
       )
    )
  )
)



# Define server logic ----
server <- function(input, output) {

  #1. Readme text  
  output$ReadMeTxt <- renderUI({
    HTML("<p>The aim of this app are:<br>
                <ol>
                  1. Analyze validation data of bioassay using a Bayesian approach,<br> 
                  2. Implement what I think is a good way to do so,<br> 
                  3. A toy project to help me learn various technical tools.<br>
                </ol>
                  </p>
                  <p>
                  It is a personal project, a personal vision. You do not like it:
                    do not use it.<br> I am however open to any constructive criticism: 
                    please shoot: <a href='mailto:rozeteric@gmail.com'> send here ! </a>
                     
                    </p>
                    
                    <p>
                    This app propose a way to analyze bioassay validation data which is nonetheless
                    coherent with most regulatory guidances (ICH Q2, ICH M10, USP 1033, and FDA related guidances).<br>
                    It is a Bayesian approach that uses Stan via brms package: so it may take few seconds to process.<br> 
                    </p>
                    <p>
                    A pdf report is also available to download which includes more output than the app:<br>
                    <ol>
                    - Precision of the bioassay over the whole range tested<br>
                    - Linearity of the results<br>
                    - Accuracy of the bioassay by target potency<br>
                    - Total analytical error<br>
                    </ol>
                    </p>
                    <p>        
                    And for decision making about the validity of the assay a <em><b>Probability profile</em></b> that will allow to define 
                    the valid range.<br>
                    </p>
                    <p>
                    Data should be uploaded as csv file with 5 columns labelled exactly as:<br>
                    <ol>
                    - <em><b>Target</b></em> : the true or target potencies;<br>
                    - <em><b>Run</b></em> : the bioassay run;<br>
                    - <em><b>Res</b></em> : the measured potencies;<br>
                    - <em><b>LRes</b></em> : the log 10 transformed measured potencies;<br>
                    - <em><b>LTarget</b></em> : the log 10 transformed true or target potencies;<br>
                    </ol>
                    </p>
                    <p><br><br>
                    </p>"
    )
  })
  
  #2. Data input & Table & Plot  
  data <- datasetServer("dataset")
  output$data <- renderTable(data())
  
  plot_server(id="plot1", dtf=data(),  xlab="Target potency", ylab="Measured potency", 
              title= "Potency data and acceptance criteria", AccLim=reactive(input$L))
  output$text1 <- renderText({paste("The red dashed lines are the acceptance 
                                      criteria selected of:",input$L, "%")})  
  
  
  #3. Some Internal functions
  new.x <- reactive({
    seq(from=round(min(data()$LTarget),2), to=round(max(data()$LTarget),2), by=0.01 ) 
  })
  
  Tbl1 <- reactive({
    mod(df=data(), MyPrior=v$Prior)
  })
  
  Tbl2 <- reactive({
    predictBayes(post.fit01=Tbl1(), Lrel=input$L, nRep=input$nRep, nRun=input$nRun, nx=new.x())
  })

  
  #4. Plots for Linearity Log10 and natural scale
  plot1_server("plot2", dtf1=reactive(Tbl2()[1:10000,]), dtf2=data(), xlab="Log10 Target potency",
               ylab="Log10 Measured potency",
               title="Log10 linearity graph and acceptance criteria",
               tpe="Log10")
  output$text2 <- renderText({paste("The red dashed lines are the acceptance 
                                      criteria selected of:",input$L, "%")})  
  
  plot1_server("plot3", dtf1=reactive(Tbl2()[1:10000,]), dtf2=data(), xlab="Target potency",
               ylab="Measured potency",
               title="linearity graph and acceptance criteria",
               tpe="Linear")
  output$text3 <- renderText({paste("The red dashed lines are the acceptance 
                                      criteria selected of:",input$L, "%")})  

  #5. Probability plot 
  plot2_server("plot4", dtf1=reactive(probaSpec(dta=Tbl2())), p=reactive(input$Prob), 
               xlab="Target potency", 
               ylab="Probability to be within acceptance criteria",
               title="Probability to be within acceptance criteria")  
  output$text4 <- renderText({paste("This graph shows the probability of having 
                                    bioassay results within the acceptance limits
                                    of:",input$L, "%. The minimum required 
                                    probability of: ", input$Prob ," is also displayed as a
                                    red dashed line.")})

  #6. Total Error plot  
  plot3_server("plot5", dtf1=reactive(TISpec(dta=Tbl2(), p=input$Prob)),
               dtf2=data(), AccLim=reactive(input$L), 
               xlab="Target potency",
               ylab="Relative Error (%)",
               title="Total Error profile and acceptance criteria")
  output$text5 <- renderText({paste("This graph shows the total error of the results of the bioassay
                   displayed in blue: it is an interval where it is expected that
                   each results will fall in with a defined probability. The probability
                   has been defined as:",input$Prob, ". The red dashed lines are the acceptance criteria selected 
                    by the user of:", input$L ,"%.")})

  
  #7. PDF report generation
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    #filename = "report.html",
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path("report.Rmd")
      #file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(dta_val = data(), L=input$L, T2=Tbl2(), pi=input$Prob,
                     nRun=input$nRun, nRep=input$nRep, T1=Tbl1(), Prior=v$Prior)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
  
  #8. Prior section
  
  #8.1 Detect which prior to put into the report
  v <- reactiveValues(Prior = NULL)
  observeEvent(input$InfP, {
    scaleIGamma<-input$IgammaMean**3/input$IgammaVar+input$IgammaMean
    shapeIGamma<-input$IgammaMean**2/input$IgammaVar+2
    v$Prior <- c(set_prior(paste("normal(1,",input$varSlope,")"), class="b", coef="LTarget"),
                 set_prior(paste("normal(0,",input$varInterc,")"), class="Intercept"),
                 set_prior(paste("inv_gamma(",shapeIGamma,",", scaleIGamma,
                 ")"), class="sigma")
                )
  })
  observeEvent(input$FlatP, {
    v$Prior <- default_prior(data = data(),
                             family = gaussian(),
                             LRes ~ 1 + LTarget + (1 + LTarget|Run))
  })
  
  #8.2 Plot priors
  r <- reactiveValues()
  observe({
    r$InfPrior <- input$InfP
    r$NPrior <- input$FlatP
  })
    
    plot4_server(id="Prior1", normMu= 1, Var=reactive(input$varSlope), xlab="Slope",
                 title="Prior distribution of slope",
                 tpe="normal", r=r)
    plot4_server(id="Prior2", normMu= 0, Var=reactive(input$varInterc), xlab="Intercept",
                 title="Prior distribution of Intercept",
                 tpe="normal", r=r)
    plot4_server(id="Prior3", Mu=reactive(input$IgammaMean),
                 Var=reactive(input$IgammaVar),
                 xlab="Repeatability variance",
                 title="Prior distribution of Repeatability Variance",
                 tpe="iGamma", r=r)
    

} 
  
# Run the app ----
shinyApp(ui = ui, server = server)


  
  
  



  
  


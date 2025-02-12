###### Priors module

plot4_ui <- function(id) {
  ns <- NS(id)
  plotOutput(outputId=ns("plot"))
}

plot4_server <- function(id, Mu=0, normMu=1, Var, xlab, title, tpe, r ){
  moduleServer(id=id,
               module=function(input, output, session) {
                 
                 observeEvent(r$InfPrior,{
                   
                   output$plot <- renderPlot({
                     
                     if(tpe=="normal"){ 
                       priordf1<-data.frame(obs=rnorm(n=10000, mean=normMu, sd=sqrt(isolate(Var()))))
                       ggplot(data=priordf1, aes(x=obs)) +
                         geom_density() +
                         labs(x=xlab, y="", title= title)
                     } else if(tpe=="iGamma"){
                       scaleIGamma<-isolate(Mu())**3/isolate(Var())+isolate(Mu())
                       shapeIGamma<-isolate(Mu())**2/isolate(Var())+2
                       priordf1<-data.frame(obs=1/rgamma(n=10000, shape=shapeIGamma, scale=1/scaleIGamma))
                       ggplot(data=priordf1, aes(x=obs)) +
                         geom_density() +
                         labs(x=xlab, y="", title= title)
                     }
                     
                   })
                   
                 })
                 
                 observeEvent(r$NPrior,{
                   
                   output$plot <- renderPlot({
                     if(is.null(input$FlatP)){return()} else {return()}
                   })  
                   
                 })
                 
                 
                 
               })
}



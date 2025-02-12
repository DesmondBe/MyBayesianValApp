#### Module for all plots

#1. Raw data plot
plot_ui <- function(id) {
  ns <- NS(id)
  plotOutput(outputId=ns("plot"))
}

plot_server <- function(id, dtf, xlab, ylab, title, AccLim){
  moduleServer(id=id,
               module=function(input, output, session) {
                output$plot <- renderPlot({
                  L<-(1-AccLim()/100)
                  ggplot(dtf, aes(x=Target, y=Res, colour=as.factor(Run))) +
                    geom_point() +
                    geom_line(aes(x=Target, y=L*Target ), colour = "red", linetype = 2) +
                    geom_line(aes(x=Target, y=1/L*Target ), colour = "red", linetype = 2) +
                    labs(colour="Runs", x=xlab, y=ylab, title= title)
                })
               }
              )
            }


#2. Linearity plots
plot1_ui <- function(id) {
  ns <- NS(id)
  plotOutput(outputId=ns("plot"))
}

plot1_server <- function(id, dtf1, dtf2, xlab, ylab, title, tpe){
  moduleServer(id=id,
               module=function(input, output, session) {
                 output$plot <- renderPlot({
                   if (tpe=="Log10") {
                   ggplot(dtf1(), aes(x=LTarget, y=LRes)) +
                     geom_point() +
                     geom_point(data=dtf2, aes(colour=as.factor(Run))) +
                     geom_line(aes(x=LTarget, y=Uspec ), colour = "red", linetype = 2) +
                     geom_line(aes(x=LTarget, y=Lspec ), colour = "red", linetype = 2) +
                     labs(colour="Runs", x=xlab, y=ylab, 
                          title= title)
                   } else if (tpe=="Linear")
                   {
                     ggplot(dtf1(), aes(x=10**LTarget, y=10**LRes)) +
                       geom_point() +
                       geom_point(data=dtf2, aes(colour=as.factor(Run))) +
                       geom_line(aes(x=10**LTarget, y=10**Uspec ), 
                                 colour = "red", linetype = 2) +
                       geom_line(aes(x=10**LTarget, y=10**Lspec ), 
                                 colour = "red", linetype = 2) +
                       labs(colour="Runs", x=xlab, y=ylab, 
                            title= title)
                   }
                })
               }
  )
}


#3. Probability plot
plot2_ui <- function(id) {
  ns <- NS(id)
  plotOutput(outputId=ns("plot"))
}

plot2_server <- function(id, dtf1, p, xlab, ylab, title){
  moduleServer(id=id,
               module=function(input, output, session) {
                 output$plot <- renderPlot({
                     ggplot(dtf1(), aes(x=10**LTarget, y=pi)) +
                       geom_smooth(color="blue") + 
                       geom_hline(yintercept=p(), linetype="dashed", color="red") +
                       labs(x=xlab, y=ylab, 
                            title= title)
                     })  
                   
                 })
               }
  


#4. Total Error plot
plot3_ui <- function(id) {
  ns <- NS(id)
  plotOutput(outputId=ns("plot"))
}

plot3_server <- function(id, dtf1, dtf2, AccLim, xlab, ylab, title){
  moduleServer(id=id,
               module=function(input, output, session) {
                 output$plot <- renderPlot({
                   L <-1-AccLim()/100
                   U<- 1/L
                   ggplot(dtf1(), aes(x=10**LTarget, y=U975)) +
                     geom_smooth(color="blue") +
                     geom_smooth(aes(x=10**LTarget, y=L025), color="blue") +
                     geom_hline(yintercept=100*(L-1), linetype="dashed", color="red") +  
                     geom_hline(yintercept=100*(U-1), linetype="dashed", color="red") +
                     geom_point(data=dtf2, aes(x=Target, y=100*(Res-Target)/Target, colour=as.factor(Run))) +
                     labs(colour="Runs", x=xlab, y=ylab, 
                          title= title)
                  })  
              })
}


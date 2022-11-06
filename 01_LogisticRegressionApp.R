#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Probability for failing XXXX OFC (Sample program)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("Age", "Age [years]:", 3, step=1.0),
      checkboxInput("BA", "Bronchial asthma"),
      checkboxInput("AD", "Atopic dermatitis"),
      checkboxInput("AR", "Allergic rhinitis"),
      numericInput("IgE", "Total IgE  [IU/L]:", 100, step=1.0),
      numericInput("EWsIgE", "Egg white specific IgE [kUA/L]:", 2.0, step=0.1),
      numericInput("OVMsIgE", "Ovomucoid specific IgE [kUA/L]:", 2.0, step=0.1),
      helpText("Original article is: Phenotyping of immediate-type food allergies based on 10 years of research: A latent class analysis "),
      helpText("doi:10.1111/pai.13873"),
      helpText("Programmed by Kyohei Takahashi, Sagamihara National Hospital, Japan"),
      helpText("Disclaimer: This is a sample program for visualization of model"),
      helpText("For more information, see: "),
      helpText("https://github.com/kcrt/iAnetLCAOpen")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("Subjects in our study:"),
      plotOutput("OriginalPlot"),
      h2("Simulation result:"),
      plotOutput("FittedPlot")
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  d = readRDS("01_LogisticRegressionData.rds")
  model = readRDS("01_LogisticRegressionModel.rds")
  
  output$OriginalPlot = renderPlot({
    ggplot(d, aes(x=log10(OVMsIgE), y=Result)) +
      ggtitle("Probability curve for OVM sIgE [kU/L]") +
      stat_smooth(formula = y ~ x, method="glm", method.args=list(family="binomial"), linewidth=1.5, se=T, fullrange=T) + 
      coord_cartesian(xlim=c(-2, 3), ylim=c(0, 1)) + xlim(-10, 10)
  })
  
  prob_curve <- function(log10OVM){
    logit_p <-
      model$coefficients["Age"] * input$Age +
      model$coefficients["log10(IgE)"] * log10(input$IgE) +
      model$coefficients["log10(EWsIgE)"] * log10(input$EWsIgE) +
      model$coefficients["log10(OVMsIgE)"] * log10OVM +
      model$coefficients["BA"] * input$BA +
      model$coefficients["AD"] * input$AD +
      model$coefficients["AR"] * input$AR + 
      model$coefficients["(Intercept)"]
    return(exp(logit_p) / (1 + exp(logit_p)))
  }
  output$FittedPlot = renderPlot({
    newdata = d[1,]
    newdata$Age = input$Age
    newdata$Sex = input$Sex
    newdata$IgE = input$IgE
    newdata$EWsIgE = input$EW
    newdata$OVMsIgE = input$OVM
    newdata$BA = input$BA
    newdata$AD = input$AD
    newdata$AR = input$AR
    
    x = log10(input$OVMsIgE)
    y = prob_curve(x)
    
    ggplot() +
      geom_function(fun = prob_curve, linewidth=1.5) +
      annotate("text", x=x, y=ifelse(y>0.8, 0, 1), label=paste0(format(y * 100, digits=2), " %"), size=5) + 
      annotate("text", x=x, y=y-0.015, label="*", colour="red", size=10) + 
      # geom_point(x = log10(input$OVMsIgE),
      #           y = prob_curve(log10(input$OVMsIgE)),
      #           colour="red",
      #           size=5) + 
      coord_cartesian(xlim=c(-2, 3), ylim=c(0, 1)) + xlim(-10, 10)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

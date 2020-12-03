library(shiny)
library(shinyRGL)
shinyUI(
  fluidPage(
    
    # Application title
    titlePanel("COVID-19 analysis"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
      sidebarPanel(
        h3("Enter data of new patient:"),
        sliderInput("LDH", "LDH U/l", min=100, max=2000, value=200),
        sliderInput("CRP", "High sensitivity C-reactive protein", min=0, max=350, value=100),
        sliderInput("NEUTR", "Neutrophils (%)", min=0, max=100, value=50)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("prediction"),
        
        #htmlOutput("plot")
        #webGLOutput("webGL")
        rglwidgetOutput("webGL", width = 600, height = 600)
      )
    )
  )
)
library(shiny)
library(textclean)
library(tm)
library(stylo)
library(data.table)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Next Word prediction with Kneser-Kney smoothing"),
  
  sidebarLayout(
    sidebarPanel(
      h3("User Input"),
      br(),
      
      strong(""),
      textInput("text", "type your text here"),
      
      h3("select number of words to predict"),
      numericInput("n", "numbers of words to predict", 
                   value = 5, min = 1,max = 10, step = 1),

      br(),
      
      strong("Click the button below to return the predicted words."),
      actionButton("buttonPredict", "Predict Next Words")
      
    ),
    
    
    # Show the top probable words
    mainPanel(
      br(),
      h4('The text as interpreted by the application:'),
      verbatimTextOutput("clnText"),
      
      br(),
      h4('The word predicted based on the phrase provided:'),
      verbatimTextOutput("nxtWord")
    )
  )
))

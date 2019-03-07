#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(textclean)
library(tm)
library(data.table)
library(stylo)


load("data.RData")


source("function.R")




# Define server 
shinyServer(function(input, output) {
  nextWord <- eventReactive(input$buttonPredict,{
    getWords(input$text, input$n)
  })
  output$nxtWord <- renderText({
    nextWord()
  })
  # Display 'clean' version of user text
  adjustedTxt <- eventReactive(input$buttonPredict, {
    paste(cleanInput(input$text))
  })
  output$clnText <- renderText({ adjustedTxt() })
  
})

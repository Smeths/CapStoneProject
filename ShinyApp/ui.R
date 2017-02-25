library(shiny)

shinyUI(fluidPage(
        titlePanel("Text Predictor"),
        sidebarPanel(
           h3("Input:"),
           h3("Two Words (wordAB):"),
           textInput(inputId = "wordAB", label="", value = "")),
        mainPanel(
          h3("Output:"),
          h3("Prediction (wordC):"),
          verbatimTextOutput("wordC"),
          br(),
          h3("Model Used:"),
          verbatimTextOutput("predtype"),
          br(),
          h3("Word C Conditional Probability:"),
          verbatimTextOutput("conprobs"),
          br(),
          h3("Warnings:"),
          verbatimTextOutput("warning"),
          br(),
          h3("Section of Data Frame:"),
          verbatimTextOutput("df")
          )
       )
)

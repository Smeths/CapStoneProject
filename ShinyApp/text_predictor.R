library(shiny)

ui <- fluidPage(
  titlePanel("Text Predictor"),
  sidebarPanel(  
     h3("Enter Two Words:"),
     textInput(inputId = "wordAB", label="", value = ""),
     br(),
     h3("Predition is:"),
     textOutput("word")),
  mainPanel(
     h3("Probabilities:"),
     verbatimTextOutput("probs")
  )
)

server <- function(input, output) {
# Opening trigram file and checking input has two words
  trigrams <- read.csv("data/conprobdummydata.csv")
  countstr <- reactive({
    sapply(strsplit(input$wordAB, " "), length)
  })
# Getting probabilities and most likely word
  getprobs <- reactive({
    trigrams[trigrams$wordAB==input$wordAB,]
  })
  getword <- reactive({as.character(trigrams[trigrams$wordAB==input$wordAB,][1,2])})
# Outputting probabilties and words
  output$probs <- renderPrint({
     if ( countstr() == 2){
        getprobs()
     }
  })
  output$word <- renderPrint({
    if ( countstr() == 2){
      getword()
    }
  })
}

shinyApp(ui = ui, server = server)
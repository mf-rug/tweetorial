library(tidyverse)
library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      pre {
        white-space:pre-wrap;
      }"))
  ),
  headerPanel("TweetRial"),
  sidebarLayout(
    position = 'right',
    sidebarPanel(
      HTML('<strong>Preview</strong>'), 
      br(), 
      uiOutput('TweetNums'),
      br(), 
      div(style = 'width:100%; padding:10px', uiOutput("preview"))
    ),
    
    mainPanel(
      column(
        12,
        textAreaInput('text', 'Enter text here:', width = "100%", height = '50vh'),
        HTML('<strong>Options</strong>'), hr(), 
        radioGroupButtons('sep', 'Separate tweets at', c('Exactly 280 characters', 'Nearest whitespace', 'Nearest comma, semi-colon, colon, full stop', 'Nearest full stop'), 'Nearest full stop', 
                          status = 'primary', direction = 'vertical')
      )
    )
  )
)

server <- function(input, output) {

  max_fields <- eventReactive(input$text, {
    max(1, ceiling(str_count(input$text) / 280))
  })
  
  output$TweetNums <- renderUI({
    HTML(paste('<i>Number of Tweets:', max_fields(), '</i>'))
  })
  
  output$preview <- renderUI({
    text_output_list <- lapply(1:max_fields(), function(i) {
      fieldname <- paste("field", i, sep="")
      verbatimTextOutput(fieldname, )
    })
    

    do.call(tagList, text_output_list)
  })

  observe({
    for (i in 1:max_fields()) {
      local({
        my_i <- i
        fieldname <- paste("field", my_i, sep="")
        output[[fieldname]] <- renderPrint({
          cat(str_sub(input$text, (my_i * 280 ) - 279, my_i * 280))
        })
      })
    }
  })
}

shinyApp(ui, server)
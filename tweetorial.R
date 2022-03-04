library(tidyverse)
library(shiny)


ui <- fluidPage(
  headerPanel("Dynamic number of texts"),
  sidebarPanel(
    HTML('<strong>Preview</strong>'), br(), br(),
    sliderInput("n", "Number of plots", value=1, min=1, max=5),
    uiOutput('preview')
  ),
  
  mainPanel(
    textAreaInput('text', 'Enter text here:', width = "100%", height = '50vh')
  )
)

max_texts <- 5

server <- function(input, output) {
  
  # Insert the right number of plot output objects into the web page
  output$preview <- renderUI({
    text_output_list <- lapply(1:input$n, function(i) {
      fieldname <- paste("plot", i, sep="")
      uiOutput(fieldname, height = 280, width = 250)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, text_output_list)
  })
  
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  for (i in 1:max_texts) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      textname <- paste("text", my_i, sep="")
      
      output[[textname]] <- renderPrint({
        input$text
      })
    })
  }
}

shinyApp(ui, server)
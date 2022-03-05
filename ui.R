library(tidyverse)
library(shiny)
library(shinyWidgets)
library(rclipboard)

ui <- fluidPage(
  rclipboardSetup(),
  tags$head(
    tags$style(HTML("
    #sidebar {
      background-color: ghostwhited;
    }
    .header {
          display: flex;
          justify-content: space-between;
          margin: 0 auto;
        }"))
  ),
  headerPanel("TweetRial"),
  sidebarLayout(
    position = 'right',
    sidebarPanel(
      id="sidebar",
      style = "overflow-y:scroll; max-height: 85vh; position:relative;",
      uiOutput('TweetNums'),
      div(style = 'width:100%; padding:10px', uiOutput("preview"))
    ),
    
    mainPanel(
      column(
        12,
        textAreaInput('text', NULL, placeholder = 'Enter text here', width = "100%", height = '50vh'),
        uiOutput('length'),
        HTML('<strong>Options</strong>'), HTML('<hr style="border-color: #cacaca; margin-bottom:10px; margin-top:2px;"/>'),
        div(style="display: inline-block;vertical-align:top; margin-top:7px;",
            HTML('<span title="Enter all characters at which text can be split into separate Tweets.\nMatch new lines with \\n"><strong>Separate Tweets at these characters:&nbsp</strong></span>')),
        div(style="display: inline-block;vertical-align:top",
            textInput('sep', NULL, '\\n ,;', width = "12ch")), br(),
        div(style="display: inline-block;vertical-align:top; margin-top:7px;",
            HTML('<span title="Use \'NUM\' as the current Tweet number and \'TOTAL\' for the total number of Tweets."><strong>Tweet number prefix:&nbsp</strong></span>')),
        div(style="display: inline-block;vertical-align:top",
            textInput('prefix', NULL, '', width = "22ch")),br(),
        div(style="display: inline-block;vertical-align:top; margin-top:7px;",
            HTML('<span title="Use \'NUM\' as the current Tweet number and \'TOTAL\' for the total number of Tweets."><strong>Tweet number suffix:&nbsp</strong></span>')),
        div(style="display: inline-block;vertical-align:top",
            textInput('suffix', NULL, ' NUM/TOTAL', width = "22ch")),
        uiOutput('message')
      )
    )
  )
)
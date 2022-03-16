library(tidyverse)
library(shiny)
library(shinyWidgets)
library(rclipboard)
library(htmltools)
library(emo)

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
        }")),
    tags$script(
      'Shiny.addCustomMessageHandler("refocus",
                                    function(NULL) {
                                      document.getElementById("text").focus();
                                    });'
    )
  ),
  HTML("<title>TweetRial</title>"),
  div(style = 'margin-top:-10px;', class = 'col-sm-12', HTML("<h1>TweetRial</h1>")), theme = bslib::bs_theme(version = bslib::version_default(), bootswatch = "cerulean"),
  column(12, div(style = 'margin-top:-15px;margin-bottom:10px', HTML('<i><small>An R shiny app to create Twitter threads from longer texts.</small></i>'))),
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
        textAreaInput('text', NULL, placeholder = 'Start typing some text here or paste', width = "100%", height = '50vh'),
        div(class='header', 
            div(style="display: inline-block;vertical-align:top; margin-top:-5px; margin-bottom:-2px",
                pickerInput(inputId = 'emoji', label = HTML("<i><font color='grey'>Copy emoji to clipboard:</font></i>"), 
                            choices = c(jis[!is.na(jis$vendor_twitter) & jis$qualified == 'fully-qualified', 'emoji'])$emoji,
                            choicesOpt = list(subtext = (jis[!is.na(jis$vendor_twitter) & jis$qualified == 'fully-qualified', 'name'])$name), 
                            selected = '', width = 'fit', options = list(`live-search` = TRUE, title = "🙂"))),
            div(style="display: inline-block;vertical-align:top; margin-top:-5px; margin-bottom:-2px",
                uiOutput('length'))
        ),
        HTML('<strong>Options</strong>'), HTML('<hr style="border-color: #cacaca; margin-bottom:10px; margin-top:2px;"/>'),
        div(style="display: inline-block;vertical-align:top; margin-top:7px;",
            HTML('<span title="Enter all characters at which text can be split into separate Tweets.\nMatch new lines with \\n"><strong>Separate Tweets at these characters:&nbsp</strong></span>')),
        div(style="display: inline-block;vertical-align:top",
            textInput('sep', NULL, '\\n.:;', width = "12ch")), br(),
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
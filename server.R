server <- function(input, output) {
  split_list <- eventReactive(c(input$text, input$sep, input$suffix, input$prefix), {
    if (input$text == '') {
      list()
    } else {
      s1 <- data.frame('l' = str_split(input$text,'')[[1]])
      split_by <- ' '
      splits <- list()
      msg <- ''
      split_end <- 0
      i <- 1
      while (split_end < nrow(s1)) {
        suffix <- str_replace_all(str_replace_all(input$suffix, 'NUM', as.character(i)), 'TOTAL', 'ßü')
        prefix <- str_replace_all(str_replace_all(input$prefix, 'NUM', as.character(i)), 'TOTAL', 'ßü')
        max_len <- 280 - str_count(suffix) - str_count(prefix)
        if (input$sep == '') {
          split_by <- '.'
        } else {
          split_by <- paste0('[', paste0(input$sep, collapse = '') , ']')
        }
        if (i == 1) {
          split_start <- 1
        } else {
          split_start <- splits[[i - 1]][2] + 1
        }
        split_max <- str_sub(input$text, split_start, split_start + max_len -1)
        split_end <- str_locate_all(split_max, split_by)[[1]][,'start'] 
        if (identical(split_end, integer(0)) && str_count(input$text) > max_len) {
          split_end <- max_len
          msg <- 'These separators don\'t allow splits of less than 280 characters; change text or separators.'
        } else {
          msg <- ''
          split_end <- max(split_end)
        }
        if (i > 1) {
          split_end <- split_end + splits[[i - 1]][2]
          split_start <- splits[[i - 1]][2] + 1
        }
        if (length(split_start:nrow(s1)) < max_len) {
          split_end <- nrow(s1)
        }
        splits[[i]] <- c(split_start, split_end, length(split_start:split_end))
        i <- i + 1
      }
      append(splits, c('msg' = msg))
    }
  })
  
  output$length <- renderUI({
    HTML(paste('<div style="text-align:right;"><font color="grey"><i>', str_count(input$text), 'characters'), '</i></font></div>')
  })
  output$TweetNums <- renderUI({
    HTML(paste0('<strong><big>Previewing ', max(0,length(split_list()) -1), ' Tweet',ifelse(max(0,length(split_list()) -1) == 1, '', 's'),'</big></strong><hr style="border-color: black; margin-bottom:2px; margin-top:2px;"/>'))
  })
  
  output$message <- renderUI({
    HTML(paste0('<font color="grey"><i>', split_list()[['msg']],'</i></font></div>'))
  })
  
  output$preview <- renderUI({
    if (input$text != '') {
      text_output_list <- lapply(seq_len(max(0,length(split_list()) -1)), function(i) {
        fieldname <- paste("field", i, sep="")
        uiOutput(fieldname)
      })
      
  
      do.call(tagList, text_output_list)
    }
  })

  observe({
    for (i in seq_len(max(0,length(split_list()) -1))) {
      local({
        my_i <- i
        fieldname <- paste("field", my_i, sep="")
        part_str <- dipsaus::html_asis(str_sub(input$text, 
                                               split_list()[[my_i]][1], 
                                               split_list()[[my_i]][2]), 
                                       space = FALSE)
        suffix <- str_replace_all(str_replace_all(input$suffix, 'NUM', as.character(my_i)), 'TOTAL', as.character(length(split_list()) -1))
        prefix <- str_replace_all(str_replace_all(input$prefix, 'NUM', as.character(my_i)), 'TOTAL', as.character(length(split_list()) -1))
        part_str <- paste0(prefix, part_str, suffix)
        output[[fieldname]] <- renderUI({
          div(
            div(class='header', 
                div(
                    a(tags$i(class = "fas fa-user-circle", style = "font-size: 40px")),
                    HTML('&nbsp&nbsp'),
                    HTML('<strong>You</strong> @your_twitter · ',
                         format(Sys.Date(), "%e %b"))
                    ),
                rclipButton(inputId = paste0('cp', my_i), label = NULL, part_str, icon = icon("copy"))
                
            ),
            div(style = "line-height:100%;", br()),
            HTML(paste0(
              '<p style = "background-color:white;">',
              part_str,
              '</p>')),
            div(style = "line-height:25%;", br()),
            div(class='header', 
              div(style="display: inline-block;vertical-align:top", a(icon(verify_fa = FALSE,'comment-o')), HTML('&nbsp0')),
              div(style="display: inline-block;vertical-align:top", a(icon(verify_fa = FALSE,'retweet')), HTML('&nbsp0')),
              div(style="display: inline-block;vertical-align:top", a(icon(verify_fa = FALSE,'heart-o')), HTML('&nbsp0')),
              div(style="display: inline-block;vertical-align:top", HTML('&nbsp')),
              div(style="display: inline-block;vertical-align:top", HTML(paste0('<i><font color="grey";>',str_count(part_str), '/280</font></i>'))),
            ),
            HTML('<hr style="width:90%;border-color: #8e8e8e; margin-top:5px; margin-bottom:16px"/>')
          )
        })
      })
    }
  })
}

shinyApp(ui, server)
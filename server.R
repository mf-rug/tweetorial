server <- function(input, output, session) {
  url_reg <- 'https?:\\/\\/(?:www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b(?:[-a-zA-Z0-9()@:%_|\\+.~,#?&//=]*)'
  
  ect <- reactive({
    emoji_cor_text <- str_replace_all(input$text, '❤', '❤️')
    save_emos <- c()
    unique_string <- ''
    msg <- ''
    if (ji_detect(emoji_cor_text)) {
      msg <- 'Your text contains emojis, those count as <a href = "https://developer.twitter.com/en/docs/counting-characters" target = "_blank">2 character each</a>.'
      save_emos <- ji_extract_all(emoji_cor_text)[[1]]
      while (unique_string == '') {
        #create random two character string but don't use special regex or html characters
        ra <- paste0(rawToChar(as.raw(sample(setdiff(33:124, c(34, 36, 38:43, 45:46, 60, 62:63, 91:94, 123:124)),1))), 
                     rawToChar(as.raw(sample(setdiff(33:124, c(34, 36, 38:43, 45:46, 60, 62:63, 91:94, 123:124)),1))))
        #check if that string occurs in the text or pre/suffix
        if (!str_detect(emoji_cor_text, ra) && !str_detect(input$prefix, str_sub(ra,1,1)) && !str_detect(input$prefix, str_sub(ra,2,2))) {
          #if not, accept as emoji placeholder
          unique_string <- ra
        }
      }
      emoji_cor_text <- ji_replace_all(emoji_cor_text, unique_string)
    }
    # print(list(emoji_cor_text, save_emos, unique_string, msg))
    list(emoji_cor_text, save_emos, unique_string, msg)
  })
  
  split_list <- reactive({
    if (input$text == '') {
      list()
    } else {
      # replace any urls with 23 'a's, text doesn't matter here as we're just counting
      url_cor_text <- str_replace(ect()[[1]], url_reg, paste0(rep('a', 23), collapse = ''))
      if (str_detect(ect()[[1]], url_reg)) {
        msg2 <- 'Your text contains links. These will be counted as 23 characters according to <a href = "https://help.twitter.com/en/using-twitter/how-to-tweet-a-link" target = "_blank">Twitters url shortening rules</a>.'
      }  else {
        msg2 <- ''
      }
      
      s1 <- data.frame('l' = str_split(url_cor_text,'')[[1]])
      split_by <- ' '
      splits <- list()
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
        split_max <- str_sub(url_cor_text, split_start, split_start + max_len -1)
        split_end <- str_locate_all(split_max, split_by)[[1]][,'start'] 
        if (identical(split_end, integer(0)) && str_count(url_cor_text) > max_len) {
          split_end <- max_len
          msg <- 'These separators don\'t allow splits of less than 280 characters; change text or separators.'
        } else {
          msg <- ''
          split_end <- suppressWarnings(max(split_end))
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
      append(splits, c('msg' = paste0(msg, '<br>', msg2)))
    }
  })
  
  output$length <- renderUI({
    HTML(paste('<div style="text-align:right;"><font color="grey"><i>', str_count(ect()[[1]]), 'characters'), '</i></font></div>')
  })
  output$TweetNums <- renderUI({
    HTML(paste0('<strong><big>Previewing ', max(0,length(split_list()) -1), ' Tweet',ifelse(max(0,length(split_list()) -1) == 1, '', 's'),'</big></strong><hr style="border-color: black; margin-bottom:2px; margin-top:2px;"/>'))
  })
  
  output$message <- renderUI({
    HTML(paste0('<font color="grey"><i>', split_list()[['msg']],'<br>', ect()[[4]], '</i></font></div>'))
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

  observeEvent(input$emoji, {
    if (input$emoji != '') {
      clipr::write_clip(input$emoji)
    }
    updatePickerInput(session = getDefaultReactiveDomain(), inputId = 'emoji', selected = '')
    session$sendCustomMessage(type="refocus",message=list(NULL))
    }, ignoreInit = TRUE)

  observe({
    for (i in seq_len(max(0,length(split_list()) -1))) {
      local({
        my_i <- i
        fieldname <- paste("field", my_i, sep="")
        
        # save the urls and replace with (presumably) unique 23 character string placeholder
        save_urls.df <- data.frame(urls = str_extract_all(ect()[[1]], url_reg)[[1]])
        if (nrow(save_urls.df) > 0) {
          save_urls.df$placeholder <- paste0(sprintf('%0.2d', seq_len(nrow(save_urls.df))), '!¬;ß;zZ~ü£@P£=£6JOF£"')
          url_cor_text <- str_replace(ect()[[1]], 
                                      paste0(rep(paste0('(.*)', url_reg), nrow(save_urls.df)), collapse=''),  
                                      paste0('\\', paste0(seq(1,nrow(save_urls.df)),  save_urls.df$placeholder), collapse = ''))
        } else {
          url_cor_text <- ect()[[1]]
        }

        part_str <- htmltools::htmlEscape(str_sub(url_cor_text, 
                                               split_list()[[my_i]][1], 
                                               split_list()[[my_i]][2]))
        suffix <- str_replace_all(str_replace_all(input$suffix, 'NUM', as.character(my_i)), 'TOTAL', as.character(length(split_list()) -1))
        prefix <- str_replace_all(str_replace_all(input$prefix, 'NUM', as.character(my_i)), 'TOTAL', as.character(length(split_list()) -1))

        part_str <- paste0(prefix, part_str, suffix)
        #add the urls again
        if (nrow(save_urls.df) > 0) {
          part_str_addurl <- mgsub::mgsub(part_str, save_urls.df$placeholder, save_urls.df$urls)
        } else {
          part_str_addurl <- part_str
        }
        #add emojis again
        if (length(ect()[[2]] > 0)) {
          for (i in seq_along(ect()[[2]])) {
            part_str_addurl <- str_replace(part_str_addurl, ect()[[3]], ect()[[2]][i])
          }
        }
        print('as')
        output[[fieldname]] <- renderUI({
          div(
            div(class='header', 
                div(
                    a(tags$i(class = "fas fa-user-circle", style = "font-size: 40px")),
                    HTML('&nbsp&nbsp'),
                    HTML('<strong>You</strong> @your_twitter · ',
                         format(Sys.Date(), "%e %b"))
                    ),
                rclipButton(inputId = paste0('cp', my_i), label = NULL, part_str_addurl, icon = icon("copy"),title="copy subtweet to clipboard")
                
            ),
            div(style="border-left:2px solid #cacaca;margin-left: 1.18em; padding-left: 2.5em;",
              div(style = "line-height:100%;", br()),
              HTML(paste0(
                '<p style = "background-color:white;">',
                str_replace_all(part_str_addurl, '\\n', '<br>'),
                '</p>')),
              div(style = "line-height:25%;", br()),
              div(class='header', 
                div(style="display: inline-block;vertical-align:top", a(icon(verify_fa = FALSE,'comment-o')), HTML('&nbsp0')),
                div(style="display: inline-block;vertical-align:top", a(icon(verify_fa = FALSE,'retweet')), HTML('&nbsp0')),
                div(style="display: inline-block;vertical-align:top", a(icon(verify_fa = FALSE,'heart-o')), HTML('&nbsp0')),
                div(style="display: inline-block;vertical-align:top", a(HTML('<i class="fa fa-sign-out fa-rotate-270" aria-hidden="true"></i>'))),
                div(style="display: inline-block;vertical-align:top", HTML('&nbsp')),
                div(style="display: inline-block;vertical-align:top", HTML(paste0('<i><font color="grey";>',str_count(part_str), '/280</font></i>'))),
              ),
              HTML('<hr style="border-color: #cacaca; margin-top:5px; margin-bottom:2px;padding-bottom:14px"/>')
            )
          )
        })
      })
    }
  })
}

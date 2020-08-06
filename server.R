#--- SERVER ---#

server <- function(input, output) {

  #- PESQUISA
  tweet_data <- reactive({

    if (input$text == "") {
      tryCatch({
        return(readRDS(file = "data/tweets.rds"))
        print("TESTING:::USING CACHED TWEETS")
      }, warning = function(e) {
        NULL
      })
    }

    tweets <- search_tweets2(
      q = input$text,
      lang = input$lang,
      n = input$n)

    tweets <- tweets %>%
      mutate(text_limpo = clean_text(text))

    search_log <- bind_rows(
      data.frame(term = input$text, language = input$lang, num = input$n),
      read.csv("data/search_log.csv", sep = ";")
    )

    write.table(search_log,
                file = "data/search_log.csv",
                sep = ";",
                row.names = FALSE)

    # saveRDS(tweets, file = "data/tweets.rds")

    return(tweets)
  })


  #- TAMANHO BASE
  tam_base <- reactive({
    p <- tweet_data()

    if (is_empty(p)) {
      texto <- paste0("No tweet was found.")

      retorno_ui <- box(width = 12,
                        h4(texto,
                           style = "color:#00A7D0",
                           align = "center")
                        )

    }

    if (!is_empty(p)) {
      texto <- paste0("It was found ", nrow(p), " distinct tweets about ", input$text, ".")
      retorno_ui <- box(width = 12,
                        h4(texto,
                           style = "color:#00A7D0",
                           align = "center"),
                        tags$hr(),
                        downloadButton("tweets", "Download Tweets")
                        )
    }

    return(retorno_ui)

  })


  #-  FREQUENCIA PALAVRAS
  world_frequencies <- reactive({
    tweets <- tweet_data()
    my_stop <- as_tibble(c(unlist(strsplit(input$stop, "\\,\\s|\\,|\\s|\\s\\,|\\s\\,\\s")),
                            stopwords(input$lang)))

    palavras <- tweets %>%
      unnest_tokens(word, text_limpo) %>%
      count(word, sort = TRUE) %>%
      anti_join(my_stop, by = c('word' = 'value'))

    return(palavras)
  })

  #- WORD CLASSIFICATION
  classify_words <- reactive({
    d <- world_frequencies()
    show_words <- input$show_words

    if (input$lang == 'pt') {
      palavras <- palavras_pt %>%
        select(word, positive, negative)
    }

    if (input$lang == 'es') {
      palavras <- palavras_es %>%
        select(word, positive, negative)
    }

    if (input$lang == 'en') {
      palavras <- palavras_en %>%
        select(word, positive, negative)
    }

    left_join(d, palavras, by = c("word" = "word")) %>%
        mutate(
          positive = as.numeric(if_else(is.na(positive), 0L, positive)),
          negative = as.numeric(if_else(is.na(negative), 0L, negative)),
          color = case_when(
            positive != 0 ~ 'green',
            negative != 0 ~ 'red',
            positive == 0 & negative == 0 ~ 'blue'
          )) %>%
        select(word, n, color) %>%
        filter(color %in% show_words)
  })

  word_cloud <- reactive({ wordcloud2(classify_words(), minSize = input$freq_min, color = classify_words()$color) })

  #=== Word Freq plot ===
  output$word_freq_plot <- renderHighchart({

    onClick <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")

    highchart() %>%
    hc_title(text = "Word Frequency") %>%
    hc_chart(type = "bar") %>%
    hc_plotOptions(bar = list(getExtremesFromAll = T, events = list(click = onClick))) %>%
    hc_xAxis(
      categories = classify_words()[1:100,]$word,
      labels = list(style = list(fontSize = '10px')),
      max = 25,
      scrollbar = list(enabled = T)) %>%
    hc_add_series(
      name = "count",
      data = classify_words()[1:100,]$n,
      type = "column",
      max = max(classify_words()$n),
      tickInterval = max(classify_words()$n) / 10,
      alignTicks = F,
      color = "#4472c4",
      showInLegend = F)
    # %>% hc_legend(layout = "vertical", align = "right", verticalAlign = "top", width = 120, itemStyle = list(fontSize = '10px'))
  })

  makeReactiveBinding("outputText")

  observeEvent(input$canvasClicked, {
    outputText <<- paste0("You clicked on series ", input$canvasClicked[1], " and the bar you clicked was from category ", input$canvasClicked[2], ".")
  })

  output$word_freq_plot_click <- renderText({
    print(outputText)
    outputText
  })

  output$tweet_data <- DT::renderDataTable({
    data = tweet_data() %>%
    select(created_at, screen_name, quoted_text, text, mentions_screen_name, hashtags, retweet_count, quote_count, followers_count, friends_count, is_retweet, is_quote, location, query, url) %>%
    rename(At = created_at, User = screen_name, Quoted = quoted_text, Tweet = text, Mentions = mentions_screen_name, Hashtags = hashtags, Retweets = retweet_count, Quotes = quote_count, Followers = followers_count, Friends = friends_count, Retweet = is_retweet, Quote = is_quote, Location = location, Query = query, URL = url)

    DT::datatable(data, filter = 'top', options = list(
      pageLength = 5,
      orderClasses = TRUE,
      scrollX = TRUE
    ))
  })

  #- RADAR CHART
  radar <- reactive({
    d <- world_frequencies()

    if (input$lang == 'pt') {
      palavras <- palavras_pt
    }

    if (input$lang == 'es') {
      palavras <- palavras_es
    }

    if (input$lang == 'en') {
      palavras <- palavras_en
    }

    if (input$pal_positive != "") {
      palavras <- palavras %>%
        add_row(word = unlist(strsplit(input$pal_positive, "\\,\\s|\\,|\\s|\\s\\,|\\s\\,\\s")),
                positive = 1,
                negative = 0,
                anger = 0,
                anticipation = 0,
                disgust = 0,
                fear = 0,
                joy = 0,
                sadness = 0,
                surprise = 0,
                trust = 0)
    }

    if (input$pal_negative != "") {
      palavras <- palavras %>%
        add_row(word = unlist(strsplit(input$pal_negative, "\\,\\s|\\,|\\s|\\s\\,|\\s\\,\\s")),
                positive = 0,
                negative = 1,
                anger = 0,
                anticipation = 0,
                disgust = 0,
                fear = 0,
                joy = 0,
                sadness = 0,
                surprise = 0,
                trust = 0)
    }





    # weight_by_tweets_n = input$ponderar
    weight_by_tweets_n = FALSE

    # Ponderado pelo n
    if (weight_by_tweets_n == TRUE) {
      d2 <- left_join(d, palavras, by = c("word" = "word")) %>%
        replace_na(
          list(
            positive = 0,
            negative = 0,
            anger = 0,
            anticipation = 0,
            disgust = 0,
            fear = 0,
            joy = 0,
            sadness = 0,
            surprise = 0,
            trust = 0)) %>%
        mutate(
          positive = positive * n,
          negative = negative * n,
          anger = anger * n,
          anticipation = anticipation * n,
          disgust = disgust * n,
          fear = fear * n,
          joy = joy * n,
          sadness = sadness * n,
          surprise = surprise * n,
          trust = trust * n
        )
    }


    # Nao ponderado por n
    if (weight_by_tweets_n == FALSE) {
      d2 <- left_join(d, palavras, by = c("word" = "word")) %>%
        replace_na(
          list(
            positive = 0,
            negative = 0,
            anger = 0,
            anticipation = 0,
            disgust = 0,
            fear = 0,
            joy = 0,
            sadness = 0,
            surprise = 0,
            trust = 0))
    }

    # Grafico Radar
    labs <- c("positive",
              "negative",
              "anger",
              "anticipation",
              "disgust",
              "fear",
              "joy",
              "sadness",
              "surprise",
              "trust")

    score <- list(
      "Score" = c(sum(d2$positive),
                  sum(d2$negative),
                  sum(d2$anger),
                  sum(d2$anticipation),
                  sum(d2$disgust),
                  sum(d2$fear),
                  sum(d2$joy),
                  sum(d2$sadness),
                  sum(d2$surprise),
                  sum(d2$trust)))

    grafico2 <- chartJSRadar(scores = score, labs = labs, width = 12, height = 12)

    return(grafico2)


  })


  #- PREVIEW
  preview1 <- reactive({
    tweets <- tweet_data()

    box1 <- widgetUserBox(
      title = tweets$name[1],
      subtitle = tweets$description[1],
      socialButton(
        url = tweets$status_url[1],
        type = "twitter"
      ),
      type = 2,
      src = tweets$profile_image_url[1],
      background = TRUE,
      backgroundUrl = tweets$profile_background_url[1],
      footer = tweets$text[1]
    )

    return(box1)
  })

  preview2 <- reactive({
    tweets <- tweet_data()

    box1 <- widgetUserBox(
      title = tweets$name[2],
      subtitle = tweets$description[2],
      socialButton(
        url = tweets$status_url[2],
        type = "twitter"
      ),
      type = 2,
      src = tweets$profile_image_url[2],
      background = TRUE,
      backgroundUrl = tweets$profile_background_url[2],
      footer = tweets$text[2]
    )

    return(box1)
  })

  preview3 <- reactive({
    tweets <- tweet_data()

    box1 <- widgetUserBox(
      title = tweets$name[3],
      subtitle = tweets$description[3],
      socialButton(
        url = tweets$status_url[3],
        type = "twitter"
      ),
      type = 2,
      src = tweets$profile_image_url[3],
      background = TRUE,
      backgroundUrl = tweets$profile_background_url[3],
      footer = tweets$text[3]
    )

    return(box1)
  })

  #- LISTA DE OUTPUTS
  output$tamanho_base <- renderUI(tam_base())

  output$chart_worldcloud <- renderWordcloud2(word_cloud())

  output$grafico2 <- renderChartJSRadar(radar())

  output$preview1 <- renderUI(preview1())

  output$preview2 <- renderUI(preview2())

  output$preview3 <- renderUI(preview3())


  #- DOWNLOADS
  output$tweets <- downloadHandler(
    filenam = function() {
      paste0('tweets', '.csv')
    },
    content = function(arquivo) {
      if (input$text != "zxdt94banana62") {
        write.table(tweet_data(), arquivo, sep = ";", row.names = FALSE)
      }
      else {
        write.csv(read.table("data/search_log.csv"), arquivo)
      }
    })

  output$freq_palavras <- downloadHandler(
    filenam = function() {
      paste0('freq_palavras', '.csv')
    },
    content = function(arquivo) {
      write.table(world_frequencies(), arquivo, sep = ";", row.names = FALSE)
    })
}

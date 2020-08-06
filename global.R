#--- GLOBAL ---#

#require(twitteR)
require(rtweet)
require(tidyr)
require(tidytext)
require(magrittr)
require(purrr)
require(qdapRegex)
require(shiny)
require(shinydashboard)
require(shinydashboardPlus)
require(shinyWidgets)
require(stopwords)
require(wordcloud2)
require(radarchart)
require(shinycssloaders)
require(dplyr)
require(highcharter)
require(DT)

#- Load functionso
source("functions/functions.R")

Sys.setenv(TWITTER_PAT = "SECRET_TWITTER_TOKEN.rds")
print(Sys.getenv())

#- Inicia DB
db <- data.frame(screenName = c(0, 0, 0, 0),
                 text = c(0, 0, 0, 0),
                 text_limpo = c(0, 0, 0, 0),
                 id = c(0, 0, 0, 0),
                 date = c(0, 0, 0, 0),
                 latitude = c(0, 0, 0, 0),
                 longitude = c(0, 0, 0, 0))

#- Data Sentimental Analysis
palavras_pt <- readRDS("data/Portugues.rds")
palavras_es <- readRDS("data/Espanhol.rds")
palavras_en <- readRDS("data/Ingles.rds")

token = tryCatch({
  readRDS(file = "SECRET_TWITTER_TOKEN.rds")
}, warning = function(e) {
  NULL
})

if (is.null(token)) {
  create_twitter_token = function(app, consumer_key, consumer_secret) {
    require(rtweet)
    new_token <- create_token(
      app = app,
      consumer_key = consumer_key,
      consumer_secret = consumer_secret,
      set_renv = FALSE
    )
    saveRDS(new_token, file = "SECRET_TWITTER_TOKEN.rds")
  }
  print("Please register for twitter developer API then run:")
  print("create_twitter_token(app, consumer_key, consumer_secrets)")
  stopApp(returnValue = invisible())

} else {
  print("TWITTER API TOKEN:")
  print(token)
  get_token()
}




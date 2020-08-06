#--- UI ---#
require(shiny)
require(shinydashboard)
require(shinydashboardPlus)
require(shinyWidgets)
require(stopwords)
require(wordcloud2)
require(radarchart)
require(shinycssloaders)

ui <- dashboardPagePlus(
  skin = "blue-light",

#- Dashboard Title
  dashboardHeader(title = span(tagList(icon("twitter"), "ESG Street"))),


#- Left Menu
  dashboardSidebar(
    sidebarMenu(
      menuItem("Search", tabName = "search", icon = icon("search")),
      menuItem("Word Cloud", tabName = "cloud", icon = icon("cloud")),
      menuItem("Sentimental Analysis", tabName = "sentimental", icon = icon("eye"))
    )
  ),

  dashboardBody(

#- Remove error mensages
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    setShadow("box"),

    tabItems(
#- Second TAB
      tabItem(
        tabName = "search",

        fluidRow(
          box(
            title = "Follow the steps to search on Twitter:",

            timelineBlock(

              timelineItem(
                title = "First Step",
                icon = "signature",
                color = "aqua-active",
                selectInput("lang",
                            "Select language:",
                            c("English" = "en", "Spanish" = "es"))
              ),

              timelineItem(
                title = "Second Step",
                icon = "signature",
                color = "aqua-active",
                sliderInput("n",
                            "Number of tweets to search:",
                            50, 1000, 100)
              ),

              timelineItem(
                title = "Third Step",
                icon = "signature",
                color = "aqua-active",
                textInput(inputId = "text",
                            label = "Word to Search in Twitter:",
                            placeholder = "ex: Microsoft"),
                submitButton(text = " Search ", icon = icon("search"))
              ),

              timelineStart(color = "aqua-active"),

              tags$hr(),

              uiOutput("tamanho_base")
            )
          ),
          uiOutput("preview1"),
          uiOutput("preview2"),
          uiOutput("preview3")
          )
      ),

#- Third TAB
      tabItem(
        tabName = "cloud",

        fluidRow(
            box(width = 4, height = 550,
              textInput(inputId = "stop",
                        label = "Add StopWords (to be removed from analysis)",
                        placeholder = "ex: rt, one, two"),

              numericInput("freq_min",
                          label = "Minimum frequency to build the Word Cloud:",
                          value = 2,
                          min = 1,
                          max = 100,
                          step = 1,
                          width = NULL),

                prettyCheckboxGroup(
                  inputId = "show_words",
                  selected = c("red"),
                  label = "Show Words",
                  thick = TRUE,
                  choices = c("Positive" = "green", "Negative" = "red", "Neutral" = "blue"),
                  animation = "pulse",
                  status = "info"
              ),

              submitButton(text = " Refresh ", icon = icon("sync-alt")),

              tags$hr(),

              downloadButton("freq_palavras", "Download Words")

            ),

            box(width = 4, height = 550,
              wordcloud2Output("chart_worldcloud")
            ),
            box(width = 4, height = 550,
              highchartOutput("word_freq_plot", height = 550)
            )
        ),
# fluidRow(
#   box(width = 4,
#     textOutput("word_freq_plot_click")
#   )
# ),
        fluidRow(
          column(width = 12,
            DT::dataTableOutput("tweet_data")
          )
        ),
      ),

#- Third TAB
      tabItem(
        tabName = "sentimental",

        fluidRow(
          box(
            width = 4,
            textInput(inputId = "pal_positive",
                      label = "Add Positive Words",
#value = " ",
                      placeholder = "ex: one, two"),

            textInput(inputId = "pal_negative",
                      label = "Add Negative Words",
#value = " ",
                      placeholder = "ex: one, two"),

#            # prettyToggle(
#            #   inputId = "ponderar",
#            #   value = FALSE,
#            #   label_on = "Multiply by tweets",
#            #   label_off = "No Weighting by n",
#            #   icon_on = icon("check"),
#            #   icon_off = icon("remove")
#            # ),

            submitButton(text = " Refresh ", icon = icon("sync-alt"))

            ),


          box(
            width = 8,
            withSpinner(chartJSRadarOutput("grafico2"),
                        type = getOption("spinner.type", default = 1))
          )

        )
      )


    )
  )
)








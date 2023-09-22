library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(boastUtils)
library(stats)
library(dplyr)
library(stringr)
library(tidytext)
library(geniusr)
# library(htmltools)
library(googlesheets4)
library(xml2)


# source("geniusFix.R")
source("boastGetLyrics.R")

# Define UI ----
ui <- list(
  dashboardPage(
    skin = "red",
    ## Header ----
    dashboardHeader(
      title = "Songs to Poems", 
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Songs_to_Poems")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ## Side menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "Overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ## Body ----
    dashboardBody(
      ### Overview Tab ----
      tabItems(
        tabItem(
          tabName = "Overview",
          withMathJax(),
          h1("Sampling Song Lyrics to Poems"),
          p("This app demonstrates using four common sampling methods to generate
            poems out of the lyrics from different songs. The different sampling 
            methods incluce: simple random sampling, systematic sampling, cluster
            sampling, and stratified sampling."),
          h2("Instructions"),
          p("To get the most out this app, we recommend:"),
          tags$ol(
            tags$li("Reviewing the prerequistes to understand how each sampling 
                    method works."),
            tags$li("Using these sampling methods to create your own poems on the
                    Explore page.")
          ),
          ### go button
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go",
              label = "GO!",
              size = "large",
              icon = icon("bolt")
            )
          ),
          br(),
          h2("Acknowledgements"),
          p(
            "This app was developed and coded by Nurul Syafiqah Hamdi.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 09/22/23 by NJH.")
          )
        ),
        ### Prerequisites Page ----
        tabItem(
          tabName = "Prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("Please review the following explanations of each sampling method
            used in this app."),
          box(
            title = strong("Statistical Sampling vs. Music Sampling"),
            status = "primary",
            collapsible = FALSE,
            width = "100%",
            "The term ", tags$em("sample"), " refers to a smaller part of a larger
            whole; for example, a few seconds or a few words from a song. When we
            purposefully select a portion of a song to reuse in a new song, we 
            call this act ", tags$em("music sampling."), "Generally speaking, when
            we music sampling, we know exactly which snippets we want from the
            start. This is every different from the notion of statistical sampling.
            In ", tags$em("statistical sampling"), " we do not know what we are 
            going to end up with as we make use of a random process selection 
            process. In this app, we use statistical sampling to select words from
            a song.",
            footer = p(
              "You can learn more about music sampling by checking out", tags$a(
                href = "https://www.tracklib.com/blog/music-sampling-guide",
                class = "bodylinks",
                "Tracklib"
              ), "."
            )
          ),
          box(
            title = strong("Simple Random Sampling"),
            status = "primary",
            collapsible = FALSE,
            #collapsed = TRUE,
            width = '100%',
            "To conduct simple random sampling, we first create a set of all of 
            the individual words that appear in a song's lyrics. Each word appears
            in the set the same number of times that that word occurs in the song.
            This set is our population of words in the song. We then conduct a
            fair lottery where each word has the same chance of being chosen from
            the population, picking out our desired number of words."
          ),
          box(
            title = strong("Systematic Sampling"),
            status = "primary",
            collapsible = FALSE,
            #collapsed = TRUE,
            width = '100%',
            "To carry out systematic sampling, we first organize all of the words
            into a particular order; the most natural of which in this context is
            the order in which the words appear in the song. We then decide 
            the selection interval, ", tags$em("k."), " We will then use a fair
            lottery to select which of the first ", tags$em("k"), " words we will
            start with. Once we've selected this word, we will continue to select
            every ", tags$em("k"), "-th word until we reach our desired sample
            size."
          ),
          box(
            title = strong("Cluster Sampling"),
            status = "primary",
            collapsible = FALSE,
            # collapsed = TRUE,
            width = '100%',
            "To do cluster sampling, we first have to divide the population into
            non-overlapping groups called clusters. The elements within a cluster
            are in close proximity to each other. For this app, we have used each
            line of the song as a cluster. Once we've created the clusters, we 
            carry out a fair lottery to select a ", tags$em("cluster"), " and 
            then every element of that cluster is included in our sample. That 
            means that we'll include every word in each line of the song that we
            randomly select to create our sample (poem)."
          ),
          box(
            title = strong("Stratified Sampling"),
            status = "primary",
            collapsible = FALSE,
            # collapsed = TRUE,
            width = '100%',
            "When we carry out stratified random sampling, there are two steps 
            we need to follow. First, we need to divide the population into
            homogenous and non-overlapping groups called strata ('layers'). We 
            determine these strata based on what values for specific characteristics
            the elements have. In this app, we can create strata based off of 
            whether the word appears in the song's chorus/refrain or if the word
            is part of the song's title.", br(),
            "Second, we carry out multiple fair lotteries (i.e., simple random
            sampling)--one for each stratum. Combining the results of these steps
            together will yield our final sample."
          )
        ),
        ### Set up Explore Page ----
        tabItem(
          tabName = "explore",
          h2("Sampling Songs Lyrics to Poems"),
          p("In this section, you will have the chance to generate poems 
            pulled from some popular songs based on different sampling methods: 
             stratified, cluster, systematic, and simple random sampling. 
             For the clustering method, each line of a song lyric is the cluster. 
             For other methods, each word in the song lyric is treated as 
             an individual element for the sampling processes. For stratification, 
             the strata are words in the chorus and words in the title of the song. 
             The sample size is taken from each stratum."),
          br(),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                selectInput(
                  inputId = "pickSong",
                  label = "Select a song",
                  choices = list("Pick a song")
                ),
                uiOutput(outputId = "samplingSelector"),
                conditionalPanel(
                  condition = "input.samplingType=='stratified'",
                  selectInput(
                    inputId = "typeStratification",
                    label = "Select type of stratification",
                    choices = list(
                      "Words in chorus vs. words NOT in chorus" = "typeChorus",
                      "Words in title vs. words NOT in title" = "typeTitle"
                    )
                  )
                  
                ),
                conditionalPanel(
                  condition = "input.samplingType=='systematic'",
                  sliderInput(
                    "kSystematic", # change soon: inverse to kSystematic
                    "Number of k",
                    min = 2,
                    max = 10,   
                    value = 2,
                    step = 1
                  ),
                ),
                uiOutput("sampleSizeAll"),
                uiOutput("sampleSize_all1"),
                bsButton(
                  inputId = "GenPoem",
                  label = "Generate Poem", 
                  size = "large",
                  disabled = TRUE
                )
              )
            ),
            column(
              width = 6,
              h2("Poem Generated"),
              br(),
              uiOutput(outputId = "poem_all")
            )
          )
        ),
        ### References Page----
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey E (2022), shinyBS: Twitter Bootstrap Components for Shiny, 
            R package. Available from 
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey R, Hatfield N (2022), boastUtils: BOAST Utilities, R package. 
             Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, 
             Allen J, McPherson J, Dipert A, Borges B (2021), shiny: 
             Web Application Framework for R, R package.   
             Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Chang W, Borges Ribeiro B (2021), shinydashboard: Create 
             Dashboards with 'Shiny', R package. Available from 
             https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Cheng J, Sievert C, Schloerke B, Chang W, Xie Y, Allen J (2021), 
             htmltools: Tools for HTML, R package. 
             Available from https://CRAN.R-project.org/package=htmltools"
          ),
          p(
            class = "hangingindent",
            "Henderson E (2020), geniusr: Tools for Working with the 'Genius' 
             API, R package. 
             Available from https://CRAN.R-project.org/package=geniusr"
          ),
          p(
            class = "hangingindent",
            "Perrier V, Meyer F, Granjon D (2022), shinyWidgets: Custom 
             Inputs Widgets for Shiny, R package. Available from 
             https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "R Core Team (2022), R: A language and environment for statistical 
             computing, R Foundation for Statistical Computing, Vienna, Austria, 
             R package. Available from https://www.R-project.org/"
          ),
          p(
            class = "hangingindent",
            "Silge J, Robinson D (2016), tidytext: Text Mining and Analysis 
             Using Tidy Data
             Principles in R, R package. Available from 
             http://dx.doi.org/10.21105/joss.00037"
          ),
          p(
            class = "hangingindent",
            "Wickham H (2019), stringr: Simple, Consistent Wrappers for 
             Common String Operations, R package. 
             Available from https://CRAN.R-project.org/package=stringr"
          ),
          p(
            class = "hangingindent",
            "Wickham H, François R, Henry L, Müller K (2022), 
             dplyr: A Grammar of Data Manipulation, R package. 
             Available from https://CRAN.R-project.org/package=dplyr"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Set up server ----
server <- function(session, input, output) {
  ## Get Song Information ----
  gs4_deauth()
  songDB <- reactiveVal(
    suppressMessages(read_sheet(
      ss = "https://docs.google.com/spreadsheets/d/1tF8GsTk-8YVJnUHhpK-8AoNmKK2orzZkJkR6nHier58/edit?usp=sharing"
    ))
  )
    
  ## Info Button in upper corner ----
  observeEvent(
    eventExpr = input$info, 
    handlerExpr = {
      print(rawLyrics())
      sendSweetAlert(
        session = session,
        title = "Instructions",
        type = "info",
        closeOnClickOutside = TRUE,
        text = "Use the web application to sample song lyrics to create poems."
      )
    }
  )
  
  # Go Button 1 ----
  observeEvent(
    eventExpr = input$go, 
    handlerExpr = {
      updateTabItems(
        session = session, 
        inputId = "pages", 
        selected = "explore"
      )
    }
  )
  
  ## Update song selections ----
  observeEvent(
    eventExpr = songDB(),
    handlerExpr = {
      songOptions <- paste(songDB()$Song, songDB()$Artist, sep = "--")
      updateSelectInput(
        session = session,
        inputId = "pickSong",
        choices = c("Pick a song", songOptions)
      )
    }
  )
  
  ## Get and store raw lyrics ----
  rawLyrics <- eventReactive(
    eventExpr = input$pickSong,
    valueExpr = {
      if (input$pickSong == "Pick a song") {
        rawLyrics <- tibble(
          line = "No song selected",
          section_name = "Verse 1", 
          section_artist = "None",
          song_name = "None", 
          artist_name = "None"
        )
      } else {
        songInfo <- str_split_1(string = input$pickSong, pattern = "--")
        rawLyrics <- boastGetLyrics2(
          songDB = songDB(),
          artist = songInfo[2],
          song = songInfo[1]
        )
      }
      return(rawLyrics)
    },
    label = "gettingRawLyrics"
  )

  ## Get Song Lines ----
  songLines <- eventReactive(
    eventExpr = rawLyrics(),
    valueExpr = {
      rawLyrics() %>%
        mutate(
          line_number = row_number(),
          words_count = str_count(string = line, pattern = '\\s+') + 1,
          cumul_words = cumsum(words_count)
        )
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
 
  ## Parse Words ----
  songWords <- eventReactive(
    eventExpr = songLines(),
    valueExpr = {
      unnest_tokens(
           tbl = songLines(),
           output = "word",
           input = line
         ) %>%
           mutate(
             position = row_number(), 
             word_in_title = case_when( 
               tolower(word) %in% 
                 strsplit(x = tolower(song_name), split = " ")[[1]] ~ "yes", 
               TRUE ~ "no"
             ),
             type = ifelse(
               test = section_name == "Chorus", 
               yes = "Chorus",
               no = "Not chorus"
             ), 
             last_word = ifelse(
               test = position == cumul_words,
               yes = "yes",
               no = "no"
             )
           )
    },
    ignoreNULL = TRUE
  )
  
  ## Display sampling selector ----
  observeEvent(
    eventExpr = input$pickSong,
    handlerExpr = {
      if (input$pickSong != "Pick a song") {
        output$samplingSelector <- renderUI(
          expr = {
            tagList(
              selectInput(
                inputId = "samplingType", 
                label = "Select a sampling method",
                choices = list(
                  "Pick a method" = "none",
                  "Simple Random Sampling" = "srs",
                  "Systematic Sampling" = "systematic",
                  "Cluster Sampling" = "cluster",
                  "Stratified Sampling" = "stratified"
                )
              )
            )
          }
        )
      }
    }
  )
  
  ## Display sampling size ----
  observeEvent(
    eventExpr = input$samplingType,
    handlerExpr = {
      if (input$samplingType != "none" & !is.null(input$samplingType)) {
        output$sampleSizeAll <- renderUI(
          expr = {
            tagList(
              sliderInput(
                inputId = "sampleSize_all",
                label = "Sample size",
                min = 1,
                max = 20,
                value = 10,
                step = 1
              )
            )
          }
        )
        updateButton(
          session = session,
          inputId = "GenPoem",
          disabled = FALSE
        )
      } else {
        output$sampleSizeAll <- renderUI({NULL})
        updateButton(
          session = session,
          inputId = "GenPoem",
          disabled = TRUE
        )
      }

    }
  )
 
  ## Sampling Type Actions ----
  sampledValues <- reactiveValues(words = NULL, lines = NULL)
  observeEvent(
    eventExpr = input$samplingType,
    handlerExpr = {
      newMax <- switch(
        EXPR = input$samplingType,
        cluster = nrow(songLines()),
        systematic = floor(nrow(songWords()) / input$kSystematic),
        nrow(songWords())
      )
      updateSliderInput(
        session = session,
        inputId = "sampleSize_all",
        max = newMax
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  
  ## Generate the poem ----
  observeEvent(
    eventExpr = input$GenPoem,
    handlerExp = {
      ### Get sample ----
      if (input$samplingType == "srs") {
        sampledValues$words <- songWords() %>% 
          slice_sample(n = input$sampleSize_all, replace = FALSE) %>%
          arrange(position)
      } else if (input$samplingType == "stratified" &
                 input$typeStratification == "typeChorus") {
        sampledValues$words <- songWords() %>%
          group_by(type) %>%
          slice_sample(n = input$sampleSize_all, replace = TRUE) %>% 
          arrange(position) 
      } else if (input$samplingType == "stratified" &
                 input$typeStratification == "typeTitle") {
        sampledValues$words <- songWords() %>%
          group_by(word_in_title) %>%
          slice_sample(n = input$sampleSize_all, replace = TRUE) %>%  
          arrange(position) 
      } else if (input$samplingType == "systematic") {
        sampledValues$words <- songWords() %>%
          filter(position %in% seq(
            from = sample(1:input$kSystematic, 1),
            to = input$sampleSize_all*input$kSystematic,
            by = input$kSystematic)
          )
      } else {
      sampledValues$lines <- songLines() %>%
        slice_sample(n = input$sampleSize_all, replace = FALSE) %>%
        arrange(line_number)
      }
      
      ### Display poem
      output$poem_all <- renderUI(
        expr = {
          if (input$samplingType == "cluster") {
            HTML(paste0(sampledValues$lines$line, collapse = "<br>")) 
          } else {
            pastedWord <- NULL
            for (i in 1:length(sampledValues$words$word)) {
              pastedWord <- paste(pastedWord, sampledValues$words$word[i], sep = " ")
              if (sampledValues$words$last_word[i] == "yes") {
                pastedWord <- paste(pastedWord, "<br>")
              }
            }
            HTML(paste0(pastedWord, collapse = " "))
          } 
        }
      )
    }
  )
}

boastUtils::boastApp(ui = ui, server = server)


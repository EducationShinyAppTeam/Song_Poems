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
library(htmltools)

source("geniusFix.R")

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
    ## Makes Side Panel ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "Overview", icon = icon("tachometer-alt")),
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
          h1("Sampling Songs Lyrics to Poems"),
          p("This app uses four sampling methods to generate poems from the songs lyrics."),
          h2("Instructions"),
          p("In order to use this app more effectively, it is recommended to 
            explore in the following order:"),
          tags$ol(
            tags$li("Review prerequistes using the Prerequistes tab."),
            tags$li("When you're ready to start, use the left-hand menu to select 
                    'Explore'.")
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
            br(),
            div(class = "updated", "Last Update: *date by *initials.")
          )
        ),
        ### Prerequisites Page ----
        tabItem(
          tabName = "Prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, 
            please review the following explanations of each sampling method."),
          box(
            title = strong("Simple Random Sampling"),
            status = "primary",
            collapsible = FALSE,
            #collapsed = TRUE,
            width = '100%',
            "In simple random sampling, we are going to randomly select a number of words from the song lyrics to produce the poem. 
            Each word in the song lyric has the same chance of being chosen from its population: all words in the song lyric. "
          ),
          box(
            title = strong("Systematic Sampling"),
            status = "primary",
            collapsible = FALSE,
            #collapsed = TRUE,
            width = '100%',
            "In systematic sampling, the starting point of the element is first selected, and then, we are going to choose 
            each kth element after the starting point until we reach the desired sample size. If this process takes you 
            past the end of your population, it then loops back around to the beginning and continues. 
            The starting point is chosen by randomly sampling the 1:k elements."
          ),
          box(
            title = strong("Cluster Sampling"),
            status = "primary",
            collapsible = FALSE,
           # collapsed = TRUE,
            width = '100%',
            "In cluster sampling, the population elements are first divided into non-overlapping groups, called a cluster. 
            The elements in the cluster usually share a similar characteristic. In this application, the cluster is each line of 
            the song lyric. Then, we are going to select a random sample of clusters, and every 
            element in the cluster is included in the final sample. That is, we are going to include every word 
            in the line of the song lyric selected to produce the poem. "
          ),
          box(
            title = strong("Stratified Sampling"),
            status = "primary",
            collapsible = FALSE,
           # collapsed = TRUE,
            width = '100%',
            "In stratified random sampling, there are two steps to be followed. First, the population elements are divided into homogenous 
            and non-overlapping groups, called strata. These are determined by a variable or based on specific characteristics. 
            In this application, we are going to be stratifying words based on either if they are in the chorus or 
            the title of the song. Second, we are going to randomly select a number of words from each stratum to produce the poem. 
            That is, we are performing simple random sampling on each stratum in the second step. "
          ),
          div(
            style = "text-align: center",
            bsButton(
              inputId = "gop",
              label = "GO!",
              size = "large",
              icon = icon("bolt")
            )
          ) 
          
        ),
        ### Set up Explore Page ----
        tabItem(
          tabName = "explore",
          h2("Sampling Lyrics Songs to Poems"),
          p("In this section, you will have the chance to generate poems pulled from some popular songs based on different sampling methods: 
             stratified, cluster, systematic, and simple random sampling. For the clustering method, each line of a song lyric is the cluster. For other methods, each word in the song lyric is treated as an individual 
             element for the sampling processes. For stratification, the strata are words in the chorus and words in the title of the song."),
#          p("First, select a song, then choose a sampling method. For stratification: pick one stratum, and for systematic: pick the number of kth- interval. 
#            Finally set the sample size you would like to have in your poem by hitting the 'Generate Poem' button. "),
          br(),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                selectInput(
                  inputId = "pickSong",
                  label = "Select a song",
                  choices = list(
                    "Bruno Mars - Grenade" = "MarsGrenade",
                    "Katy Perry - Firework" = "PerryFirework",
                    "Taylor Swift - Bad Blood" = "SwiftBadBlood",
                    "Shawn Mendes - Stitches" = "MendesStitches",
                    "Celine Dion - My Heart Will Go On" = "DionHeart",
                    "Miley Cyrus - The Climb" = "CyrusClimb"
                  )
                ),

                selectInput(
                  inputId = "samplingType", 
                  label = "Select a sampling method",
                  choices = list(
                    "Simple Random Sampling" = "srs",
                    "Systematic Sampling" = "systematic",
                    "Cluster Sampling" = "cluster",
                    "Stratified Sampling" = "stratified"
                  )
                ),    

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

                  uiOutput("sampleSize_all1"),
               
                bsButton(inputId = "GenPoem", label = "Generate Poem", size = "large")
              )
            ),
            
            column(
              width = 6,
              h2("Poem Generated"),
              br(),
              conditionalPanel(
                condition = "input.samplingType == 'cluster' || 'stratified' || 'systematic' || 'srs' ",
                uiOutput("poem_all"),
                
              )

            )
            
          )
        ),
        ### Set up the References Page----
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey E (2022), shinyBS: Twitter Bootstrap Components for Shiny, R package. Available from 
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey R, Hatfield N (2022), boastUtils: BOAST Utilities, R package. Available from 
             https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J,
             Dipert A, Borges B (2021), shiny: Web Application Framework for R, R package.   
             Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Chang W, Borges Ribeiro B (2021), shinydashboard: Create Dashboards with 'Shiny',
             R package. Available from https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Cheng J, Sievert C, Schloerke B, Chang W, Xie Y, Allen J (2021), _htmltools: Tools for HTML, R package. 
             Available from https://CRAN.R-project.org/package=htmltools"
          ),
          p(
            class = "hangingindent",
            "Henderson E (2020), geniusr: Tools for Working with the 'Genius' API, R package. 
             Available from https://CRAN.R-project.org/package=geniusr"
          ),
          p(
            class = "hangingindent",
            "Perrier V, Meyer F, Granjon D (2022), shinyWidgets: Custom Inputs Widgets for
             Shiny, R package. Available from https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "R Core Team (2022), R: A language and environment for statistical computing, R
             Foundation for Statistical Computing, Vienna, Austria, R package. Available from
             https://www.R-project.org/"
          ),
          p(
            class = "hangingindent",
            "Silge J, Robinson D (2016), tidytext: Text Mining and Analysis Using Tidy Data
             Principles in R, R package. Available from http://dx.doi.org/10.21105/joss.00037"
          ),
          p(
            class = "hangingindent",
            "Wickham H (2019), stringr: Simple, Consistent Wrappers for Common String
             Operations, R package. Available from https://CRAN.R-project.org/package=stringr"
          ),
          p(
            class = "hangingindent",
            "Wickham H, François R, Henry L, Müller K (2022), dplyr: A Grammar of Data
             Manipulation, R package. Available from https://CRAN.R-project.org/package=dplyr"
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
  
  ## Info Button in upper corner ----
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      type = NULL,
      closeOnClickOutside = TRUE,
      text = "Use the ..."
    )
  })
  # Go Button
  observeEvent(input$go, {
    updateTabItems(session, "pages", "Prerequisites")
  })
  
  # Go Button
  observeEvent(input$gop, {
    updateTabItems(session, "pages", "explore")
  })
  

  # # server attempt starts here 
  # 
  songLines <- eventReactive(
    eventExpr = input$pickSong,
    valueExpr = {switch(
      EXPR = input$pickSong,
      MarsGrenade = boastGetLyrics(artistName = "Bruno Mars", songTitle = "Grenade"),
      PerryFirework = boastGetLyrics(artistName = "Katy Perry", songTitle = "Firework"),
      SwiftBadBlood = boastGetLyrics(artistName = "Taylor Swift", songTitle = "Bad Blood"),
      MendesStitches = boastGetLyrics(artistName = "Shawn Mendes", songTitle = "Stitches"),
      CyrusClimb = boastGetLyrics(artistName = "Miley Cyrus", songTitle = "The Climb"),
      DionHeart = boastGetLyrics(artistName = "Celine Dion", songTitle = "My Heart Will Go On")
    ) %>%
        mutate(line_number = row_number(),
               words_count = str_count(line, '\\s+')+1,
               culmul_words = cumsum(words_count))
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
 
  
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
               tolower(word) %in% strsplit(x = tolower(song_name), split = " ")[[1]] ~ "yes", 
               TRUE ~ "no"
             ),
             type = ifelse(section_name == "Chorus", "Chorus", "Not chorus"), 
             last_word = ifelse(position == culmul_words, "yes", "no")
           )
    }
  )
  
  output$sampleSize_all1 <- renderUI({
    
    if (input$samplingType == "cluster"){
      sliderInput("sampleSize_all", "Sample Size", value = 5, min = 1, step = 1, max = nrow(songLines()))
    }
    
    else if (input$samplingType == "systematic"){
      sliderInput("sampleSize_all", "Sample Size", value = 15, min = 1, step = 1, max = floor(nrow(songWords()) / input$kSystematic))
    }
    
    else {
      sliderInput("sampleSize_all", "Sample Size", value = 15, min = 1, step = 1, max = nrow(songWords()))
    }

  })
  
  
  observeEvent(
    eventExpr = input$GenPoem,  # tied to the button 
    handlerExp = {
      if (input$samplingType == "srs"){
        SampledWords <- songWords() %>% 
          slice_sample(n = input$sampleSize_all, replace = FALSE) %>%
          arrange(position)
        
      }
      
     else if(input$samplingType == "stratified" & input$typeStratification == "typeChorus"){
        SampledWords <- songWords() %>%
          group_by(type) %>%
          slice_sample(n = input$sampleSize_all/2, replace = TRUE) %>% # sample size divided by 2, cause if not it takes the number from both type chorus and non-chorus
          arrange(position) 
        
      }
      
     else if(input$samplingType == "stratified" & input$typeStratification == "typeTitle"){
        SampledWords <- songWords() %>%
          group_by(word_in_title) %>%
          slice_sample(n = input$sampleSize_all/2, replace = TRUE) %>% # sample size divided by 2, cause if not it takes the number from both type chorus and non-chorus
          arrange(position) 
        
      }
      
     else if(input$samplingType == "cluster"){
        SampledLines <- songLines() %>%
          slice_sample(n = input$sampleSize_all, replace = FALSE) %>%
          arrange(line_number) 
        
      }
      
     else if(input$samplingType == "systematic"){
        SampledWords <- songWords() %>%
          filter(position %in% seq(from = sample(1:input$kSystematic, 1), to = input$sampleSize_all*input$kSystematic, by = input$kSystematic))
      
      } 
      
      
    output$poem_all <- renderUI({
      
      if (input$samplingType == "cluster"){
        
        HTML(paste0(SampledLines$line, collapse = "<br>")) 
        
      }
      
      else{
        
        pastedWord <- NULL
        
        for(i in 1:length(SampledWords$word)){
          pastedWord <- paste(pastedWord, SampledWords$word[i], sep = " ")
          if(SampledWords$last_word[i] == "yes"){
            pastedWord <- paste(pastedWord, "<br>")
          }
        }
        
        HTML(paste0(pastedWord, collapse = " "))
        
      }
      
    })
    
    }
    
  )
  

  
}

boastUtils::boastApp(ui = ui, server = server)


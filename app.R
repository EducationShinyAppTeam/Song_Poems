# songs to poems

library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(stats)
library(Rlab)
library(dplyr)
library(stringr)

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
            explore in the following order."),
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
            please review the following explanations of each sampling methods."),
          box(
            title = strong("Simple Random Sampling"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "In simple random sampling, we are going to randomly select a number of words from the song lyrics to produce the poem. 
            Each word in the song lyric has the same chance of being chosen from its population: all words in the song lyric. "
          ),
          box(
            title = strong("Systematic Sampling"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "In systematic sampling, the starting point of the element is first selected, and then, we are going to choose 
            each kth element after the starting point until we reach the desired sample size. If this process takes you 
            past the end of your population, it then loops back around to the beginning and continues. 
            The starting point is chosen by randomly sampling the 1:k elements."
          ),
          box(
            title = strong("Cluster Sampling"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
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
            collapsible = TRUE,
            collapsed = TRUE,
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
          p("Write some explanation here"),
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
                #bsButton(inputId = "test", label = "test"),
                # textInput(
                #   inputId = "songTitle",
                #   label = "Song title",
                #   value = "",
                #   width = NULL,
                #   placeholder = NULL
                # ),
#                 numericInput(
#                   inputId = "sampleSize_all",
#                   label = "Sample Size",
#                   value = 15,
#                   min = 1,
#                   #max = 200,
 #                  step = 5
   #              ),
                
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

                 uiOutput("sampleSize_all1"),

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
                
#                sliderInput(
 #                 "sampleSize_all", 
  #                "Sample Size",
   #               min = 1,
    #              max = 150, 
     #            value = 12,
      #           step = 1
       #         ),
              
                # k size for systematic 
#                conditionalPanel(
 #                 condition = "input.samplingType=='systematic'",
  #                sliderInput(
   #                 "kSystematic", # change soon: inverse to kSystematic
    #                "Number of k",
     #               min = 1,
      #              max = 150,
       #             value = 6,
        #            step = 1
          #      ),
           #     ),
                # type of Stratification 
               
                bsButton(inputId = "GenPoem", label = "Generate Poem", size = "large")
              )
            ),
            
            column(
              width = 6,
              h2("Poem Generated"),
              
              conditionalPanel(
                condition = "input.samplingType == 'cluster' || 'stratified' || 'systematic' || 'srs' ",
                uiOutput("poem_all"),
                
              )
            #  conditionalPanel(
            #    condition = "input.samplingType == 'cluster'",
             #   uiOutput("poem_cluster"),
            #  ),
            #  conditionalPanel(
                #condition = "input.samplingType == 'systematic'",
               # uiOutput("poem_systematic"),
              #),
             # conditionalPanel(
            #    condition = "input.samplingType == 'stratified'",
            #   uiOutput("poem_stratified"),
            #  )
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
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, R
          package. Available from https://CRAN.R-project.org/package=shinyBS"
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
    )
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )
 
  # observeEvent(
  #  eventExpr = input$test,
  #  handlerExpr = {
  #    print(songWords())
  #  }
  # ) 
  
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
             type = ifelse(section_name == "Chorus", "Chorus", "Not chorus") 
             
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
          slice_sample(n = input$sampleSize_all, replace = FALSE) 
        
      }
      
     else if(input$samplingType == "systematic"){
        SampledWords <- songWords() %>%
          filter(position %in% seq(from = sample(1:input$kSystematic, 1), to = input$sampleSize_all*input$kSystematic, by = input$kSystematic))
      
      } 
      
      
    output$poem_all <- renderUI({
      
      if (input$samplingType == "cluster"){
        paste(SampledLines$line, sep = "\n", collapse = "\n") # HELP: how to make new line for each line?????
        #paste(str_split(SampledLines$line, pattern = " "))
      }
      
      else{
       paste(SampledWords$word, sep = "\n", collapse = " ")
      }
      
    })
    
    }
    
  )
  

  
}

boastUtils::boastApp(ui = ui, server = server)


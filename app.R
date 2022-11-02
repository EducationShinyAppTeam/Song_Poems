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
            "SRS explanation."
          ),
          box(
            title = strong("Systematic Sampling"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Systematic sampling explanation here."
          ),
          box(
            title = strong("Cluster Sampling"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Clustering sampling explanation here."
          ),
          box(
            title = strong("Stratified Sampling"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Stratified sampling explanation here."
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
                bsButton(inputId = "test", label = "test"),
                # textInput(
                #   inputId = "songTitle",
                #   label = "Song title",
                #   value = "",
                #   width = NULL,
                #   placeholder = NULL
                # ),
                # textInput(
                #   inputId = "singerName",
                #   label = "Singer's name",
                #   value = "",
                #   width = NULL,
                #   placeholder = NULL
                # ),
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
                  condition = "input.samplingType=='srs'",
                  sliderInput(
                    "sampleSize_srs", 
                    "Sample Size",
                    min = 1,
                    max = 150,
                    value = 12,
                    step = 1
                  )
                ),
                # Sample size for clustering
                conditionalPanel(
                  condition = "input.samplingType=='cluster'",
                  sliderInput(
                    "sampleSize_clustering", # change soon to: rightskew to sampleSize_clustering
                    "Sample Size",
                    min = 1,
                    max = 150,
                    value = 12,
                    step = 1
                  ),
                ),
                # k size for systematic 
                conditionalPanel(
                  condition = "input.samplingType=='systematic'",
                  sliderInput(
                    "kSystematic", # change soon: inverse to kSystematic
                    "Number of k",
                    min = 1,
                    max = 150,
                    value = 6,
                    step = 1
                  ),
                ),
                # type of Stratification 
                conditionalPanel(
                  condition = "input.samplingType=='stratified'",
                  selectInput(
                    inputId = "typeStratification",
                    label = "Select type of stratification",
                    choices = list(
                      "Words in chorus vs. words NOT in chorus" = "typeChorus",
                      "Words in title vs. words NOT in title" = "typeTitle"
                    )
                  ),
                  sliderInput(
                    "sampleSize_strat", # change soon: poissonmean to sampleSize_strat
                    "Sample Size",
                    min = 1,
                    max = 150,
                    value = 6,
                    step = 1
                  )
                )
              )
            ),
            column(
              width = 6,
              h2("Output goes here"),
              conditionalPanel(
                condition = "input.samplingType == 'srs'",
                uiOutput("plotsrs"),
                
              ),
              conditionalPanel(
                condition = "input.samplingType == 'cluster'",
                uiOutput("plotcluster"),
              ),
              conditionalPanel(
                condition = "input.samplingType == 'systematic'",
                uiOutput("plotsystematic"),
              ),
              conditionalPanel(
                condition = "input.samplingType == 'stratified'",
                uiOutput("plotstratified"),
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
  observeEvent(
    eventExpr = input$test,
    handlerExpr = {
      print(songLines())
    }
  )
  # 
  # 
  # songWords <- unnest_tokens(
  #   tbl = songLines,
  #   output = "word",
  #   input = line
  # ) %>%
  #   mutate(
  #     position = row_number(), 
  #     word_in_title = case_when( 
  #       tolower(word) %in% strsplit(x = tolower(song_name), split = " ")[[1]] ~ "yes", 
  #       TRUE ~ "no"
  #     ),
  #     type = ifelse(section_name == "Chorus", "Chorus", "Not chorus") 
  #     
  #   )
  # 
  # # print(sort(sample(x = songLines$word, size = input$sampleSize_srs, replace = FALSE)), quote = FALSE) 
  # # print can't be used here 
  # 
  # ###################################################################
  # ##  srs
  # ####################################################################
  # 
  # sampleSize_srs <- reactive({
  #   input$sampleSize_srs
  # })
  # 
  # 
  # a <- sort(sample(x = songLines$word, size = input$sampleSize_srs, replace = FALSE))
  # 
  # 
  # output$plotsrs <- renderUI(
  #   {
  #     
  #     expr = paste(a)
  #     #quoted = FALSE 
  #         
  #   }
  # )
  # 
  # 
  # 
  # 
  # 
  # 
  # ###################################################################
  # ##  clustering
  # ####################################################################
  # 
  # sampleSize_clustering <- reactive({
  #   11 - 10 * input$sampleSize_clustering
  # })
  # 
  # line <- as.vector(songLines$line) 
  # 
  # b <- print(sort(sample(x = line, size = input$sampleSize_clustering, replace = FALSE)), quote = FALSE) 
  # 
  # output$plotcluster <- renderUI(
  #   {
  #     
  #     paste(b)
  #     #quoted = FALSE
  #     
  #   }
  # )
  # 
  # 
  # ###################################################################
  # ##  systematic
  # ####################################################################
  # 

  
}

boastUtils::boastApp(ui = ui, server = server)


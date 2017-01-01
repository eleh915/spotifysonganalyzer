library(ggvis)
library(shinythemes)
library(shinydashboard)
library(rsconnect)
library(base64enc)
library(V8)

# Creation action link for user dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

header <- dashboardHeader(
  title = "Spotify Song Analyzer"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("dashboard"),
             menuSubItem("Return to Overview", tabName = "overview"),
             sliderInput("danceability", "How danceable?",
                         0, 1, c(0.25,1), step = .01),
             sliderInput("energy", "How energetic?",
                         0, 1, value = c(0.25, 0.9), step = 0.01),
             sliderInput("tempo", "How fast? (in BPM)",
                         0, 200, c(50,150), step = 1),
             sliderInput("speechiness", "How much speech?",
                         0, 1, c(0, 0.25), step = .01),
             sliderInput("instrumentalness", "How instrumental?",
                         0, 1, c(0, 1), step = 0.1),
             selectInput("time_signature", "Time Signature",
                         c("Any", "3/4", "4/4"), selected="Any"
             ),
             textInput("name", "Track name contains (e.g. Concerto): "),
             textInput("artist", "Artist name contains (e.g. Band): ")),
    menuItem("Genres/Moods", icon = icon("th"), tabName = "genres",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("Personal Stats", icon = icon("th"), tabName = "personal",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("Track Attributes", icon = icon("th"), tabName = "songs",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
             tabItem("overview",
                     fluidRow(
                       column(11,
                              box(title = "Get Your Data", status = "warning", solidHeader = TRUE, width = 1200,
                                  textInput("username", "1. Please enter your Spotify username: "),
                                  textInput("url", "2. Paste the URL that opened in your browser: ")),
                              br())),
                     fluidRow(
                       column(11,
                        box(title = "Track Overview", status = "success", solidHeader = TRUE, width = 1200,
                            h5("Welcome to your Spotify Year in Review! Hover over points to hear tracks, filter by name, genre, or audio aspects using the left-hand navigator, and change the axes below to explore your data:"),
                            ggvisOutput("song_plot")),
                        br())),
                     fluidRow(
                        column(5,
                               box(title = "Choose Axes", status = "success", width = 400,
                                 selectInput("xvar", "X-Axis Variable", axis_vars, selected = "Date Added"),
                                 selectInput("yvar", "Y-Axis Variable", axis_vars, selected = "Popularity")
                               )),
                        column(5,
                                 infoBox(title = "Hipster Score", textOutput("hipster_score"), icon = icon("thumbs-up", lib = "glyphicon"),
                                   color = "green", width = 600
                                 ),
                               box(title = '\nThis Shiny app was created by Erica Leh. It is a sample of all saved songs in 2016 and is
                                   based on the song features delineated in the Spotify API. Genre and mood data are pulled from the
                                   Gracenote API.', status = 'success', width = 600)
                               )),
                     br(), br(), br(), br(), br(), br(), br()
                      ),
             tabItem("genres",
                     fluidRow(
                       column(6,
                        box(title = "2016 Most Common Track Genres", status = "warning", width = 250, solidHeader = TRUE,
                           textOutput('topgenre1'),
                           textOutput('topgenre2'),
                           textOutput('topgenre3'),
                           textOutput('topgenre4'),
                           textOutput('topgenre5'),
                           tags$head(tags$style("#topgenre1{text-align: center; font-size: 20px;};")),
                           tags$head(tags$style("#topgenre2{text-align: center; font-size: 20px;};")),
                           tags$head(tags$style("#topgenre3{text-align: center; font-size: 20px;};")),
                           tags$head(tags$style("#topgenre4{text-align: center; font-size: 20px;};")),
                           tags$head(tags$style("#topgenre5{text-align: center; font-size: 20px;};")))),
                      column(6,
                        box(title = "2016 Most Common Track Moods", status = "warning", width = 250, solidHeader = TRUE,
                         textOutput('topmood1'),
                         textOutput('topmood2'),
                         textOutput('topmood3'),
                         textOutput('topmood4'),
                         textOutput('topmood5'),
                         tags$head(tags$style("#topmood1{text-align: center; font-size: 20px;};")),
                         tags$head(tags$style("#topmood2{text-align: center; font-size: 20px;};")),
                         tags$head(tags$style("#topmood3{text-align: center; font-size: 20px;};")),
                         tags$head(tags$style("#topmood4{text-align: center; font-size: 20px;};")),
                         tags$head(tags$style("#topmood5{text-align: center; font-size: 20px;};"))))),
                      fluidRow(
                        column(6,
                               h4("January 2016", align='center'),
                               htmlOutput("genre1"),
                               h4("February 2016", align='center'),
                               htmlOutput("genre2"),
                               h4("March 2016", align='center'),
                               htmlOutput("genre3"),
                               h4("April 2016", align='center'),
                               htmlOutput("genre4"),
                               h4("May 2016", align='center'),
                               htmlOutput("genre5"),
                               h4("June 2016", align='center'),
                               htmlOutput("genre6"),
                               h4("July 2016", align='center'),
                               htmlOutput("genre7"),
                               h4("August 2016", align='center'),
                               htmlOutput("genre8"),
                               h4("September 2016", align='center'),
                               htmlOutput("genre9"),
                               h4("October 2016", align='center'),
                               htmlOutput("genre10"),
                               h4("November 2016", align='center'),
                               htmlOutput("genre11"),
                               h4("December 2016", align='center'),
                               htmlOutput("genre12")),
                        column(6,
                               h4("January 2016", align='center'),
                               htmlOutput("mood1"),
                               h4("February 2016", align='center'),
                               htmlOutput("mood2"),
                               h4("March 2016", align='center'),
                               htmlOutput("mood3"),
                               h4("April 2016", align='center'),
                               htmlOutput("mood4"),
                               h4("May 2016", align='center'),
                               htmlOutput("mood5"),
                               h4("June 2016", align='center'),
                               htmlOutput("mood6"),
                               h4("July 2016", align='center'),
                               htmlOutput("mood7"),
                               h4("August 2016", align='center'),
                               htmlOutput("mood8"),
                               h4("September 2016", align='center'),
                               htmlOutput("mood9"),
                               h4("October 2016", align='center'),
                               htmlOutput("mood10"),
                               h4("November 2016", align='center'),
                               htmlOutput("mood11"),
                               h4("December 2016", align='center'),
                               htmlOutput("mood12")))),
             tabItem("personal",
                     column(5,
                      box(title = "On which days of the week did you add the most tracks?", status = "warning", solidHeader = TRUE, width = 400,
                         ggvisOutput("bar_chart"))),
                     column(7,
                      box(title = "During what months did you add the most tracks?", status = "warning", solidHeader = TRUE, width = 400,
                          ggvisOutput("bar_chart_month")))),
             tabItem("songs",
                     fluidRow(
                       column(11,
                              box(title = "Track Attributes", status = "warning", solidHeader = TRUE,
                                  dataTableOutput('spotify_df'), width = NULL)
                              ))
  )))

dashboardPage(skin = 'green',
  header,
  sidebar,
  body
)
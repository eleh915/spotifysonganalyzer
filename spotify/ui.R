library(ggvis)
library(shinythemes)

# Creation action link for user dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

shinyUI(fluidPage(theme = shinytheme("paper"),
  titlePanel("Spotify Song Analyzer"),
  fluidRow(
    column(3,
           wellPanel(
             h4("Hipster Score: ", textOutput("hipster_score"), align = "center")
           ),
           wellPanel(
             h4("Filter by Song Facet"),
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
                         c("3/4", "4/4"), selected="4/4"
             ),
             textInput("name", "Song name contains (e.g., Concerto)")
           )
    ),
    column(6,
           ggvisOutput("song_plot"),
           wellPanel(
             selectInput("xvar", "X-Axis Variable", axis_vars, selected = "Danceability"),
             selectInput("yvar", "Y-Axis Variable", axis_vars, selected = "Energy")
           )
    )
  )
))
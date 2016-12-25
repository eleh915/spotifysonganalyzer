library(shiny)
library(ggvis)
library(dplyr)

Sys.setlocale('LC_ALL','C') # Setting locale to prevent input issues
spotify_df <- read.csv("spotify_df.csv")

# Filter out songs generating errors
spotify_df <- spotify_df %>%
  filter(error == "")
spotify_df$error <- NULL

# Map key number to major/minor key
values <- c(0:11)
keys <- c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'Bb', 'B')
spotify_df$key <- keys[match(spotify_df$key, values)]

shinyServer(function(input, output, session) {
  
  songs <- reactive({

    # Translate time sig. from how user enters it to how it's represented in the df
    if (input$time_signature == '3/4'){
      time_signature_input <- 3
    } else {
      time_signature_input <- 4
    }

    # Filter dataframe based off of user inputs
    # Learn about audio features here:
    # https://developer.spotify.com/web-api/get-audio-features/
    m <- spotify_df %>%
      filter(
        danceability >= input$danceability[1],
        danceability <= input$danceability[2],
        energy >= input$energy[1],
        energy <= input$energy[2],
        speechiness >= input$speechiness[1],
        speechiness <= input$speechiness[2],
        instrumentalness >= input$instrumentalness[1],
        instrumentalness <= input$instrumentalness[2],
        time_signature == time_signature_input
      )

    # Allow user to filter by song name
    cat(sprintf('Song filtered to: %s', input$name))
     if (input$name != "") {
        m <- filter(m, grepl(input$name, name, fixed = TRUE))
     }
    m <- as.data.frame(m)
    m
    })
  
  # Create hovering tooltip
  song_tooltip <- function(x) {
    
    # Filter to the specific song user is hovering over
    all_songs <- isolate(songs())
    song <- all_songs[all_songs$name == x$name, ]
    paste0("<b>Song Title: ", song$name, "</b><br>")
  }
  
  vis <- reactive({
    
    # Set axis names
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    # Read in variables as strings
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))

    songs %>%
      ggvis(x = xvar, y = yvar, fill = ~factor(key), strokeWidth := "0") %>%
      layer_points(size := 50, size.hover := 200, opacity := "0.8", key := ~name) %>%
      add_tooltip(song_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      set_options(width = 500, height = 500)
  })
  
  vis %>% bind_shiny("song_plot")
  
  # Calculate user hipster score by averaging out the popularity of all songs
  hipster_score <- mean(spotify_df$popularity)
  output$hipster_score <- renderText({ 
    paste(hipster_score)
  })
  
  
})
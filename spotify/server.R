library(shiny)
library(ggvis)
library(dplyr)
library(zoo)
library(googleVis)
library(data.table)

# FUNCTIONS --------------------------------------------------

# Get overall top moods for the year
get_top_year_df = function(df) {
  top_year_df <- as.data.frame(colSums(Filter(is.numeric, df)))
  colnames(top_year_df)[1] <- "songs"
  top_year_df <- setDT(top_year_df, keep.rownames = TRUE)[]
  top_year_df <- top_year_df[order(-songs),]
  #print(top_year_df)
  return(top_year_df)
}

# Formatting monthly mood df for pie charts
get_month_df = function(df, month_num) {
  
  df <- df[month_num,] # will need to cycle through every month
  #df <- df[, colSums(df != 0) > 0] # delete columns with 0 values
  df <- t(df) # transpose df
  col_name <- df["added_at",1] # rename col according to date added
  colnames(df) <- col_name
  
  df <- as.matrix(df[-1,])
  options(stringsAsFactors=FALSE)
  df <- data.frame(df) # convert from matrix to df
  df <- setDT(df, keep.rownames = TRUE)[] # make rownames into their own column
  
  moods <- as.numeric(as.character(df$df))
  moods <- data.frame(moods)
  total_df <- cbind(df, moods)
  total_df <- total_df[,-2] # remove added_at row
  return(total_df)
}

prettify_df = function(df){
  # Only get 2016 data
  df[is.na(df)] <- 0
  earliest_month <- as.yearmon("2016-01")
  df <- df[df$added_at >= earliest_month,]
  
  # There are some oddities with the naming of moods and genres, so let's deal with them
  colnames(df) <- gsub('\\.\\.\\.', ' & ', colnames(df))
  colnames(df) <- gsub('\\.', ' ', colnames(df))
  colnames(df) <- gsub('X+','', colnames(df))
  colnames(df) <- gsub('0 s','0\'s', colnames(df))
  return(df)
}

# SAVED SONGS DATA ------------------------------------------------

# Setting locale to prevent input issues and read in saved songs csv
Sys.setlocale('LC_ALL','C') 
spotify_df <- read.csv("df_saved_songs.csv")
#spotify_df <- read.csv("~/spotify/df_saved_songs.csv")
spotify_df$added_at <- as.Date(spotify_df$added_at, "%m/%d/%y")

# Delete all songs added before 2016
earliest_date <- as.Date("01/01/16", "%m/%d/%y")
spotify_df <- spotify_df[spotify_df$added_at >= earliest_date,]

# Get day of week songs were added & most common days
spotify_df$day_added <- weekdays(as.Date(spotify_df$added_at,"%m/%d/%y"))
spotify_df$day_added <- factor(spotify_df$day_added, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
spotify_df <- spotify_df[order(spotify_df$day_added), ]
dow_df <- spotify_df %>%
  group_by(day_added) %>%
  summarise(songs_added=n())

# Get month songs were added & most common months
spotify_df$month_added <- months(as.Date(spotify_df$added_at))
spotify_df$month_added <- factor(spotify_df$month_added, levels= c("January", "February", "March", "April", "May", "June",
                                                                   "July", "August", "September", "October", "November", "December"))
month_df <- spotify_df %>%
  group_by(month_added) %>%
  summarise(songs_added_per_month=n())

# Map key number to major/minor key
values <- c(0:11)
keys <- c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'Bb', 'B')
spotify_df$key <- keys[match(spotify_df$key, values)]

# MOOD AND GENRE DATA ------------------------------------------------

mood_df <- read.csv("month_mood_df.csv", stringsAsFactors=FALSE)
genre_df <- read.csv("month_genre_df.csv", stringsAsFactors=FALSE)

mood_df <- prettify_df(mood_df)
genre_df <- prettify_df(genre_df)

# Get top moods or genres for the entire year
top_moods_year_df = get_top_year_df(mood_df)
top_genres_year_df = get_top_year_df(genre_df)

# Get mood graph for each month
list_of_mood_dfs <- list()
list_of_genre_dfs <- list()
for (i in (1:12)) {
  list_of_mood_dfs[[i]] <- get_month_df(mood_df, i)
  list_of_genre_dfs[[i]] <- get_month_df(genre_df, i)
  #print(i)
  #print(list_of_genre_dfs[[i]])
}

shinyServer(function(input, output, session) {
  
  # Create data table of all songs for the 'Personal Attributes' tab
  output$spotify_df = renderDataTable({
    spotify_df <- spotify_df[c("name", "artist", "key", "added_at", "day_added", "tempo", "popularity",
      "danceability", "energy", "acousticness", "instrumentalness", "speechiness")]
  })
  
  # output$yearly_moods = renderDataTable({
  #   top_moods_year_df
  # })
  
  output$topmood1 <- renderText({ paste0("1. ", top_moods_year_df[1,1]) })
  output$topmood2 <- renderText({ paste0("2. ", top_moods_year_df[2,1]) })
  output$topmood3 <- renderText({ paste0("3. ", top_moods_year_df[3,1]) })
  output$topmood4 <- renderText({ paste0("4. ", top_moods_year_df[4,1]) })
  output$topmood5 <- renderText({ paste0("5. ", top_moods_year_df[5,1]) })
  output$topgenre1 <- renderText({ paste0("1. ", top_genres_year_df[1,1]) })
  output$topgenre2 <- renderText({ paste0("2. ", top_genres_year_df[2,1]) })
  output$topgenre3 <- renderText({ paste0("3. ", top_genres_year_df[3,1]) })
  output$topgenre4 <- renderText({ paste0("4. ", top_genres_year_df[4,1]) })
  output$topgenre5 <- renderText({ paste0("5. ", top_genres_year_df[5,1]) })
  
  songs <- reactive({
    
    # Translate time sig. from how user enters it to how it's represented in the df
    if (input$time_signature == '3/4'){
      time_signature_input <- 3
    } else {
      time_signature_input <- 4
    }
    
    # Filter dataframe based off of user inputs
    # Learn about audio features here: https://developer.spotify.com/web-api/get-audio-features/
    m <- spotify_df %>%
      filter(
        danceability >= input$danceability[1],
        danceability <= input$danceability[2],
        energy >= input$energy[1],
        energy <= input$energy[2],
        speechiness >= input$speechiness[1],
        speechiness <= input$speechiness[2],
        tempo >= input$tempo[1],
        tempo <= input$tempo[2],
        instrumentalness >= input$instrumentalness[1],
        instrumentalness <= input$instrumentalness[2]
      )
    
    # If time signature is specified, filter to it
    if (input$time_signature != 'Any') {
      m <- spotify_df %>%
        filter(time_signature == time_signature_input)
    }
    
    # Allow user to filter by song or artist name
    # Need to take caps into account
    cat(sprintf('Song filtered to: %s', input$name))
    if (input$name != "") {
      m <- filter(m, grepl(input$name, name, fixed = TRUE))
    }
    if (input$artist != "") {
      m <- filter(m, grepl(input$artist, artist, fixed = TRUE))
    }
    m <- as.data.frame(m)
    m
  })
  
  # Create hovering tooltip
  song_tooltip <- function(x) {
    
    # Filter to the specific song user is hovering over
    all_songs <- isolate(songs())
    song <- all_songs[all_songs$name == x$name, ]
    paste0(tags$img(src = song$track_img, width = "100px", height = "100px"),
           br(),
          "<br><b>\nSong Title: ", song$name, "</b><br>",
          "<b>Artist: ", song$artist, "</b><br>",
           tags$audio(src=song$preview_url, type = "audio/mp3", autoplay = "autoplay", controls = "controls"))
  }
  
  # Create day-of-week bar chart
  bar_chart <- dow_df %>% ggvis(~day_added, ~songs_added) %>% layer_bars(fill = ~factor(day_added), stroke := 'white') %>%
    add_axis("x", title = 'Day of Week', title_offset = 100,
             properties = axis_props(labels = list(angle = 45, align = "left", fontSize = 14), title = list(fontSize = 16))) %>%
    add_legend("fill", title = "Day of Week Added") %>%
    add_axis("y", title = 'Songs Added', title_offset = 50, tick_padding=20,
             properties = axis_props(labels = list(align = "left", fontSize = 14), title = list(fontSize = 16))) %>%
    set_options(width = 600, height = 600, resizable=TRUE) %>%
    bind_shiny("bar_chart")
  
  # Create month bar chart
  bar_chart_month <- month_df %>% ggvis(~month_added, ~songs_added_per_month) %>% layer_bars(fill = ~factor(month_added), stroke := 'white') %>%
    add_axis("x", title = 'Month', title_offset = 100,
             properties = axis_props(labels = list(angle = 45, align = "left", fontSize = 14), title = list(fontSize = 16))) %>%
    add_legend("fill", title = "Month Added") %>%
    add_axis("y", title = 'Songs Added', title_offset = 50, tick_padding=20,
             properties = axis_props(labels = list(align = "left", fontSize = 14), title = list(fontSize = 16))) %>%
    set_options(width = 850, height = 600, resizable=TRUE) %>%
    bind_shiny("bar_chart_month")
  
  vis <- reactive({
    
    # Create graph for each month showing most common moods
    output$mood1 <- renderGvis({ gvisPieChart(list_of_mood_dfs[[1]], options=list(height=300)) })
    output$mood2 <- renderGvis({ gvisPieChart(list_of_mood_dfs[[2]], options=list(height=300)) })
    output$mood3 <- renderGvis({ gvisPieChart(list_of_mood_dfs[[3]], options=list(height=300)) })
    output$mood4 <- renderGvis({ gvisPieChart(list_of_mood_dfs[[4]], options=list(height=300)) })
    output$mood5 <- renderGvis({ gvisPieChart(list_of_mood_dfs[[5]], options=list(height=300)) })
    output$mood6 <- renderGvis({ gvisPieChart(list_of_mood_dfs[[6]], options=list(height=300)) })
    output$mood7 <- renderGvis({ gvisPieChart(list_of_mood_dfs[[7]], options=list(height=300)) })
    output$mood8 <- renderGvis({ gvisPieChart(list_of_mood_dfs[[8]], options=list(height=300)) })
    output$mood9 <- renderGvis({ gvisPieChart(list_of_mood_dfs[[9]], options=list(height=300)) })
    output$mood10 <- renderGvis({ gvisPieChart(list_of_mood_dfs[[10]], options=list(height=300)) })
    output$mood11 <- renderGvis({ gvisPieChart(list_of_mood_dfs[[11]], options=list(height=300)) })
    output$mood12 <- renderGvis({ gvisPieChart(list_of_mood_dfs[[12]], options=list(height=300)) })
    
    output$genre1 <- renderGvis({ gvisPieChart(list_of_genre_dfs[[1]], options=list(height=300)) })
    output$genre2 <- renderGvis({ gvisPieChart(list_of_genre_dfs[[2]], options=list(height=300)) })
    output$genre3 <- renderGvis({ gvisPieChart(list_of_genre_dfs[[3]], options=list(height=300)) })
    output$genre4 <- renderGvis({ gvisPieChart(list_of_genre_dfs[[4]], options=list(height=300)) })
    output$genre5 <- renderGvis({ gvisPieChart(list_of_genre_dfs[[5]], options=list(height=300)) })
    output$genre6 <- renderGvis({ gvisPieChart(list_of_genre_dfs[[6]], options=list(height=300)) })
    output$genre7 <- renderGvis({ gvisPieChart(list_of_genre_dfs[[7]], options=list(height=300)) })
    output$genre8 <- renderGvis({ gvisPieChart(list_of_genre_dfs[[8]], options=list(height=300)) })
    output$genre9 <- renderGvis({ gvisPieChart(list_of_genre_dfs[[9]], options=list(height=300)) })
    output$genre10 <- renderGvis({ gvisPieChart(list_of_genre_dfs[[10]], options=list(height=300)) })
    output$genre11 <- renderGvis({ gvisPieChart(list_of_genre_dfs[[11]], options=list(height=300)) })
    output$genre12 <- renderGvis({ gvisPieChart(list_of_genre_dfs[[12]], options=list(height=300)) })
    
    # Set axis names
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    # Read in variables as strings
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    # When date added is the x-axis var, graph needs to be handled differently
    if (input$xvar != 'added_at') {
      cat(sprintf(input$xvar))
      songs %>%
        ggvis(x = xvar, y = yvar, fill = ~factor(day_added), strokeWidth := "0") %>%
        layer_points(size := 50, size.hover := 200, opacity := "0.8", key := ~name) %>%
        add_tooltip(song_tooltip, "hover") %>%
        add_axis("x", title = xvar_name, title_offset = 50,
                 properties = axis_props(labels = list(angle = 45, align = "left", fontSize = 14), title = list(fontSize = 16))) %>%
        add_axis("y", title = yvar_name, title_offset = 50, tick_padding=20,
                 properties = axis_props(labels = list(align = "left", fontSize = 14), title = list(fontSize = 16))) %>%
        set_options(width = 1200, height = 700, resizable=TRUE)
    } else {
      songs %>%
        ggvis(x = xvar, y = yvar, fill = ~factor(day_added), strokeWidth := "0") %>%
        layer_points(size := 50, size.hover := 200, opacity := "0.8", key := ~name) %>%
        add_legend("fill", title = "Day of Week Added") %>%
        add_tooltip(song_tooltip, "hover") %>%
        add_axis("x", title = xvar_name, title_offset = 100,
                 properties = axis_props(labels = list(angle = 45, align = "left", fontSize = 14), title = list(fontSize = 16))) %>%
        scale_datetime("x", nice = "week") %>%
        add_axis("y", title = yvar_name, title_offset = 50, tick_padding=20,
                 properties = axis_props(labels = list(align = "left", fontSize = 14), title = list(fontSize = 16))) %>%
        set_options(width = 1200, height = 700, resizable=TRUE)
    }
  })
  
  vis %>% bind_shiny("song_plot")
  
  # Calculate user hipster score by averaging out the popularity of all songs
  hipster_score <- 100 - round(mean(spotify_df$popularity),2)
  output$hipster_score <- renderText({ 
    paste(hipster_score)
  })
  
})

library(shiny)
library(plyr)
library(dplyr)
library(zoo)
library(googleVis)
library(data.table)
library(devtools)
library(spotifyr)
library(httr)

client_redirect_uri = 'https://www.spotify.com/'
clientID = '930456c642564135939390fde3dd06bf'
secret = '1c7b545d45b841f9aeaee87b70d35be6'

set_credentials(client_id = clientID, client_secret = secret, client_redirect_uri = client_redirect_uri)

# FUNCTIONS --------------------------------------------------

# Get user access and refresh token given user code
get_user_token <- function(user_code){
  
  response <- POST('https://accounts.spotify.com/api/token', accept_json(), authenticate(clientID,secret),
                   body=list(grant_type='authorization_code', code=user_code, redirect_uri=client_redirect_uri), encode='form')
  content <- get_response_content(response)
  
  # Make accessible globally
  assign('access_token',content$access_token,envir = .GlobalEnv)
  assign('refresh_token',content$refresh_token,envir = .GlobalEnv)
  
  content
}

# Function to turn NULL values into empty strings
nullToBlank <- function(x) {
  x[sapply(x, is.null)] <- ''
  return(x)
}
nullToZero <- function(x) {
  x[sapply(x, is.null)] <- 0
  return(x)
}

# Filtering to 2016-on and getting day of week info
date_manipulations = function(spotify_df){
  spotify_df$added_at <- as.Date(spotify_df$added_at, "%Y-%m-%d")
  
  # Delete all songs added before 2016
  earliest_date <- as.Date("01/01/16", "%m/%d/%y")
  spotify_df <- spotify_df[spotify_df$added_at >= earliest_date,]
  
  # Get day of week songs were added & most common days
  spotify_df$day_added <- weekdays(as.Date(spotify_df$added_at,"%m/%d/%y"))
  spotify_df$day_added <- factor(spotify_df$day_added, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  spotify_df <- spotify_df[order(spotify_df$day_added), ]
  return(spotify_df)
}

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

# We want to start with an empty df
spotify_df <- read.csv("df_saved_songs.csv")
spotify_df = date_manipulations(spotify_df)

dow_df <- spotify_df %>% group_by(day_added) %>% dplyr::summarise(songs_added=n())

# Get month songs were added & most common months
spotify_df$month_added <- months(as.Date(spotify_df$added_at))
spotify_df$month_added <- factor(spotify_df$month_added, levels= c("January", "February", "March", "April", "May", "June",
                                                                   "July", "August", "September", "October", "November", "December"))
month_df <- spotify_df %>% group_by(month_added) %>% dplyr::summarise(songs_added_per_month=n())

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

access_token <- ''

shinyServer(function(input, output, session) {
  
  # Get user code for Authorization Code user code
  response <- GET(url=authorize_url, query=list(client_id=clientID, response_type='code', scope=all_scopes, redirect_uri=client_redirect_uri))
  navigate_url <- response$url
  
  output$auth <- renderUI({
    HTML(paste0('<a href = "', navigate_url, '"><b>1. Open this link in a new tab/window to log in to your Spotify account.</b> </a>'))
  })
  
  songs <- reactive({

    # If user wants to get his data in real time, he will input URL
    if (input$url != "") {

      # Only get an access token if one does not exist already
      if (access_token == '') {
        user_code <- str_split(input$url,pattern='code=')[[1]][2]
        user_code <- gsub('#_=_','', user_code)
        print(paste('user code: ', user_code))
        access_token_outputs <- get_user_token(user_code)
        access_token <<- access_token_outputs$access_token # can access globally outside of reactive function
        refresh_token <<- access_token_outputs$refresh_token
      }
 
      # Only query the API if we haven't gotten the data yet
      print('number of rows:')
      print(nrow(spotify_df))
      if (nrow(spotify_df) <= 1) {
        
        print(paste('access token: ', access_token))
        offset_val = 0; total_results = 0
        saved_songs_df <- data.frame()
        while (offset_val <= total_results) {
          print(paste('offset_val: ', offset_val))
          Sys.sleep(5)
          results <- content(GET(url = library_url, query=list(limit=50, offset=offset_val), add_headers(Authorization=paste('Bearer',access_token))))
          print(results[[1]])
  
          i = 1;
          while ((results[[1]] != '7b') & (i <= (results$total %% 50))) {
            total_results <- results$total
            name = results$item[[i]]$track$name
            artist = results$item[[i]]$track$artists[[1]]$name
            popularity = results$item[[i]]$track$popularity
            track_img = results$item[[i]]$track$album$images[[1]]$url
            preview_url = results$item[[i]]$track$preview_url
            duration_ms = results$item[[i]]$track$duration_ms
            added_at = gsub("T.*$", "", results$item[[i]]$added_at)
            track_id = results$item[[i]]$track$id
  
            # Feed in track_id to get audio features
            URI = paste0('https://api.spotify.com/v1/audio-features/', track_id)
            headerValue = paste("Bearer", access_token)
            request = GET(url = URI, add_headers(Authorization = headerValue))
            json_parsed = fromJSON(content(request, "text"))
            
            acousticness = json_parsed$acousticness
            danceability = json_parsed$danceability
            energy = json_parsed$energy
            instrumentalness = json_parsed$instrumentalness
            key = json_parsed$key
            liveness = json_parsed$liveness
            loudness = json_parsed$loudness
            speechiness = json_parsed$speechiness
            tempo = json_parsed$tempo
            time_signature = json_parsed$time_signature
            valence = json_parsed$valence
            
            # Sometimes we encounter blank values, and in those cases we need to fill them with empty strings or 0
            list_of_str_vars = list(name, artist, added_at, preview_url, track_img)
            list_of_int_vars = list(popularity, acousticness, danceability, duration_ms, energy, instrumentalness, key, liveness,
                                loudness, speechiness, tempo, time_signature, valence)
            list_of_str_vars = nullToBlank(list_of_str_vars)
            list_of_int_vars = nullToZero(list_of_int_vars)
  
            mini_df <- data.frame(name = list_of_str_vars[[1]], artist = list_of_str_vars[[2]], popularity = list_of_int_vars[[1]], acousticness = list_of_int_vars[[2]],
                                  danceability = list_of_int_vars[[3]], duration_ms = list_of_int_vars[[4]], energy = list_of_int_vars[[5]], instrumentalness = list_of_int_vars[[6]],
                                  key = list_of_int_vars[[7]], liveness = list_of_int_vars[[8]], loudness = list_of_int_vars[[9]], speechiness = list_of_int_vars[[10]],
                                  tempo = list_of_int_vars[[11]], time_signature = list_of_int_vars[[12]], valence = list_of_int_vars[[13]],
                                  added_at = list_of_str_vars[[3]], preview_url = list_of_str_vars[[4]], track_img = list_of_str_vars[[5]])
            saved_songs_df <- rbind(saved_songs_df, mini_df)
  
            i = i + 1;
          }
          offset_val = offset_val + 50
        }
        print(saved_songs_df)
        spotify_df <<- saved_songs_df
      }
      spotify_df <- date_manipulations(spotify_df)
      
      # Get day of week data frame
      dow_df <- spotify_df %>% group_by(day_added) %>% dplyr::summarise(songs_added=n())

      # Get month songs were added & most common months
      spotify_df$month_added <- months(as.Date(spotify_df$added_at))
      spotify_df$month_added <- factor(spotify_df$month_added, levels= c("January", "February", "March", "April", "May", "June",
                                                                         "July", "August", "September", "October", "November", "December"))
      month_df <- spotify_df %>% group_by(month_added) %>% dplyr::summarise(songs_added_per_month=n())

      # Map key number to major/minor key
      spotify_df$key <- keys[match(spotify_df$key, values)]
    }
    
    # Create data table of all songs for the 'Personal Attributes' tab
    output$spotify_df = renderDataTable({
      spotify_df <- spotify_df[c("name", "artist", "key", "added_at", "day_added", "tempo", "popularity",
                                 "danceability", "energy", "acousticness", "instrumentalness", "speechiness")]
    })
    
    bar_chart <- dow_df %>% ggvis(~day_added, ~songs_added) %>% layer_bars(fill = ~factor(day_added), stroke := 'white') %>%
      add_axis("x", title = 'Day of Week', title_offset = 100,
               properties = axis_props(labels = list(angle = 45, align = "left", fontSize = 14), title = list(fontSize = 16))) %>%
      add_legend("fill", title = "Day Added") %>%
      add_axis("y", title = 'Songs Added', title_offset = 50, tick_padding=20,
               properties = axis_props(labels = list(align = "left", fontSize = 14), title = list(fontSize = 16))) %>%
      set_options(width = 500, height = 600, resizable=TRUE) %>%
      bind_shiny("bar_chart")
    
    # Create month bar chart
    bar_chart_month <- month_df %>% ggvis(~month_added, ~songs_added_per_month) %>% layer_bars(fill = ~factor(month_added), stroke := 'white') %>%
      add_axis("x", title = 'Month', title_offset = 100,
               properties = axis_props(labels = list(angle = 45, align = "left", fontSize = 14), title = list(fontSize = 16))) %>%
      add_legend("fill", title = "Month Added") %>%
      add_axis("y", title = 'Songs Added', title_offset = 50, tick_padding=20,
               properties = axis_props(labels = list(align = "left", fontSize = 14), title = list(fontSize = 16))) %>%
      set_options(width = 750, height = 600, resizable=TRUE) %>%
      bind_shiny("bar_chart_month")
    
    # Calculate user hipster score by averaging out the popularity of all songs
    hipster_score <- 100 - round(mean(spotify_df$popularity),2)
    output$hipster_score <- renderText({
      paste(hipster_score)
    })
    
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
  
  # Create hovering tooltip
  song_tooltip <- function(x) {
    
    # Filter to the specific song user is hovering over
    all_songs <- isolate(songs())
    song <- all_songs[all_songs$name == x$name, ]
    print(song$track_img)
    paste0(tags$img(src = song$track_img, width = "100px", height = "100px"),
           br(),
          "<br><b>\nSong Title: ", song$name, "</b><br>",
          "<b>Artist: ", song$artist, "</b><br>",
           tags$audio(src=song$preview_url, type = "audio/mp3", autoplay = "autoplay", controls = "controls"))
  }
  
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
        set_options(width = 1100, height = 700, resizable=TRUE)
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
  
})

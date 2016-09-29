library(shiny)
library(leaflet)
library(streamR)

# sample data
tweets_df <- parseTweets("tweets_streaming_2_25_4.json", simplify = FALSE)
tweets_df[is.na(tweets_df)] <- 0
tweets_df_loc_taggable <- subset(tweets_df, tweets_df$place_lon > 0 & tweets_df$place_lat > 0)
tweets_df_loc_taggable_text <- apply(tweets_df_loc_taggable, 1, function(row) row["text"])


# function to get tweets
get_tweets <- function(tweet_text, tweets_df_loc_taggable) {
  
  # gather streaming data using filterstream
  tweets <- filterStream(file.name = "",
               track = tweet_text,
               language = "en",
               timeout = 180,
               oauth = my_oauth)
  
  # not using one of the parameters, throws an error, so we just call it and keep it here.
  tweets_df_loc_taggable
  
  # check if enough tweets were received
  if (length(tweets) >= 30) {
    tweets <- parseTweets(tweets)
    tweets <- subset(tweets, !is.na(place_lon) & !is.na(place_lat))
    
    lon <- ifelse(is.na(tweets$lon), 0, tweets$lon)
    lat <- ifelse(is.na(tweets$lat), 0, tweets$lat)

    #clean tweets
    text <- gsub('[^[:graph:]]', ' ', as.character(tweets$text))
    text <- gsub('^ +', '', text)
    text <- gsub(' +$', '', text)
    text <- gsub(' +', ' ', text)
    tweets$text <- text
  
    return(tweets) 
  }
  else {
    return(tweets_df_loc_taggable)
  }
}

#creating the UI for the application
ui <- fluidPage(
  titlePanel("TwiStreamer"),
  leafletOutput("mymap"),
  p(),
  column(6, 
         textInput("tweet_query", label = h4("What are you searching today?"), 
                   value = "")
  ),
  column(6,
         tags$h5("Some of the tweets from the query"),
         tags$div(verbatimTextOutput("summary"))
         )
)

# server code of the application
server <- function(input, output, session) {
  
  get_input <- reactive({
      get_tweets(input$tweet_query,tweets_df_loc_taggable)
  })
  
  output$mymap <- renderLeaflet({
    leaflet(get_input()) %>%
      addTiles() %>%
      addCircleMarkers(~place_lon, ~place_lat, radius = 10, weight = 1, color = "#FF0000",
                 fillOpacity = 0.7, popup = ~text)
  })
  
  output$summary <- renderPrint({     
                          tweets_summary <- get_input()
                          tweets_summary$text
  }) 
}

shinyApp(ui, server)
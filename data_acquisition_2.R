library(jsonlite)
file <- NULL
existing_twt <- NULL
if (file.exists("tweet_collection.json")){
  existing_twt<- fromJSON("tweet_collection.json")
}

print(head(existing_twt))
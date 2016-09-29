#!/usr/local/bin Rscript
# Specifying Libraries to be used
library(twitteR)
library(jsonlite)
library(base64enc)

#Credentials are set first
options(httr_oauth_cache=T)
setup_twitter_oauth("", "",
                    access_token = "", 
                    access_secret = "")

#Searching twitter with specific terms
search_real_estate <- searchTwitter("'real estate NYC' OR 'real estate New York' OR 'real estate Manhattan' OR 'real estate New York City' OR 'real estate Bronx' OR 'real estate home'", n =100)
search_housing <- searchTwitter("'housing New York' OR 'housing NYC' OR 'housing New York City' OR 'housing Manhattan'", n = 100)
search_residential <- searchTwitter("'residential New York' OR 'residential NYC' OR 'residential New York City' OR 'residential Manhattan'", n = 100)
search_realtor <- searchTwitter("'realtor NYC' OR 'realtor New York City' OR 'realtor Manhattan'", n = 100)
search_house <- searchTwitter("'house NYC' OR 'house New York City' OR 'house Manhattan' OR 'house NYC' OR 'house New York City'", n= 100)
search_foreclosure <- searchTwitter("'foreclosure NYC' OR 'foreclosure New York City' OR 'foreclosure Manhattan'", n = 100)
search_condos <- searchTwitter("'condos NYC' OR 'condos New York City' OR 'condos Manhattan'", n = 100)
search_apartments <- searchTwitter("'apartments NYC' OR 'apartments New York City' OR 'apartments Manhattan'", n = 100)
search_misc_terms <- searchTwitter("'Realtor' OR 'fsbo' OR 'buy home' OR 'condos' OR 'apartments'", n = 200, geocode ='40.712784,-74.005941,200mi')
search_composite_tweets <- list()
search_composite_tweets <- c(search_real_estate,search_housing,search_residential,search_realtor,search_house,search_foreclosure,search_misc_terms)
search_composite_tweets <- unique(search_composite_tweets)

# Check if file exists already, if it does we should append to it 
existing_twt_df <- NULL
if (file.exists("/tweet_collection.json")){existing_twt_df<- fromJSON("/tweet_collection.json")}

# Merge the data extracted from the file with our newly collected tweets
new_twt_df <- do.call("rbind", lapply(search_composite_tweets, as.data.frame))
new_twt_df$latitude <- NULL
new_twt_df$longitude <- NULL
new_twt_df <- rbind(new_twt_df,existing_twt_df)

# Remove duplicates using !duplicated method on field id, this stops duplicate tweets
new_twt_df <- new_twt_df[!duplicated(new_twt_df[c("id")]),]

# Write to the file again with the updated dataset after converting it to JSON object
json_convert <- toJSON(new_twt_df)
write(json_convert, file ="/tweet_collection.json")
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(wordcloud)
library(jsonlite)
library(SnowballC)

# read txt files with names of the form nyt
setwd('~/CSE_587/Projects/Dataset')
data_csv_files <- list.files(path=getwd(), pattern = 'rollingsales.*\\.csv$');

data_list <- lapply(data_csv_files, read.csv, sep = "," , skip = 4)

# collect tweet data
if (file.exists("tweet_collection.json")){existing_twt_df<- fromJSON("tweet_collection.json")}

# clean-up things
existing_twt_df$text <- gsub("rt", "", existing_twt_df$text)
existing_twt_df$text <- gsub("@\\w+", "", existing_twt_df$text)
existing_twt_df$text <- gsub("[[:punct:]]", "", existing_twt_df$text)
existing_twt_df$text <- gsub("http\\w+", "", existing_twt_df$text)
existing_twt_df$text <- gsub("[ |\t]{2,}", "", existing_twt_df$text)
existing_twt_df$text <- gsub("^ ", "", existing_twt_df$text)
existing_twt_df$text <- gsub(" $", "", existing_twt_df$text)

myCorpus <- Corpus(VectorSource(existing_twt_df$text))

myCorpus <- tm_map(myCorpus, 
                   content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                   mc.cores=1
                   )

# remove stopwords
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"),  mc.cores=1)

# Generate WordCloud
wordcloud(myCorpus, min.freq=2, max.words=100, random.order=T)

# Combine both csv and tweet locations to get a summary of maximum words

data_bronx_hoods <- subset(data_list[[1]], select=c("NEIGHBORHOOD"))
data_brooklyn_hoods <- subset(data_list[[2]], select=c("NEIGHBORHOOD"))
data_manhattan_hoods <- subset(data_list[[3]], select=c("NEIGHBORHOOD"))
data_queens_hoods <- subset(data_list[[4]], select=c("NEIGHBORHOOD"))
data_staten_hoods <- subset(data_list[[5]], select=c("NEIGHBORHOOD"))

data_neighborhood_cumu <- rbind(data_neighborhood_cumu, data_bronx_hoods)
data_neighborhood_cumu <- rbind(data_neighborhood_cumu, data_brooklyn_hoods)
data_neighborhood_cumu <- rbind(data_neighborhood_cumu, data_manhattan_hoods)
data_neighborhood_cumu <- rbind(data_neighborhood_cumu, data_queens_hoods)
data_neighborhood_cumu <- rbind(data_neighborhood_cumu, data_staten_hoods)

data_neighborhood_cumu_final <- as.data.frame(table(unlist(data_neighborhood_cumu)))

wordcloud(words = data_neighborhood_cumu_final$Var1, freq = data_neighborhood_cumu_final$Freq, min.freq = 1, max.words=100)

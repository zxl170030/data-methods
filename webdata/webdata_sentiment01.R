# Data Method: Web data 
# File: webdata_sentiment01.R
# Theme: Access Twitter data and perform Sentiment Analysis

# Install packages to prepare for collecting and analyzing web data (Twitter)
install.packages("RColorBrewer")
install.packages("tm") # Text mining package
install.packages('ROAuth') # Authentication
install.packages('plyr') # Tools for Splitting, Applying and Combining Data# 
install.packages('stringr') # Wrappers for Common String Operations 
install.packages('twitteR') # Twitter client to interface to the Twitter web API
install.packages("ggmap") # Visualization with ggplot2 and get online map data such as Google 
# Load libraries

library(RColorBrewer)
library(tm)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)

# Twitter Authentication (after getting Twitter account and acquire credentials
#                        from https://dev.twitter.com/apps)
consumer_key <- "yourkey"
consumer_secret <- "yoursecret"
access_token <- "youraccess_token"
access_secret <- "youraccess_secret"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Collect Tweets from Dallas 

library(ggmap)
latlon <- geocode("Dallas, TX",source = "dsk")

# Be sure to plug in the lat and lon data
tic <- print(Sys.time()) # mark the time when the job starts
TC_tweet <- searchTwitter("Ted Cruz",  n=300, geocode='lat,lon, 100mi')
toc <- print(Sys.time()) # mark the time when the job stops
print(toc-tic) # calculate how much time it takes.

# Check out the text based data

str(head(TC_tweet,1))
length(TC_tweet)

# Organize the Twitter raw data into DF
TCtwt <- do.call("rbind", lapply(TC_tweet, as.data.frame))

# Sentiment analysis
# install needed packages

install.packages("lubridate") # Help with date data 
library(lubridate)
install.packages("tidytext") # Help clean up text data for text ming
library(tidytext)
library(dplyr) # tidyverse way of managing data
install.packages("tokenizers") # Tokenize corpus
library(tokenizers)
library(ggplot2)
library(tidyr)

# Extracting text only 

TCtxt <- data_frame(text=TCtwt$text)
tidytwt= TCtxt %>% 
  unnest_tokens(word, text)

# Call in the stop word dictionary

data(stop_words)
tidytwt <- tidytwt %>%  anti_join(stop_words)
tidytwt %>% 
  count(word,sort=T)

tidytwt %>%
  count(word, sort = TRUE) %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Ready data for plotting

tidytwt <- tidytwt %>%
  mutate(linenumber = row_number())

# Sort out positive and negative keywords
sentiment_CTT <- tidytwt %>%          
  inner_join(get_sentiments("bing")) %>%
  count(index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Plot sentments  
ggplot(sentiment_CTT, aes(index, sentiment)) +
  geom_col(show.legend = FALSE)

# Plot separate positive and negative uses
bing_word_counts <- tidytwt %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Can you do sentiment analysis on Beto?

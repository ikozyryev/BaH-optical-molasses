# first load all the necessary libraries
library(rtweet)
library(httpuv)
library(dplyr)
library(reshape)
library(ggplot2)
library(readr)
library(qdapRegex) # for removing URLs from tweet text
library(tm) # load the text mining package
library(tidytext)
library(tidyr)
library(dplyr)
library(textdata) # to load various text datasets for sentiment analysis
library(reshape2)
library(wordcloud)
# library(rlang)
#install.packages("tidyverse")
# library(qdap) # to extract term frequency
# search_tweets() # to connect to twitter and authorize browser pop-up
# options(max.print = 100)

# get all the trending topics now
US_trends <- get_trends("United States")
head(US_trends)
# get
# tweet_volume
# trends_available() # extract geographic locations where trends are available
# head(US_trends$trend)

trend_df <- US_trends %>% 
  group_by(trend) %>%
  summarize(tweet_vol = mean(tweet_volume))

# reorder the tweets by popularity
trends_df_sort <- arrange(trend_df, desc(tweet_vol))
# which(trends_df_sort[,1]=="#NFLDraft")

# lets perform the timeseries analysis next
# apply the necessary filter to obtain a manageable file
# #SaturdayThoughts
covid19_st <- search_tweets("#COVID19 -filter:retweets -filter:quote -filter:replies",n = 10000, include_rts = FALSE, lang = "en") # retryonratelimit = TRUE
head(covid19_st)
# determine which tweets have been retweeted at least ones
retwt_inds <- which(covid19_st$retweet_count>0)
# select only tweets which have been retweeted
# keep the status_id in order to account for the case if one use tweeted multiple times as a unique identifier 
retwted <- covid19_st[retwt_inds,c("status_id","screen_name","text","retweet_count","followers_count","friends_count")]
# create a time series plot
twt_txt <- retwted$text # get the tweet text
# start to clean the tweets data
# let's clean text for tweets before starting to analyze
twt_txt_no_url <- rm_twitter_url(twt_txt) # first remove the URL 
head(twt_txt_no_url)
twt_chrs <- gsub("[^A-Za-z]"," ", twt_txt_no_url) # only keep the upper and lower case characters; replace everything else with a space
head(twt_chrs)
retwted_df <- data.frame(retwted$status_id,retwted$screen_name,twt_chrs,retwted$retweet_count,retwted$followers_count,retwted$friends_count)

twts_tidy <- as_tibble(retwted_df)
# change the column names for convinience
twts_tidy <- twts_tidy %>% 
  dplyr::rename(id = retwted.status_id, user_name = retwted.screen_name, text = twt_chrs, retweets = retwted.retweet_count, friends = retwted.friends_count, followers = retwted.followers_count)
colnames(twts_tidy)

twts_tokens <- twts_tidy %>%
  unnest_tokens(word,text)

# remove the stop words
twts_clean <- twts_tokens %>%
  anti_join(get_stopwords())

# now lets assign a quantitative score to tweets to determine whether they are positive or negative
# we will use the afinn lexicon here

twts_afinn <- twts_clean %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(golden = followers/friends)
# now lets assign the sentiment score to each unique tweet

# twts_sentiment <- twts_afinn %>%
#   group_by(id) %>%
#   #summarize(score = sum(value)) %>%
#   mutate(score = sum(value))
#   #arrange(desc(score)) %>%
#   ungroup()

retwts_sentiment <- twts_afinn %>%
  group_by(id,retweets,followers) %>%
  summarize(score = sum(value)) %>%
  #mutate(score = sum(value)) %>%
  arrange(desc(score))# %>%
  #ungroup()

# plot to see if there is any relationship
retwts_sentiment %>%
  ggplot(aes(score,retweets,size=followers)) +
  geom_point()# +
  ylim(0,350)

### now lets try using "bing" lexicon
twts_bing <- twts_clean %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(golden = followers/friends)

twts_bing %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red","blue"),max.words =100)
ggsave("COVID19_wordcloud.png", width = 5, height = 5)

retwts_bing_sent <- twts_bing %>%
  group_by(id,retweets,followers) %>%
  # filter(retweets > 5) %>%
  count(id,retweets,sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)



retwts_bing_sent %>%
  ggplot(aes(sentiment,retweets)) +
  geom_bar(stat = "identity") 
+
  geom_smooth(model = lm) +
  ylim(0,500)

retwts_bing_sent %>%
  ggplot(aes(followers,retweets)) +
  geom_point() +
  geom_smooth() +
  ylim(0,500) +
  scale_x_log10()

  summarize(score = sum(value)) %>%
  #mutate(score = sum(value)) %>%
  arrange(desc(score))# %>%
#ungroup()

twts_clean %>%
  count(word,sort = TRUE)

# convert the text to a vector source > corpus
twt_corpus <- twt_txt_chrs %>%
  VectorSource() %>%
  Corpus()
twt_corpus[[2]]$content

twt_corpus_lwr <- tm_map(twt_corpus, tolower) # convert all the letters to lower case
twt_corpus_lwr[[2]]$content # check to see if it worked

# remove stopwords next
twt_corpus_no_stpwd <- tm_map(twt_corpus_lwr, removeWords, stopwords("english"))
twt_corpus_no_stpwd[[2]]$content

# next remove additional white spaces
twt_corpus_final <- tm_map(twt_corpus_no_stpwd, stripWhitespace)
twt_corpus_final[[2]]$content

custom_stop <- c("Covid")

#### let's start processing the tweeter data now

# visualize populat terms from tweets

term_count <- freq_terms(twt_corpus_final, 60)

s_plot(covid19_st, by = "minutes", color = "red")

stream_tweets("covid_19",parse = FALSE,file_name = '')
# get the time series data
camry_st <- ts_data(camry_st, by='hours')
head(camry_st)
names(camry_st) <- c("time","camry_n")
head(camry_st)

merged_df <- merge(tesla_ts, camry_st, by = 'time', all = TRUE)
head(merged_df)

# stack the tweet frwquency columns using melt() function
# prepare the data frame for plotting with ggplot
melt_df <- melt(merged_df, na.rm = TRUE, id.vars = "time")
head(melt_df)

# plot the results now
ggplot(data = melt_df, 
       aes(x = time, y = value, col = variable)) + 
      geom_line(lwd = 0.8)

#### let's look at the CDCgov twits 

CDC <- get_timelines("@katyperry", n = 3) # max number of tweets per user is 3,200


  
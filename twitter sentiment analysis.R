#install.packages("rtweet", "dplyr", "tidyr", "tidytext", "purrr", "wordcloud")
library("rtweet")
library("dplyr")
library("tidyr")
library("tidytext")
library("ggplot2")
library("purrr")
library("wordcloud")

## store api keys (MUST UPDATE with your developer account credentials -- Essentials level account required for v1.1 api access)
api_key <- ""
api_secret_key <- ""
bearer_token <- ""
access_token <- ""
access_token_secret <- ""

## authenticate 
token = create_token(
    app = "TBS_Vaccine_Sentiment_Analysis",
    consumer_key = api_key,
    consumer_secret = api_secret_key,
    access_token = access_token,
    access_secret = access_token_secret)

get_token()

get_stems = function(tweets) {
    tweets_text = tweets %>% select(screen_name, text) # save the author and tweet into a variable
    print(head(tweets_text))
    
    # strip the link to the tweet from the end of the text
    tweets_text$stripped = gsub("http\\S+", "", tweets_text$text)
    
    # tokenize/convert words in tweets to stems that can be analyzed for sentiment. This method strips punctuation and makes words lowercase, then removes small words like articles and prepositions
    tweets_text_stems = tweets_text %>% select(stripped) %>% unnest_tokens(word, stripped) %>% anti_join(stop_words)
    
    print(head(tweets_text_stems))
    return(tweets_text_stems)
}

# sentiment analysis with the bing lexicon
sentiment_bing = function(tweets_text) {
    sentiments = get_sentiments("bing") %>% 
        filter(word!="supreme") %>% 
        filter(word!="trump")
    
    # clean the tweet text
    tweet_tbl = tibble(text = tweets_text) %>%
        mutate(
            stripped = gsub("http\\S+", "", tweets_text)
        ) %>%
        unnest_tokens(word, stripped) %>%
        anti_join(stop_words) %>%
        inner_join(sentiments) %>% # join the list of sentiment words from the bing library with any matches in the tweets tibble
        count(word, sentiment, sort = TRUE) %>%
        ungroup() %>%
        # map negative sentiments as -1 and positive as 1
        mutate(
            score = case_when(
                sentiment == "negative" ~ n*(-1),
                sentiment == "positive" ~ n*1
            )
        )
    
    #sum the scores
    sent_score = case_when(
        nrow(tweet_tbl) == 0 ~ 0, # if there aren't any words in the tibble, the score is 0
        nrow(tweet_tbl) > 0 ~ sum(tweet_tbl$score) # if there are words, just sum positive and negative scores
    )
    
    # when the score is 0, find out whether it's because no words were returned or if the total sum is 0
    zero_type = case_when(
        nrow(tweet_tbl) == 0 ~ "No sentiment words found", # no words at all
        nrow(tweet_tbl) > 0 ~ "Sentiment words exist", # sum of positive and negative words is 0
    )
    list(score = sent_score, type = zero_type, tweet_tbl = tweet_tbl)
}

get_overall_sentiment = function(tweets) {
    tweets_sent = lapply(tweets$text, sentiment_bing)
    
    overall_sentiment = bind_rows(
        tibble(
            subject = "vaccine mandate",
            score = unlist(map(tweets_sent, "score")),
            type = unlist(map(tweets_sent, "type"))
        )
    )
    
    print(overall_sentiment)
    
    return(overall_sentiment)
}

get_avg_sentiment = function(overall_sentiment) {
    return(mean(overall_sentiment$score))
} 


# stems analysis of positive/negative words
perform_stems_analysis = function(tweets) {
    sentiments = get_sentiments("bing") %>% 
        filter(word!="supreme") %>% 
        filter(word!="trump")
    
    stems = get_stems(tweets)
    
    bing_tweets = stems %>% inner_join(sentiments) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()
    
    print(bing_tweets)
    
    set.seed(1)
    wordcloud(words = bing_tweets$word, freq = bing_tweets$n, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2")) # make sure the plot window is large enough to display the full word cloud.
    
    # Perform sentiment analysis using the Bing lexicon and get_sentiments function from the tidytext package
    bing_results = stems %>% 
        inner_join(sentiments) %>% # suppress messages gets rid of the join by printout 
        count(word, sentiment, sort = TRUE) %>% 
        ungroup()
    
    # get the top 10 negative and positive words
    bing_results %>% 
        group_by(sentiment) %>% 
        top_n(10) %>% 
        ungroup() %>% 
        mutate(word = reorder(word, n)) %>% 
        ggplot(aes(word, n, sentiment)) + 
        geom_col(show.legend = FALSE) + 
        facet_wrap(~sentiment, scales = "free_y") + 
        labs(title = "Tweets containing 'vaccine mandate", y = "Contribution to sentiment", x = NULL) + 
        coord_flip()
}

perform_sentiment_analysis = function(query, num_tweets=100, geocode=NULL, sample_size=0) {
    results = search_tweets(
        query,
        n = num_tweets,
        type = "recent",
        include_rts = FALSE,
        geocode = geocode,
        max_id = NULL,
        parse = TRUE,
        retryonratelimit = TRUE,
        verbose = TRUE,
        lang = "en",
        tweet_mode = "extended"
    )
    
    if (sample_size > 0) {
        #get a subset for debugging/testing
        results = results[sample(nrow(results), sample_size), ]
    }
    
    tweets = results %>% select(screen_name, text, created_at) %>% drop_na()
    
    perform_stems_analysis(tweets)
    
    overall_sentiment = get_overall_sentiment(tweets)
    avg_sentment = get_avg_sentiment(overall_sentiment)
    print(avg_sentment)
    
    ggplot(overall_sentiment, aes(x = score, fill = subject)) + geom_histogram(col="grey")
    
    # get max and min date in the data
    min_date = as.POSIXct(min(tweets$created_at))
    max_date = as.POSIXct(max(tweets$created_at))
    
    # gets number of days using difftime function
    num_days = ceiling(as.double(difftime(as.POSIXct(max_date), as.POSIXct(min_date), units = "days")))
    
    # creates a sequence of dats starting at min_date
    date_seq = seq(as.Date(min_date), by = "day", length.out = num_days)
    
    #initialize an empty 0x3 tibble with the same col names as tweets
    empty_tbl = c("screen_ name", "text", "created_at") %>% purrr::map_dfc(setNames, object = list(character()))
    
    # initialize some empty lists
    daily_tweets = vector(mode = "list", length = 0)
    daily_score = vector(mode = "list", length = num_days)
    
    # format a list of empty tibbles called daily_tweets
    for (i in seq_along(date_seq)){
        daily_tweets = append(daily_tweets, list(empty_tbl))
    }
    
    # fill our list of empty tibbles with daily data, where each list element in daily_tweets is a different day
    for (i in seq_along(date_seq)){
        daily_tweets[[i]] = filter(tweets, as.Date(created_at) == as.Date(date_seq[i]))
        #get the daily sentiment score and store it in a separate list
        daily_score[i] = get_avg_sentiment(get_overall_sentiment(daily_tweets[[i]]))
    }
    
    #join our dates list with our daily_score tibble into a new dataframe
    daily_score = data.frame(unlist(date_seq), unlist(daily_score))
    names(daily_score) = c("date","score")
    
    # plot sentiment over time
    ggplot(data=daily_score, aes(x=date, y=score)) +
        geom_line(color="#aa0022") +
        geom_point(color="#aa0022") +
        ggtitle("Average Sentiment Score for vaccine mandate, Daily")
}

# variables used when retrieving tweets
query = "vaccine mandate"
num_tweets = 10000

# first we run it with no location set, so global results appear:
perform_sentiment_analysis(query, num_tweets = num_tweets, sample_size = 1000)

locations = list("40.683768905628,-73.9578089321052,200mi", 
                 "38.911740899081,-77.009051285332,200mi", 
                 "37.7588531529652,-122.436029586314,200mi", 
                 "32.6894723433889,-97.127152444191,200mi", 
                 "30.3064078662536,-81.6453317393833,200mi", 
                 "35.4844094736764,-97.5289315721415,200mi")
            # most republican/democrat cities from https://www.forbes.com/pictures/gfii45img/the-most-and-least-conservative-cities-in-america/?sh=1ac1790125e5
            # new york, washington, san francisco, arlington, jacksonville, oklahoma city

for (location in locations) {
    #we need to make sure these locations actually have data
    perform_sentiment_analysis(query, num_tweets = num_tweets, geocode = location, sample_size = 100)
}
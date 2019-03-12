library(rtweet)
library(readr)
library(dplyr)
library(futile.logger)

token = create_token(
  app = "dem_primary_nlp",
  consumer_key = config::get("consumer_api_key"),
  consumer_secret = config::get("consumer_api_secret"),
  access_token = config::get("access_token"),
  access_secret = config::get("access_token_secret"))

flog.logger("scraper", INFO, 
            appender = appender.file(paste0("logs/", as.character(Sys.Date()), ".log")))

paginate_tweets <- function(search_term, num_tweets, min_status_id = NULL) {
  search_tweets(search_term, 
                n = num_tweets, 
                max_id = min_status_id,
                includs_rts = FALSE,
                retryonratelimit = TRUE)
}

scrape_tweets <- function(candidate, n, step, output_csv, logger = "") {
  
  tweets_returned = 0
  min_status_id = NULL
  
  flog.info(sprintf("Starting scrape job at %s", as.character(Sys.time())),
            name= logger)
  all_tweets = list()
  i = 1
  while (tweets_returned < n) {
    tweets = paginate_tweets(candidate, step, min_status_id)
    write_csv(tweets, output_csv, append = file.exists(output_csv))
    tweets_returned = tweets_returned + nrow(tweets)
    min_status_id = min(tweets$status_id)
    flog.info(sprintf("Canidate %s: %d tweets for a total of %d. Earliest status_id: %s for time %s",
                      candidate, nrow(tweets), tweets_returned, min_status_id, as.character(min(tweets$created_at))))
    all_tweets[i] = tweets
    i = i + 1
  }
  
  return(bind_rows(all_tweets))
}

PrimaryField = readr::read_csv("candidates2020.csv")

candidates = PrimaryField %>% 
  mutate(full_name = paste(first_name, last_name)) %>% 
  pull(full_name) %>% tolower








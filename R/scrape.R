library(readr)
library(futile.logger)

source("R/util.R")

token = create_token(
  app = config::get("app_name"),
  consumer_key = config::get("consumer_api_key"),
  consumer_secret = config::get("consumer_api_secret"),
  access_token = config::get("access_token"),
  access_secret = config::get("access_token_secret"))

flog.logger("scraper", INFO, 
            appender = appender.file(paste0("logs/", as.character(Sys.Date()), ".log")))


###########################################################
#' Main function to grab a candidate's tweets
#' 
scrape_tweets <- function(candidate, n, step, output_csv, logger = "") {
  
  tweets_returned = 0
  if (file.exists(output_csv)) {
    min_status_id = read_csv(output_csv) %>% pull(status_id) %>% min
  } else {
    min_status_id = NULL
  }
  flog.info(sprintf("Starting scrape job for %s", candidate),
            name= logger)
  all_tweets = list()
  i = 1
  while (tweets_returned < n) {
    tweets = paginate_tweets(candidate, step, min_status_id)
    num_tweets = nrow(tweets)
    
    if (num_tweets == 0) break
    
    write_csv(tweets, output_csv, append = file.exists(output_csv))
    tweets_returned = tweets_returned + num_tweets
    min_status_id = min(tweets$status_id)
    flog.info(sprintf("Candidate %s: %d tweets for a total of %d. Earliest status_id: %s for time %s",
                      candidate, num_tweets, tweets_returned, min_status_id, as.character(min(tweets$created_at))),
              name=logger)
    all_tweets[i] = tweets
    i = i + 1
  }
  
  return(bind_rows(all_tweets))
}

PrimaryField = readr::read_csv("candidates2020.csv")

candidates = PrimaryField %>% 
  mutate(full_name = paste(first_name, last_name)) %>% 
  pull(full_name) %>% tolower

#map(candidates,
#    ~scrape_tweets(.,
#                  250000,
#                  5000,
#                  paste0("data/", ., "_", as.character(Sys.Date()), ".csv"),
#                  "scraper"),
#    )






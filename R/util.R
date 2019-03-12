library(purrr)
library(dplyr)
library(rtweet)


###########################################################
#' Transforms twitter geographic coordinates to strings
#' @description geo-coordinate list columns transformed to strings, which can be
#' written to CSV
#' @param coords: list, the list of coordinates
#' @return a ";" separate string with all of the coordinates
geo_coords_to_string <- function(coords) {
  empty_or_collapse <- function(row) {
    row = unlist(row)
    if (any(is.na(row))) {
      ""
    } else {
      paste(row, collapse = "; ")
    }
  }
  purrr::map_chr(coords, empty_or_collapse)
}



###########################################################
#' One call to search tweets
#'
paginate_tweets <- function(search_term, num_tweets, min_status_id = NULL) {
  search_tweets(search_term, 
                n = num_tweets, 
                max_id = min_status_id,
                includs_rts = FALSE,
                retryonratelimit = TRUE) %>% 
    mutate_if(is.list, geo_coords_to_string) %>%
    filter(status_id != min_status_id)
}
#' News scraper
#'
#' Scrapes links to news from the Rospotrebnadzor website
#' @import rvest dplyr stringr lubridate xml2
#'
#' @return A data frame with self-explanatory columns.
#'
#' @examples
#' # get the news
#' df <- get_news()
#' @export

get_news <- function() {
  newslinks_1 <- get_newslinks(1)
  news <- list()
  i <- 2
  repeat {
    newslinks <- get_newslinks(i)
    if (identical(newslinks, newslinks_1)) break
    news <- c(news, list(newslinks))
    i <- i + 1
  }
  news <- c(list(newslinks_1), news)
  news <- do.call(rbind, news)
  news <-
    news %>%
    mutate(
      names = names %>% str_remove_all("\\\\r") %>% str_remove_all("\\\\n") %>% trimws(),
      date = date %>% str_remove_all("\\\\r") %>% str_remove_all("\\\\n") %>% str_remove_all("г\\.") %>% trimws(),
      url = paste0("https://www.rospotrebnadzor.ru", url),
      date = date %>%
        str_replace_all("января", "янв") %>%
        str_replace_all("февраля", "фев") %>%
        str_replace_all("марта", "мар") %>%
        str_replace_all("апреля", "апр") %>%
        str_replace_all("мая", "май") %>%
        str_replace_all("июня", "июн") %>%
        str_replace_all("июля", "июл") %>%
        str_replace_all("августа", "авг") %>%
        str_replace_all("сентября", "сен") %>%
        str_replace_all("октября", "окт") %>%
        str_replace_all("ноября", "ноя") %>%
        str_replace_all("декабря", "дек"),
      date = parse_date_time(date, "d b y")
    )
  news
}

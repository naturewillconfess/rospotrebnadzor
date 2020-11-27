#' Regional tests scraper
#'
#' Scrapes data on the cumulative number of COVID-19 tests conducted in Russian regions from Rospotrebnadzor website, including the per capita data
#' @import rvest dplyr stringr lubridate xml2
#'
#' @param news news links data frame, output of `get_news()` or a subset of that data frame
#'
#' @return A data frame with self-explanatory columns.
#'
#' @examples
#' # get all the data
#' news <- get_news()
#' df <- get_tests_federal(news)
#' @export

get_tests_regional <- function(news) {
  news <-
    news %>%
    filter(str_detect(names, "О тестировании на новую коронавирусную инфекцию в регионах")) %>%
    mutate(date = ymd(date))

  testlist <- mapply(function(url, dater) {
    tables <-
      url %>%
      xml2::read_html() %>%
      html_table(fill = TRUE)

    percapita <- tables[[1]]
    if (ncol(percapita) > 3) percapita <- percapita[-1, c(3, 5)] else percapita <- percapita[-1, -1]
    if (length(tables) > 3) abs <- tables[[3]][-1, -1] else abs <- tables[[2]][-1, -1]
    colnames(percapita) <- c("region", "percapita")
    colnames(abs) <- c("region", "abs")

    df <- full_join(percapita, abs)
    df$dater <- dater
    df
  }, news$url, news$date, SIMPLIFY = FALSE)

  tests <- do.call(rbind, testlist)
  tests <-
    tests %>%
    filter(!(tests$percapita == "" & !is.na(tests$percapita))) %>%
    mutate(
      region = region %>%
        str_replace_all("Ямало-Ненецкий автономный округ", "Ямало-Ненецкий АО") %>%
        str_replace_all("Чукотский автономный округ", "Чукотский АО") %>%
        str_remove_all("г\\.|Г\\.") %>%
        str_remove_all("Якутия") %>%
        str_remove_all("\\(") %>%
        str_remove_all("\\)") %>%
        str_remove_all("Горный") %>%
        str_replace_all("  ", " ") %>%
        trimws(),
      percapita = as.numeric(str_replace_all(percapita, ",", "\\.")),
      abs = as.numeric(str_replace_all(abs, ",", "\\."))
    ) %>%
    group_by(region, dater) %>%
    summarize(
      percapita = ifelse(all(is.na(percapita)), NA, percapita[!is.na(percapita)]),
      abs = ifelse(all(is.na(abs)), NA, abs[!is.na(abs)])
    ) %>%
    ungroup()
  tests
}

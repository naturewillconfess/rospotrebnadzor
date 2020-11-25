#' Federal tests scraper
#'
#' Scrapes data on the cumulative number of COVID-19 tests conducted in Russia from Rospotrebnadzor website
#' @import rvest dplyr stringr lubridate xml2
#'
#' @param news news links data frame, output of `get_news()` or a subset of that data frame
#'
#' @return A data frame with self-explanatory columns.
#'
#' @examples
#' # get all the data from the first page
#' df <- get_tests_federal(get_news())
#' @export


get_tests_federal <- function(news) {
  news <-
    news %>%
    filter(str_detect(names, "Информационный бюллетень о ситуации")) %>%
    mutate(date = ymd(date))

  testlist <- mapply(function(url, dater) {
    tests <-
      url %>%
      xml2::read_html() %>%
      html_text() %>%
      str_extract_all("проведен.*лабораторн|проведен.*исслед") %>%
      `[[`(1) %>%
      `[`(str_detect(., "[0-9]"))

    if (length(tests) == 0) {
      return(data.frame(tests = NA, dater = dater))
    }
    if (length(tests) > 1) tests <- tests[str_detect(tests, "проведен.*лабораторн")]


    if (str_detect(tests, "тыс") & !str_detect(tests, "млн")) {
      tests <- as.numeric(tests %>% str_remove_all("[^0-9,]") %>% str_replace_all(",", "\\.")) * 1000
    } else if (!str_detect(tests, "тыс") & str_detect(tests, "млн")) {
      tests <- as.numeric(tests %>% str_remove_all("[^0-9,]") %>% str_replace_all(",", "\\.")) * 1000000
    } else if (!str_detect(tests, "тыс") & !str_detect(tests, "млн")) {
      tests <- as.numeric(tests %>% str_remove_all("[^0-9]"))
    } else if (str_detect(tests, "тыс") & str_detect(tests, "млн")) {
      tests <- str_match_all(tests, "^[^0-9]*([0-9]+)[^0-9]*млн[^0-9]*([0-9,]+)[^0-9]*тыс[^0-9]*([0-9]*)[^0-9]*$")[[1]][, -1]
      tests[2] <- str_replace_all(tests[2], ",", "\\.")
      if (tests[3] == "") tests[3] <- 0
      tests <- sum(as.numeric(tests) * c(10^6, 10^3, 1))
    }
    return(data.frame(tests = tests, dater = dater))
  }, news$url, news$date, SIMPLIFY = FALSE)

  tests <- do.call(rbind, testlist)
  tests <- arrange(tests, dater)
  tests
}

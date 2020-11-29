#' COVID-19 cases scraper
#'
#' Scrapes data on the number of new COVID-19 cases conducted in Russian regions from Rospotrebnadzor website
#' @import rvest dplyr stringr lubridate xml2
#'
#' @param news news links data frame, output of `get_news()` or a subset of that data frame
#'
#' @return A data frame with self-explanatory columns.
#'
#' @examples
#' # get all the data
#' news <- get_news()
#' df <- get_cases(news)
#' @export
#'
get_cases <- function(news) {
  news$date <- ymd(news$date)
  news <- news[str_detect(news$names, "О подтвержденных случаях новой коронавирусной инфекции") & news$date > ymd("2020-03-18") & news$date != ymd("2020-03-23"), ]
  caselist <- vector("list", nrow(news))
  xpath <- c(
    "//*[contains(text(), ' - ')][1]|//*[contains(text(), ' - ')][1]/following-sibling::*",
    "//text()[contains(., ' - ')]|//text()[contains(., ' - ')]",
    "//*[contains(text(), ' – ')][1]|//*[contains(text(), ' – ')][1]/following-sibling::*",
    "//text()[contains(., ' – ')]|//text()[contains(., ' – ')]",
    "//*[contains(text(), 'Москва')][1]/../..|//*[contains(text(), 'Москва')][1]/../../following-sibling::*" %>% iconv("windows-1251", "UTF-8"),
    "//*[contains(text(), 'Москва')][1]/../../..|//*[contains(text(), 'Москва')][1]/../../../following-sibling::*" %>% iconv("windows-1251", "UTF-8"),
    "//*[contains(text(), 'Москва')][1]/..|//*[contains(text(), 'Москва')][1]/../following-sibling::*" %>% iconv("windows-1251", "UTF-8"),
    "//*[contains(text(), 'Москва')][1]|//*[contains(text(), 'Москва')][1]/following-sibling::*" %>% iconv("windows-1251", "UTF-8")
  )

  caselist <- mapply(function(url, date) {
    page <- xml2::read_html(url)
    data <- 0
    j <- 1
    while (length(data) <= 15 & j <= length(xpath)) {
      data <-
        page %>%
        html_nodes(xpath = xpath[j]) %>%
        html_text() %>%
        str_remove_all("\\r") %>%
        str_remove_all("\\n") %>%
        str_remove_all("^[:space:]*$")
      data <- data[data != ""]
      j <- j + 1
    }


    data <-
      data %>%
      str_remove_all("[0-9]+\\.") %>%
      str_replace_all("[:space:]{2,}", " ") %>%
      trimws() %>%
      `[`(data != "" & data != "- NA") %>%
      str_match_all("^([^0-9]+)([ 0-9]+)[^0-9]*$")

    data <- as.data.frame(do.call(rbind, data)[, -1])
    colnames(data) <- c("region", "cases")
    data$date <- date
    data
  }, news$url, news$date, SIMPLIFY = FALSE)



  cases <- do.call(rbind, caselist)
  cases <-
    cases %>%
    filter(!str_detect(region, "Все|Ситуация|распространении|Определ|зарег|Роспотреб")) %>%
    mutate(
      region = region %>% str_remove_all("г\\.") %>% str_remove_all(" -|- |– | –") %>% str_remove_all("\\u200b") %>% trimws(),
      cases = cases %>% str_remove_all("[:space:]"),
      region = case_when(
        region == "Ненецкий автономный округ" ~ "Ненецкий АО",
        region == "Ямало-Ненецкий автономный округ" ~ "Ямало-Ненецкий АО",
        region == "Ханты-Мансийский автономный округ" ~ "Ханты-Мансийский АО",
        region == "Чукотский автономный округ" ~ "Чукотский АО",
        region == "Еврейская автономная область" ~ "Еврейская АО",
        region == "Республика Северная Осетия-Алания" ~ "Республика Северная Осетия",
        region == "Чувашская Республика" ~ "Республика Чувашия",
        region == "Республика Горный Алтай" ~ "Республика Алтай",
        TRUE ~ region
      )
    )
  cases
}

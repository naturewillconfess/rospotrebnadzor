get_newslinks <- function(i) {
  newspage <- paste0("https://www.rospotrebnadzor.ru/about/info/news/?PAGEN_1=", i)
  link <-
    newspage %>%
    xml2::read_html() %>%
    html_nodes(xpath = "/html/body/div[3]/ul/li[3]/div[2]/div[1]/div[1]/ul/li")

  date <- link %>%
    html_nodes("p") %>%
    html_text()
  url <- link %>%
    html_nodes("a") %>%
    html_attr("href")
  names <- link %>%
    html_nodes("a") %>%
    html_text()
  data.frame(url = url, names = names, date = date)
}

library(rvest)

x <- read_html("https://in.ign.com/middle-earth-shadow-of-mordor/65534/review/shadow-of-mordor-review")

ign_review <- x %>%
  html_nodes(".article-page") %>%
  html_text()
write.csv(ign_review,'ign.csv')

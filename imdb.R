library(rvest)
library(XML)
library(magrittr)

# Flipkart Reviews #############################
aurl <- "https://www.imdb.com/user/ur119552354/reviews?ref_=tt_urv"
flipkart_reviews <- NULL
for (i in 1:4){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  flipkart_reviews <- c(flipkart_reviews,rev)
}
write.table(flipkart_reviews,"imdb.txt")
write.csv(flipkart_reviews,file = "imdb.csv")
getwd()

library(rvest)
library(XML)
library(magrittr)

# Flipkart Reviews #############################
aurl <- "https://www.flipkart.com/mi-4a-pro-80-cm-32-hd-ready-led-smart-android-tv-google-data-saver/product-reviews/itmfdwh5jyqhmvzg?pid=TVSFDWH5K9N2FDTK&lid=LSTTVSFDWH5K9N2FDTK9M2YF1&marketplace=FLIPKART"
flipkart_reviews <- NULL
for (i in 1:4){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".qwjRop") %>%
    html_text()
  flipkart_reviews <- c(flipkart_reviews,rev)
}
write.table(flipkart_reviews,"flipkart.txt")

read <- readLines(file.choose())
View(read)
write.csv(flipkart_reviews,file = "flipkart.csv")
getwd()

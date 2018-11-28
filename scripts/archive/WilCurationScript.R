# Download the chrome extension from the URL below 
# in order to select and view CSS code elements on webpages.
# http://SelectorGadget.com

library(rvest)
library(stringr)

year = 2011
page <- html(paste0("http://www.presidency.ucsb.edu/executive_orders.php?year=",
                    year,
                    "&Submit=DISPLAY"
)
)

links <-
  page %>%
  html_nodes(".listdate a") %>%
  html_attr("href")

title <-
  page %>%
  html_nodes(".listdate a") %>%
  html_text() %>%
  str_trim()

df <- data.frame(links = links, titles = titles, stringsAsFactors = FALSE)

lapply(links,
       FUN = function(link) {
         filename <- "test.html" # change to something useful based on the EO
         absolute_link <- str_replace(link, "^\\.\\./", "http://www.presidency.ucsb.edu/")
         page <- html(absolute_link) 
         
         sink(filename)
         print(page)
         sink()
       }
)

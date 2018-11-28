#question: how to extract the node html as a character object?

rm(list = ls())

library(rvest)

page <- html("http://www.presidency.ucsb.edu/ws/index.php?pid=78516")

#extract html node which has EO text
textnode <- 
  page %>%
  html_nodes("span.displaytext") %>% 
  html_nodes("p")                     # within that span, extract individual paragraphs

class(textnode) #returns XMLNodeSet

result <- character()
for (node in textnode) {
  kids = XML::xmlChildren(node, addFinaliizer = FALSE)
  i = sapply(kids, inherits, "XMLInternalTextNode")
  if (any(i)) 
    result <- c(result, paste(unlist(lapply(kids[i], XML::xmlValue, ignoreComments, 
                               recursive = TRUE, encoding = "UTF-8")), collapse = " "))
}

result

#extract text
eoText <-
  textnode %>%
  html_text()

class(eoText) #returns character

# notice each sentence is now an element of the character vector
eoText

#word scrunching example
eoText[5]

#would like to extract the html directly so that html tags can be removed using
#str_replace_all(eoText, "<.*?>", " ") which will prevent the above problem

# textnode[[1]] #returns the html
# 
# #howver:
# eoText <- as.character(textnode[[1]]) #fails
# class(textnode[[1]]) #probably because returns multiple classes



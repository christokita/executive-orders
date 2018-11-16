rm(list = ls())

library(RCurl)
library(plyr)
library(dplyr)
library(stringr)
library(rvest)

##############################
# Create Download Function
##############################

downloadEOs <- function(year){
  #function to extract EOs from the archive at http://www.presidency.ucsb.edu/
  
  print(year)
  
  #pause to avoid overtaxing website
  #pick a random delay in case the server gets suspicious
  Sys.sleep(runif(1, min = 0.01, max = 0.05))
  
  page <- read_html(paste0("http://www.presidency.ucsb.edu/executive_orders.php?year=",
                      year,
                      "&Submit=DISPLAY"
                      )
  )

  #extract EO titles
  titles <-
    page %>%
    html_nodes(".listdate a") %>%
    html_text() %>%
    str_trim()
  
  #convert endashes to regular dashes
  titles <- str_replace_all(string = titles,
                            pattern = "\u2014", 
                            replacement = " - ")
  
  #extract EO links
  links <-
    page %>%
    html_nodes(".listdate a") %>%
    html_attr("href") %>%
    str_replace_all(fixed("../"), "/")

  df <- data.frame(title = titles, 
                   link  = links,
                   stringsAsFactors = FALSE)

  #remove unumbered orders
  to.remove <- paste0(c("Order-Further Designation Under Executive Order No. 12958",
                 "Order-Designation Under Executive Order 12958", 
                 "Annex to the Executive Order on Taking Additional Steps To Address the National Emergency With Respect to Significant Malicious Cyber-Enabled Activities"), collapse = "|")
  
  df <- df[!grepl(to.remove, df$title), ]
  
  #extract EO numbers - note that some "numbers" have letters appended
  df$num <- regmatches(df$title, 
                       regexpr("^Executive Order (No\\. )?[0-9]{4,5}-?[A-Z]?", 
                               df$title))
  
  #remove prefix
  df$num <- str_replace_all(string      = df$num, 
                            pattern     = "Executive Order (No\\. )?", 
                            replacement = "")
  
  df$year <- rep(year, nrow(df))
  
  #x <- c(df$title[23], df$link[23], df$num[23]) #for testing
  
  #for each year, dowload the html of each EO
  eos <- apply(df, 1, function(x){
    
    print(x[3])
    
    #pause to avoid overtaxing website
    #pick a random delay in case the server gets suspicious
    Sys.sleep(runif(1, min = 0.01, max = 0.05))
    
    url <- paste0("http://www.presidency.ucsb.edu", x[2])

#   #rvest method that doesn't quite work
#   #would like to extract HTML instead of text
#     page <- html(url)
#     eoText <- page %>%
#       html_nodes("span.displaytext") %>%
#       html_text()
    
    content <- getURL(url) #get webpage url
    
    #<br /> tags with adjacent newlines messes up the eoHtml regex below
    #in some cases so need to remove them
    content <- str_replace_all(string      = content, 
                               pattern     = "<br ?/> ?\r\n", 
                               replacement = " ")
    
    #split into lines
    content <- unlist(str_split(string  = content, 
                                pattern = "\n")) 
    
    #extract EO HTML
    eoHtml <- content[str_detect(string  = content,
                                 pattern = "<span class=\"displaytext\">.*</span>")]
    
    #this should only happen for 13446, and is fixed later
    if(length(eoHtml) == 0){eoHtml <- "Could not extract HTML."}
    
    out <- data.frame()
    try(out <- data.frame(num  = x[3], 
                          html = eoHtml, 
                          stringsAsFactors = FALSE))
    
    return(out)
    
  })
  
  eos2 <- ldply(eos)      #merge list of dataframes
  eos3 <- merge(eos2, df) #merge with EO titles
  
  return(eos3)
}

##############################
# Call Download Function
##############################

#years to scrape from website
#FDR starts in 1933, Truman starts in 1945
years    <- as.character(2015:2017)
eos.list <- lapply(years, downloadEOs) #download EOs
eos.new      <- ldply(eos.list) #merge lists

##############################
# Final Cleanup
##############################

eos.new$link <- paste0("http://www.presidency.ucsb.edu", eos.new$link)
eos.new <- select(eos.new, -.id)

sum(duplicated(eos.new$num))

#add HTML for one EO that does not download properly (13446)
missing <- filter(eos, grepl("Could not extract HTML", html))
missing.html <- scan(file = "data/eo13446.txt",
                     what = "character",
                     sep  = "\n")
missing.html <- paste(missing.html, collapse = " ")
eos[eos$num == "13446", ]$html <- missing.html

##############################
# Save Output
##############################


# #commented out to avoid accidental overwrite
# load("data/eosRaw.RData") #initial EOs data set of 1945 - 2015
# 
# eos <- unique(rbind(eos, eos.new)) #add in newly downloaded EOs
# 
# save(eos, file = "data/eosRaw.new.RData") 















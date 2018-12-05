rm(list = ls())

# Load packages, functions, and data
source("scripts/util/__Util_MASTER.R")

load("data/eosRaw.new.RData")

#remove a few FDR EOs that were scraped from the 1945 page
eos <- filter(eos, !(num %in% c("9511", "9523", 
                                "9524", "9531", 
                                "9533")))

#remove unnecessary html snippets
eos$html <- str_replace_all(string      = eos$html, 
                            pattern     = "</div></div><span class=\"displaytext\">", 
                            replacement = "") 

eos$html <- str_replace_all(string      = eos$html,
                            pattern     = "</span><hr noshade=\"noshade\" size=\"1\"><span class=\"displaynotes\"><i></i></span><hr noshade=\"noshade\" size=\"1\">", 
                            replacement = "") 

#remove non-breaking spaces
eos$html <- str_replace_all(string      = eos$html, 
                            pattern     = "&nbsp;", 
                            replacement = " ") 


#replace unicode apostrophes
eos$html <- str_replace_all(string      = eos$html, 
                            pattern     = "\u0092", 
                            replacement = "'")

#replace other unicode characters
eos$html <- iconv(x    = eos$html, 
                  from = "utf-8", 
                  to   = "ascii", 
                  sub  = " ")

#remove html tags
eos$text <- str_replace_all(string      = eos$html, 
                            pattern     = "<.*?>", 
                            replacement = " ") 


##############################
# Remove Unpublished EOs
##############################

#convert multiple spaces to single spaces
eos$text <- str_replace_all(string      = eos$text, 
                            pattern     = "\\s+", 
                            replacement = " ")

#rough word count
eos$word.count <- str_count(string  = eos$text, 
                            pattern = " ") + 1
summary(eos$word.count)

# Find and remove unpublished EOs
shortEOs <- filter(eos, eos$word.count < 10)
unpublishedEOs <- shortEOs$num
eos <- filter(eos, word.count > 10)

#convert endashes to regular dashes
eos$title <- str_replace_all(string = eos$title,
                          pattern = "\u2014", 
                          replacement = " - ")

#shorten titles
eos$title  <- str_replace_all(string      = eos$title,
                              pattern     = "^Executive Order (No\\. )?[0-9]{4,5}-?[A-Z]?\\s?-+\\s?", 
                              replacement = "")


##############################
# Determine Missing EOs
##############################

#note that this method will not catch missing EOs that have letters appended
expected   <- as.character(9538:13764)
missingEOs <- setdiff(expected, eos$num)
missingEOs <- setdiff(missingEOs, unpublishedEOs)
missingEOs <- as.data.frame(missingEOs)
names(missingEOs) <- "num2"


##############################
# Attach President Metadata
##############################

presRanges <- read.csv(file = "data/PresidentOrderRanges.csv", 
                       stringsAsFactors = FALSE)

presRanges$President <- paste0(presRanges$Number, "-", presRanges$President)

pres <- apply(presRanges, 1, function(x){
  
  start   <- x[2]
  end     <- x[3]
  eoRange <- start:end
  
  out <- data.frame(president = rep(x[1], length(eoRange)),
                    party     = rep(x[5], length(eoRange)),
                    num2      = eoRange, 
                    stringsAsFactors = FALSE)
})
pres <- do.call('rbind', pres)

#remove letters to make EO matching simpler
eos$num2 <- str_replace_all(string      = eos$num,
                            pattern     = "-?[A-Z]$",
                            replacement = "")

eos <- merge(x     = eos, 
             y     = pres, 
             by    = "num2", 
             all.x = TRUE)

eos <- select(eos, -num2)

missingEOs <- merge(x     = missingEOs, 
                    y     = pres,
                    by    = "num2",
                    all.x = TRUE)

#remove Trump EOs
eos <- eos[eos$president != "45-Trump", ]



##############################
# Add Manual Topic Tags
##############################

topicTable <- read.csv(file = "data/eoTopicTagCodebook.csv", 
                       stringsAsFactors = FALSE)

eoTopicTags <- read.csv(file = "data/eoTopicTags.csv", 
                        stringsAsFactors = FALSE)

names(eoTopicTags) <- c("id", "num", "president", "party", 
                        "beg.term", "end.term", "date", "year", 
                        "month", "day", "congress", "divided", 
                        "description", "topic.num", "topic.sub.num", "tn2", 
                        "tsubn2")

eoTopicTags <- eoTopicTags %>%
  select(-id, -topic.num, -president, -party,
         -year, -month, -day, -tn2, -tsubn2) %>%
  unique() %>%
  mutate(num = as.character(num)) %>%
  mutate(num = str_replace_all(string  = num,
                               pattern = c("\\.1" = "-A", 
                                           "\\.2" = "-B",
                                           "\\.3" = "-C")))

eoTopicTags <- merge(x = eoTopicTags, 
                     y = topicTable, 
                     all.x = TRUE)

eoTopicTags <- select(eoTopicTags, -beg.term, -end.term, -congress, 
                      -divided, -description)

eos <- merge(x = eos, 
             y = eoTopicTags, 
             all.x = TRUE) 

eos[is.na(eos$topic),       ]$topic       <- "Topic Not Yet Assigned"
eos[is.na(eos$topic.sub),   ]$topic.sub   <- "Subtopic Not Yet Assigned"
eos[is.na(eos$topic.broad), ]$topic.broad <- "Topic Not Yet Assigned"

##############################
# Save Output
##############################

save(eos, file = "data/eosCurated.RData")

write.csv(x    = missingEOs, 
          file = "output/missingExecutiveOrders.csv", 
          row.names = FALSE)


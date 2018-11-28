rm(list = ls())

source("scripts/util/__Util_MASTER.R")

load("data/eosCurated.RData")

eos <- select(eos, -html)

#combine title with body
eos$title.plus.text <- paste(eos$title, eos$text, sep = " : ")

#remove USC / CRF / FR references since these could be confused with EO numbers
eos$title.plus.text <- 
  str_replace_all(string      = eos$title.plus.text,
                  pattern     = paste("U\\.?S\\.?C\\.? [0-9]+(-| and )?([0-9]+)?",
                                       "C\\.?F\\.?R\\.? [0-9]+(-| and )?([0-9]+)?",
                                       "F\\.?R\\.? [0-9]+(-| and )?([0-9]+)?",
                                       sep = "|"), 
                  replacement = "")

#regex test
# str_replace_all(string      = "asdfs USC 123-3456 owefinl F.R. 554234 and 8327 ioiflam C.F.R 94189",
#                 pattern     = paste("U\\.?S\\.?C\\.? [0-9]+(-| and )?([0-9]+)?",
#                                     "C\\.?F\\.?R\\.? [0-9]+(-| and )?([0-9]+)?",
#                                     "F\\.?R\\.? [0-9]+(-| and )?([0-9]+)?",
#                                     sep = "|"), 
#                 replacement = "")

#make all text lowercase
eos$title.plus.text <- str_to_lower(eos$title.plus.text)

#replace ampersands
eos$title.plus.text <- str_replace_all(string      = eos$title.plus.text, 
                                       pattern     = " & ",
                                       replacement = " and ")

#pad punctuation with spaces
eos$title.plus.text <- str_replace_all(string      = eos$title.plus.text, 
                                       pattern     = "([:punct:])",
                                       replacement = " \\1 ")

eos$title.plus.text <- str_replace_all(string      = eos$title.plus.text, 
                                       pattern     = " ' ",
                                       replacement = "'")

eos$title.plus.text <- str_replace_all(string      = eos$title.plus.text, 
                                       pattern     = "\\( ([A-Z]+) \\)",
                                       replacement = "\\(\\1\\)")

#multi to single space
eos$title.plus.text <- str_replace_all(string      = eos$title.plus.text, 
                                       pattern     = "\\s+", 
                                       replacement = " ")

##############################
# Search for Mentions of Other EOs
##############################

#note that these EOs are not yet accounted for by the below regexes
(eos[grepl("[A-Z]", eos$num), ]$num)

df <- unique(select(eos, num, title.plus.text))

matches <- apply(df, 1, function(x){
  
  #remove letters appended to EOs to simplify matching process
  num <- str_replace_all(string      = x[1], 
                         pattern     = "-.*", 
                         replacement = "")
  
  #remove money - note that the $ doesn't get treated as punctuation
  text <- str_replace_all(string      = x[2],
                          pattern     = "\\$[0-9]+", 
                          replacement = "")
  
  #search for all 4-5 digit numbers
  m <- str_match_all(string  = text, 
                     pattern = regex("[0-9]{4,5}", 
                                     ignore_case = TRUE))
  
  m <- unique(unlist(m))
  
  #remove false matches
  matches <- m[as.numeric(m) < as.numeric(num) & #numbers higher than current EO num
                 as.numeric(m) >= 9538  & #Truman lower limit
                 as.numeric(m) <= 13701 & #Obama upper limit
                 !str_detect(string  = m, 
                             pattern = "^0")  & #numbers with leading zeroes
                 m != ""] #empty matches
  
  matches <- paste0(matches, collapse = "; ")
  
  return(matches)
  
})

eos$eoMatches.out <- matches

#remove selfmatches
eos$eoMatches.out <- str_replace_all(string      = eos$eoMatches.out,
                                     pattern     = paste0(eos$num, "(; )?"),
                                     replacement = "")


stopifnot(sum(grepl("NA", eos$eoMatches.out)) == 0)

# Split out eoMatches list into separate rows
eos2 <- eos %>%
  mutate(eoMatch = str_split(string  = eoMatches.out, 
                             pattern = "; ")) %>%
  unnest(eoMatch) %>% 
  mutate(eoMatch = str_trim(eoMatch))

eos2 <- eos2 %>%
  filter(eoMatch != "") %>% 
  select(num, eoMatch) %>%
  group_by(eoMatch) %>%
  summarise(eoMatches.in = paste0(num, collapse = "; ")) %>%
  as.data.frame()

names(eos2) <- c("num", "eoMatches.in")
eos <- merge(eos, eos2, all.x = TRUE) 
eos[is.na(eos$eoMatches.in), ]$eoMatches.in <- ""
rm(eos2)

##############################
# Search for Agencies & Concepts
##############################

#the queries.csv file contains five types of queries:
#1. science agency queries;
#2. non-science agency quries;
#3. science "concept" queries; and
#4. non-science "concept" queries;
#5. president name queries

#the querying takes place in a two step process:
#step 1: condensing related words into a single word
#step 2: searching for the new, shortened single words
#this is done for various reasons, including:
#1. to prevent the concept queries from overlapping agency queries
#(e.g. "scien" matching "Science" in "National Science Foundation), and
#2. to make it easier to compute the relative prevalence of the query within 
#each EO - i.e. the ratio of query word matches to total word count

queries <- read.csv(file = "data/queries.csv", 
                    stringsAsFactors = FALSE)

#step 1
for(i in 1:nrow(queries)){
  
  #print(queries$query.name[i])
  
  eos$title.plus.text <- 
    str_replace_all(string      = eos$title.plus.text, 
                    pattern     = regex(queries$query.long[i], 
                                        ignore_case = TRUE), 
                    replacement = paste(" ", queries$query.short[i], " "))
  
}

#remove president replacements
queries <- filter(queries, query.type != "President")

all.queries <- paste(queries$query.short, collapse = "|")
st.queries  <- paste(queries[queries$science.flag, ]$query.short, collapse = "|")

#step 2
matches <- str_match_all(string = eos$title.plus.text, 
                         pattern = regex(all.queries, 
                                         ignore_case = TRUE))

matches <- lapply(matches, FUN = unique)
matches <- lapply(matches, FUN = paste, collapse = "; ")
matches <- unlist(matches)

eos$matchWords <- matches

eos$st.query.topic <- str_detect(string  = eos$matchWords,
                                 pattern = st.queries)

##############################
# Compute Query Stats
##############################

library(reshape2)

eos$eoMatches.out.num <- str_count(string  = eos$eoMatches.out, 
                                   pattern = "; ") + 1
eos[eos$eoMatches.out == "", ]$eoMatches.out.num <- 0
summary(eos$eoMatches.out.num)

eos$eoMatches.in.num <- str_count(string  = eos$eoMatches.in, 
                              pattern = "; ") + 1
eos[eos$eoMatches.in == "", ]$eoMatches.in.num <- 0
summary(eos$eoMatches.in.num)

eos$eoMatches.all.num <- eos$eoMatches.out.num + eos$eoMatches.in.num

eos$matchWords.num <- str_count(string  = eos$matchWords, 
                                pattern = "; ") + 1
eos[eos$matchWords == "", ]$matchWords.num <- 0
summary(eos$matchWords.num)

eos$stMatch.count <- str_count(string  = eos$title.plus.text, 
                               pattern = st.queries)
summary(eos$stMatch.count)
eos$stMatchRatio <- eos$stMatch.count / eos$word.count
eos <- arrange(eos, desc(stMatchRatio))

##############################
# Add PCAST Flag
##############################

eos$pcast <- str_detect(string = eos$matchWords, pattern = "PCAST")

first.deg.out <- unlist(str_split(paste(eos[eos$pcast, ]$eoMatches.out, collapse = "; "), "; "))
first.deg.in <- unlist(str_split(paste(eos[eos$pcast, ]$eoMatches.in, collapse = "; "), "; "))
first.deg <- unique(c(first.deg.out, first.deg.in))
first.deg <- setdiff(first.deg, eos[eos$pcast, ]$num)
eos$pcast.first.deg <- eos$num %in% first.deg

second.deg.out <- unlist(str_split(paste(eos[eos$pcast.first.deg, ]$eoMatches.out, collapse = "; "), "; "))
second.deg.in <- unlist(str_split(paste(eos[eos$pcast.first.deg, ]$eoMatches.in, collapse = "; "), "; "))
second.deg <- unique(c(second.deg.out, second.deg.in))
second.deg <- setdiff(second.deg, first.deg)
second.deg <- setdiff(second.deg, eos[eos$pcast, ]$num)
eos$pcast.second.deg <- eos$num %in% second.deg

eos$ostp <- str_detect(string = eos$matchWords, pattern = "OSTP")

first.deg.out <- unlist(str_split(paste(eos[eos$ostp, ]$eoMatches.out, collapse = "; "), "; "))
first.deg.in <- unlist(str_split(paste(eos[eos$ostp, ]$eoMatches.in, collapse = "; "), "; "))
first.deg <- unique(c(first.deg.out, first.deg.in))
first.deg <- setdiff(first.deg, eos[eos$ostp, ]$num)
eos$ostp.first.deg <- eos$num %in% first.deg

second.deg.out <- unlist(str_split(paste(eos[eos$ostp.first.deg, ]$eoMatches.out, collapse = "; "), "; "))
second.deg.in <- unlist(str_split(paste(eos[eos$ostp.first.deg, ]$eoMatches.in, collapse = "; "), "; "))
second.deg <- unique(c(second.deg.out, second.deg.in))
second.deg <- setdiff(second.deg, first.deg)
second.deg <- setdiff(second.deg, eos[eos$ostp, ]$num)
eos$ostp.second.deg <- eos$num %in% second.deg

##############################
# Save Output
##############################

save(eos, file = "output/eosCuratedWithS&TQuery.RData")

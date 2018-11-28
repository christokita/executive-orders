##################################################################################################
#
# Topic model EOs
#
##################################################################################################
 
source("scripts/util/__Util_MASTER.R")

####################
# Load data
####################
load("data/eosCurated.RData")


####################
# Prep data
####################
# Get stopwords
stop_words <- paste(stopwords('en'), collapse = "\\b|\\b")
stop_words <- paste0('\\b', stop_words, '\\b')

# Process
eo_text <- eos %>% 
  select(num, text) %>% 
  mutate(text = gsub("\\b[0-9]+st\\b|\\b[0-9]+nd\\b|\\b[0-9]+th\\b", "", text)) %>% # remove 1st, 2nd, etc.
  mutate(text = gsub("\\([a-z]\\)", "", text)) %>% # remove (a), (b), etc.
  mutate(text = gsub("[^A-Za-z ]", "", text)) %>% # remove non-letters
  mutate(text = tolower(text)) %>% # make lowercase
  mutate(text = gsub(stop_words, "", text)) %>% # remove stopwords
  mutate(text = gsub("^[ ]+", "", text)) %>% # remove leading whitespace
  mutate(text = gsub("[ ]+$", "", text)) %>% # remove trailing whitespace
  mutate(text = gsub("[ ]+", " ", text)) %>%  # remove extra space
  mutate(text = stemDocument(text)) #stem words



####################
# Make document-term matrix (DTM)
####################
# Separate out words
eo_words <- eo_text %>% 
  unnest_tokens(word, text)

# Count words by document
word_counts <- eo_words %>% 
  count(num, word, sort = TRUE)

# Count words by document
eo_dtm <- word_counts %>% 
  cast_dtm(num, word, n)

####################
# Topic model
####################
eo_lda <- LDA(eo_dtm, k = 20, control = list(seed = 323))

####################
# Interpret model
####################
eo_topics <- tidy(eo_lda, matrix = "beta")
eo_top_terms <- eo_topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)


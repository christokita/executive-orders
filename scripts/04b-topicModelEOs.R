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
# make document-term matrix (DTM)
eo_dtm <- CreateDtm(doc_vec = eos$text, 
                    doc_names = eos$num, 
                    ngram_window = c(1, 2), 
                    stopword_vec = c(stopwords('en'), stopwords('smart')),
                    lower = TRUE,
                    remove_punctuation = TRUE,
                    remove_numbers = TRUE,
                    verbose = FALSE,
                    # stem_lemma_function = function(x) SnowballC::wordStem(x, "porter"),
                    cpus = 2)

# Basic corpus statistics
tf_mat <- TermDocFreq(eo_dtm) 
head(tf_mat[order(tf_mat$term_freq, decreasing = TRUE), ], 20) # see top words
tf_bigrams <- tf_mat[ stringr::str_detect(tf_mat$term, "_") , ]
head(tf_bigrams[ order(tf_bigrams$term_freq, decreasing = TRUE) , ], 20) # see top bigram words

####################
# Document clustering
####################
# Cosine similarity using inverse document frequency (IDF)
tfidf <- t(eo_dtm[, tf_mat$term]) * tf_mat$idf # reweight terms by IDF
tfidf <- t(tfidf)
csim <- tfidf / sqrt(rowSums(tfidf * tfidf)) # calculate cosine similiarity
csim <- csim %*% t(csim)
cdist <- as.dist(1 - csim) #make it distance
hc <- hclust(cdist, "ward.D")
clustering <- cutree(hc, 150)
plot(hc, main = "Hierarchical clustering of Executive Orders",
     ylab = "", xlab = "", yaxt = "n")
rect.hclust(hc, 150, border = "red")

# Cluster summary
p_words <- colSums(eo_dtm) / sum(eo_dtm)
cluster_words <- lapply(unique(clustering), function(x){
  rows <- eo_dtm[ clustering == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})
cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:10 ], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)

####################
# Topic model
####################
set.seed(323)
eo_lda <- FitLdaModel(dtm = eo_dtm, 
                      k = 80,
                      iterations = 500,
                      burnin = 180,
                      alpha = 0.1,
                      beta = 0.05, 
                      optimize_alpha = TRUE, 
                      calc_likelihood = TRUE, 
                      calc_coherence = TRUE, 
                      calc_r2 = TRUE, 
                      cpus = 2)



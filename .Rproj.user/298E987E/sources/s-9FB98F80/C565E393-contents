##################################################################################################
#
# Create document term matrix for executive orders
#
##################################################################################################

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


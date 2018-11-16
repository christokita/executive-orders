rm(list = ls())

library(stringr)
library(tm)
library(dplyr)
library(gmodels)
library(NbClust)
library(fpc)
library(cluster)

load("output/eosCuratedWithS&TQuery.RData")

#Short EOs tend to cluster together
# eos <- filter(eos, word.count > 200)

##############################
# Create DTMs
##############################

eo.corpus <- Corpus(VectorSource(eos$title.plus.text))
eo.corpus <- tm_map(eo.corpus, removeWords, stopwords("english"))
eo.corpus <- tm_map(eo.corpus, removePunctuation)
eo.corpus <- tm_map(eo.corpus, removeNumbers)
#eo.corpus <- tm_map(eo.corpus, stemDocument, lazy = TRUE)
eo.corpus <- tm_map(eo.corpus, stripWhitespace)
dtm <- DocumentTermMatrix(eo.corpus)
dtm <- removeSparseTerms(dtm, 0.999)
# findFreqTerms(dtm, 5000)

eos.st <- filter(eos, st.flag == TRUE)
eo.corpus.st <- Corpus(VectorSource(eos.st$title.plus.text))
eo.corpus.st <- tm_map(eo.corpus.st, removeWords, stopwords("english"))
eo.corpus.st <- tm_map(eo.corpus.st, removePunctuation)
eo.corpus.st <- tm_map(eo.corpus.st, removeNumbers)
#eo.corpus.st <- tm_map(eo.corpus.st, stemDocument)
eo.corpus.st <- tm_map(eo.corpus.st, stripWhitespace)
dtm.st <- DocumentTermMatrix(eo.corpus.st)
dtm.st <- removeSparseTerms(dtm.st, 0.999)

##############################
# Compute Clusters
##############################

compute_cluster_fit_indices <- function(df){
  
  # Indices to use for our analysis (removes computationally heavy indices)
  #   indexMethods <- c("frey", "mcclain", "dunn", "sdindex","sdbw","kl", "ch", "hartigan", "ccc", 
  #                     "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db"  
  #                     ,"silhouette", "duda","beale")
  indexMethods <- c("frey", "silhouette")
  
  # Create list for index values
  fit.indices <- list()
  
  # Loop through index types
  for (idx in indexMethods) {
    print(idx)
    fit.indices[[idx]] <- NbClust(df, diss = NULL, 
                                  distance = "euclidean", min.nc = 30, 
                                  max.nc = 60,  method = "kmeans", index = idx, alphaBeale = 0.1)
    print(fit.indices[[idx]]$Best.nc)
  }
  
  fit.indices  
}

# takes a long time to run...
# fit.indices <- compute_cluster_fit_indices(dtm2)

#alternate method to determine optimum cluster number
#takes a long time to run...

#determine optimum number of clusters using silohuette method
partitionAroundMedoids <- function(k, dtm){
  print(k)
  p <- pam(dtm, k)
  print(p$silinfo$avg.width) #the higher the number, the better
  return(p)
}

# ks <- c(100, 200, 300, 400, 500, 600, 700)
ks <- c(200, 400, 600, 800)

# p <- lapply(ks, partitionAroundMedoids, dtm = dtm)
p.st <- lapply(ks, partitionAroundMedoids, dtm = dtm.st)


#compute kmeans cluster
compute.cluster.analysis <- function(df, centers){
  cluster_analysis <- kmeans(df, centers, iter.max = 20, nstart = 1)
}

#still need to make the number of centers automatic based on silohuette results
cluster.analysis <- compute.cluster.analysis(dtm.st, 500)

df <- data.frame(num = eos.st$num, cluster = cluster.analysis$cluster)
hist(df$cluster, n = 100)

# #hierarchical clustering
# d <- dist(dtm.st, method="euclidian")
# fit <- hclust(d=d, method="ward")   
# fit   
# plot(fit, hang=-1)  

##############################
# Append Custers to EO Metadata
##############################

eos2 <- merge(eos.st, df, all = TRUE)
eos2 <- eos2 %>%
  select(-html, -text, -title.plus.text) %>%
  select(num, year, title, cluster, matchWords, stMatchRatio, 
         word.count, eoMatches.out, eoMatches.in, st.flag, link) %>% 
  arrange(cluster, num) %>%
  transform(matchWords = str_replace_all(matchWords, "__", "")) %>%
  write.csv(file = "output/st.eo.cluster.review.csv", 
            row.names = FALSE)

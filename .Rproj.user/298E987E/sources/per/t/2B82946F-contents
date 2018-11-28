##################################################################################################
#
#Topic Modeling Executive Orders
#
##################################################################################################

rm(list=ls())

source("scripts/000-utilityFunctionsEO.R")

# Load packages
required.packages <- c("dplyr",
                       "igraph", 
                       "devtools", #for installing ida TM pkgs
                       "idaTopicModels",
                       "idaMakeDtm",
                       "reshape2")
LoadPackages(required.packages = required.packages)

##################################################################################################
# Initial data pruning 
##################################################################################################

# Load Data
load("output/eosCuratedWithS&TQuery.RData")

# Remove matches column and duplicates
eos <- 
  eos %>%
  select(-eoMatches) %>% #drop matches column
  unique() #remove duplicates
  
# Get only S&T eos
STeos <- 
  eos %>% 
  filter(st.topic.flag == TRUE)

rm(eos)


##################################################################################################
# Checking characterisitings of dataset
##################################################################################################
summary(nchar(STeos$title.plus.text))


#Average character length of EOs
mean(nchar(STeos$title.plus.text))
median(nchar(STeos$title.plus.text))

hist(nchar(STeos$title.plus.text))


##################################################################################################
# Modeling
##################################################################################################


########## Model Prep ###########

# EO Text only for DTm production
eo_text <- STeos$title.plus.text
names(eo_text) <- STeos$num

# Create DTM
dtm <- Vec2Dtm(vec = eo_text, 
               min.n.gram = 1, 
               max.n.gram = 1, 
               remove.stopwords = TRUE)

dtm <- DepluralizeDtm(dtm = dtm)


# Remove infrequent words and overly-frequent (>50%) words
tf <- TermDocFreq(dtm = dtm)
keep_terms <- tf$term[ tf$doc.freq < nrow(dtm)/2 & tf$doc.freq > 5]
dtm <- dtm[, keep_terms]
summary(rowSums(dtm)) #check to make sure all documents have words

# Create lexical object
lex <- Dtm2Docs(dtm = dtm, parallel = TRUE, cpus = 6)
vocab <- colnames(dtm)
lex <- lexicalize(doclines = lex, sep = " ",
                  lower = TRUE,
                  vocab = vocab) 


########## Model Build ###########

k_list <- seq(10, 170, by = 10)
names(k_list) <- paste("k", k_list, sep = "_")

# Construct models
sfInit(parallel = TRUE, cpus = 8)
sfExport(list = c("vocab", "lex"))
sfLibrary(idaTopicModels)

models <- sfLapply(k_list, fun = function(k){ 
  lda.collapsed.gibbs.sampler(documents = lex,
                              K = k,
                              vocab = vocab,
                              num.iterations = 5000,
                              alpha = 0.1, 
                              eta = 0.05, 
                              compute.log.likelihood = TRUE) 
})
#num.iterations: for play 500, for research 2,000-20,000 

sfStop()


########## Explore Models ###########

names(models)

models <- lapply(models, function(M){
  FormatRawLdaOutput(lda.result = M, docnames = rownames(dtm), smooth = TRUE)
})
  

# Calculate statstics on models
models <- lapply(models, function(M){
  M$coherence <- apply(M$phi, 1, function(x) ProbCoherence(topic = x, dtm = dtm, M = 6))
  M$prevalence <- colSums(M$theta) / sum(colSums(M$theta)) * 100
  M$r2 <- TopicModelR2(dtm = dtm, phi = M$phi, theta= M$theta, parallel = FALSE) #faster to use parallel for greater than 5-10 thousand documents
  M$ll <- CalcLikelihood(dtm = dtm, phi = M$phi, theta = M$theta, parallel = FALSE)
  return(M)
})

metric_mat <- lapply(models, function(M){
  data.frame(k = ncol(M$theta), 
             ll1 = M$likelihood[1, ncol(M$likelihood)], 
             ll2 = M$likelihood[2, ncol(M$likelihood)], 
             ll3 = M$ll, 
             coherence = mean(M$coherence), 
             r2 = M$r2$r2, 
             stringsAsFactors = FALSE)
})

metric_mat <- do.call(rbind, metric_mat)
metric_mat <- metric_mat[ order(metric_mat$k), ]

# Plot metric_mat against different parameters
plot(metric_mat$k, metric_mat$ll1, type = "o") # most useful ll for now

plot(metric_mat$k, metric_mat$ll2, type = "o")

plot(metric_mat$k, metric_mat$ll3, type = "o")

plot(metric_mat$k, metric_mat$coherence, type = "o")#always gets right number of topics in simulated data

plot(metric_mat$k, metric_mat$r2, type = "o")


# Save models
save(models, metric_mat, dtm, file = "Topic Modeling/data/ST_models.Rdata")

########## Select Final Model ###########

# Select final model
final_model <- models$k_90


# Get stuff out of final model
final_model$phi_prime <- GetPhiPrime(phi = final_model$phi, theta = final_model$theta)

final_model$top_terms_raw <- GetTopTerms(phi = final_model$phi, M = 10)

final_model$top_terms_prime <- GetTopTerms(phi = final_model$phi_prime, M = 10)


# Create output
final_model$summary <- data.frame(Topic = rownames(final_model$phi), 
                                  Coherence = final_model$coherence, 
                                  Prevalence = final_model$prevalence,
                                  TT_raw = apply(final_model$top_terms_raw, 2, function(x) paste(x, collapse = ", ")),
                                  TT_prime = apply(final_model$top_terms_prime, 2, function(x) paste(x, collapse = ", ")),
                                  stringsAsFactors = FALSE)
View(final_model$summary)

summary <- final_model$summary

write.table(summary,
            file = "Topic Modeling/output/ST_Topic_k90_Summary.csv", 
            sep = ",", 
            col.names = TRUE,
            row.names = FALSE)

####################################
# Document Topic dataframe
####################################

final_model$assignments <- t(apply(final_model$theta, 1, function(x) {
  x[ x < 0.05 ] <- 0
  x / sum(x)
}))

doc.topic <- final_model$assignments

View(doc.topic)

write.table(doc.topic,
            file = "Topic Modeling/output/EO_Topic_k80_DocTopMatrix.csv", 
            sep = ",", 
            col.names = NA,
            row.names = TRUE)

# Save model

save(final_model, doc.topic, summary, file = "Topic Modeling/output/k90_ST_model.RData")

####################################
# Create top 10 EO set for each topic
####################################
topEo <- apply(doc.topic, 2, function(x) {
  temp <- 
    x %>% 
    sort(x, decreasing = TRUE)
  temp <- names(temp)[1:10]
  
  if(!exists("topEo")) {
    topEo <- temp
  } else {
    topEo <- cbind(topEo, temp)
  }
})

# Transpose dataframe
topEo <- t(topEo)

# Melt dataframe
topEo <- as.data.frame(topEo,
                       stringsAsFactors = FALSE)
topEo$Topic <- row.names(topEo)
topEomelt <- melt(data = topEo, id.vars = "Topic")

# Append title and html link
load("output/eosCuratedWithS&TQuery.RData")
topEomelt <- merge(topEomelt, 
                   eos[, c("num", "title", "link")],
                   by.x = "value",
                   by.y = "num")
topEomelt <- 
  topEomelt %>%
  select(-variable)
names(topEomelt) <- c("num", "Topic", "title", "link")

# Append top terms
topEomelt <- merge(topEomelt,
                   summary[ , c("Topic", "TT_raw", "TT_prime")],
                   by = "Topic")

# Append topic value
doctopicmelt <- as.data.frame(doc.topic, 
                              stringsAsFactors = FALSE)
doctopicmelt$num <- rownames(doc.topic)
doctopicmelt <- melt(data = doctopicmelt, id.vars = "num")
topEomelt <- merge(topEomelt, 
                   doctopicmelt,
                   by.x = c("num", "Topic"),
                   by.y = c("num", "variable"))

# Append number of documents with that topic
doccount <- data.frame(Topic = colnames(doc.topic),
                       DocCount = NA,
                       stringsAsFactors = FALSE)

for (i in 1:nrow(doccount)) {
  
  topic <- doccount$Topic[i]
  topCol <- doc.topic[ , topic]
  topCol[topCol != 0] <- 1
  topSum <- sum(topCol)
  
  doccount$DocCount[i] <- topSum
  
}

topEomelt <- merge(topEomelt, 
                   doccount,
                   by = "Topic")

topEomelt <-
  topEomelt %>% 
  arrange(Topic, desc(value))
  

# Write
write.csv(topEomelt, file = "Topic Modeling/output/k90_ST_TopDocs.csv", row.names = FALSE)

####################################
# Output S&T topic EOs
####################################
# Select S&T EO
STtopic <- "t.33"

# Grab that topic's documents
docTopST <- doc.topic[ , colnames(doc.topic) == STtopic]
docTopST <- docTopST[docTopST > 0]

# Get these documents form EO set
STeoSet <- 
  eos %>% 
  filter(num %in% names(docTopST)) %>% 
  select(-html, -text, -title.plus.text, -eoMatches.num)

# Merge into dataframe for output
docTopST <- as.data.frame(docTopST, stringsAsFactors = FALSE)
names(docTopST) <- "PropSTtopic"
docTopST$num <- row.names(docTopST)
STeoSet <- merge(STeoSet, docTopST, by = "num")

STeoSet <-
  STeoSet %>% 
  arrange(desc(PropSTtopic))

write.csv(STeoSet, file = "Topic Modeling/output/ST_EO_DocSet.csv", row.names = FALSE)




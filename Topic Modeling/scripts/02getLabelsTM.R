rm(list=ls())

library(idaTopicModels)

load("Topic Modeling/data/ST_models.Rdata")
load("Topic Modeling/output/k90_ST_model.RData")


##################################################################
# Get Topic Labels
##################################################################
# Grab most probable topic for each document
labels <- apply(final_model$theta, 1, function(x) as.numeric(x == max(x)) )

# Establish label stopwords
stopWords <- c("sec", "stat", "year", "deg", "june", "export", "frac", "feet")

# consider removing labels that appear multiple times OR resolve manually
multiAppearance <- c("agency", "board", "chairman", "commission", "committee", "contract", 
                     "council", "department", "director", "dispute", "emergency", "employee", 
                     "feet", "foreign", "function", "information", "land", "line", 
                     "office", "organization", "person", "plan", "policy", "program", 
                     "property", "registrant", "retirement", "schedule", "service")
# Put together stopword list
stopWords <- c(stopWords, multiAppearance)
stopWords <- unique(stopWords)
stopWords <- paste0(stopWords, collapse = "|")



sfInit(parallel = T, cpus = 8)
sfExport("dtm", "stopWords")
sfLibrary(idaTopicModels)

# # For bigrams+ only
# final_model$labels <- sfApply(labels, 1, function(x){
#   result <- GetProbableTerms(docnames = names(x)[ x == 1 ], dtm = dtm[ , grepl("_", colnames(dtm)) ])
#   return(names(result)[ order(result, decreasing=T) ][ 1 ])
# })

final_model$labels <- sfApply(labels, 1, function(x){
  result <- GetProbableTerms(docnames = names(x)[ x == 1 ], dtm = dtm[ , !grepl(stopWords, colnames(dtm))])
  return(names(result)[ order(result, decreasing=T) ][ 1 ])
})

sfStop()


##################################################################
# create a summary table & write to output
##################################################################
final_model$summary <- data.frame(Id = rownames(final_model$phi),
                                  topic = rownames(final_model$phi),
                                  Label = final_model$labels,
                                  tt_prime = apply(final_model$top_terms_prime, 2, function(x) paste(x, collapse=", ")), 
                                  tt_raw = apply(final_model$top_terms_raw, 2, function(x) paste(x, collapse=", ")),
                                  coherence = round(final_model$coherence, 2),
                                  prevalence = final_model$prevalence,
                                  stringsAsFactors = FALSE)

summary <- final_model$summary

write.table(summary, file = "Topic Modeling/output/ST_Topic_k90_Summary.csv", 
            sep = ",", 
            row.names = FALSE)
 
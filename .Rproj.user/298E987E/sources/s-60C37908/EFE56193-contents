##################################################################################################
#
# Analyze Topic Models of EOs
#
##################################################################################################

source("scripts/util/__Util_MASTER.R")

####################
# Load data
####################
load("data/eosCurated.RData")


####################
# Interpret model
####################
plot(eo_lda$log_likelihood, type = "l")

# Get top terms and label topics
eo_lda$top_terms <- GetTopTerms(phi = eo_lda$phi, M = 10)
eo_lda$labels <- LabelTopics(assignments = eo_lda$theta > 5, 
                             dtm = eo_dtm,
                             M = 3)

# Summarise 
eo_lda$prevalence <- colSums(eo_lda$theta) / sum(eo_lda$theta) * 100
eo_lda$summary <- data.frame(topic = rownames(eo_lda$phi),
                             label = eo_lda$labels,
                             coherence = round(eo_lda$coherence, 3),
                             prevalence = round(eo_lda$prevalence,3),
                             top_terms = apply(eo_lda$top_terms, 2, function(x){
                               paste(x, collapse = ", ")
                             }),
                             stringsAsFactors = FALSE)
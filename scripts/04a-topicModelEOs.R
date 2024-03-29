##################################################################################################
#
# Topic model EOs
#
##################################################################################################
rm(list =ls())
source("scripts/util/__Util_MASTER.R")
library(parallel)
library(snowfall)

####################
# Load data
####################
load("/scratch/gpfs/ctokita/ExecutiveOrders/data/eosCurated.RData")


####################
# Prep data
####################
# make document-term matrix (DTM)
eo_dtm <- CreateDtm(doc_vec = eos$text, 
                    doc_names = eos$num, 
                    ngram_window = c(1, 1), 
                    stopword_vec = c(stopwords::stopwords('en'), stopwords::stopwords(source = 'smart')),
                    lower = TRUE,
                    remove_punctuation = TRUE,
                    remove_numbers = TRUE,
                    verbose = FALSE,
                    # stem_lemma_function = function(x) SnowballC::wordStem(x, "porter"),
                    cpus = 2)

# Remove overly frequent words
tf <- TermDocFreq(dtm = eo_dtm)
keep_terms <- tf$term[ tf$doc_freq <= nrow(eo_dtm)/2]
eo_dtm <- eo_dtm[, keep_terms]
summary(rowSums(eo_dtm)) #check to make sure all documents have words


# Save DTM
save(eo_dtm, file = "/scratch/gpfs/ctokita/ExecutiveOrders/dtms/eo_dtm_single.Rdata")


####################
# Topic model
####################
# Set topic numbers to test
Ks <- seq(20, 110, 5)

# Prepare for parallel
no_cores <- detectCores()
sfInit(parallel = TRUE, cpus = no_cores)
sfExportAll()
sfLibrary(dplyr)
sfLibrary(tidyr)
sfLibrary(stringr)
sfLibrary(textmineR)
sfLibrary(tm)
sfClusterSetupRNGstream(seed = 323)

parallel_ldas <- sfLapply(Ks, function(k) {
  # Run LDA model
  eo_lda <- FitLdaModel(dtm = eo_dtm, 
                        k = k,
                        iterations = 2000,
                        burnin = 200,
                        alpha = 50/k, # per Griffiths & Steyvers (2004)
                        beta = 0.1, # per Griffiths & Steyvers (2004)
                        optimize_alpha = TRUE, 
                        calc_likelihood = TRUE, 
                        calc_coherence = FALSE, 
                        calc_r2 = TRUE, 
                        cpus = 1)
  eo_lda$coherence_5 <- CalcProbCoherence(phi = eo_lda$phi, dtm = eo_dtm, M = 5)
  eo_lda$coherence_10 <- CalcProbCoherence(phi = eo_lda$phi, dtm = eo_dtm, M = 10)
  # Save
  file_name <- paste0("eo_lda_k", k)
  save(eo_lda, file = paste0("/scratch/gpfs/ctokita/ExecutiveOrders/lda_models/", file_name, ".Rdata"))
})

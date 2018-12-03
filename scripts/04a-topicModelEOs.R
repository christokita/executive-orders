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
# tf_mat <- TermDocFreq(eo_dtm) 
# head(tf_mat[order(tf_mat$term_freq, decreasing = TRUE), ], 20) # see top words
# tf_bigrams <- tf_mat[ stringr::str_detect(tf_mat$term, "_") , ]
# head(tf_bigrams[ order(tf_bigrams$term_freq, decreasing = TRUE) , ], 20) # see top bigram words

# Save DTM
save(eo_dtm, file = "/scratch/gpfs/ctokita/ExecutiveOrders/dtms/eo_dtm.Rdata")


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
                        iterations = 500,
                        burnin = 150,
                        alpha = 0.1,
                        beta = 0.05, 
                        optimize_alpha = TRUE, 
                        calc_likelihood = TRUE, 
                        calc_coherence = TRUE, 
                        calc_r2 = TRUE, 
                        cpus = 1)
  
  # Save
  file_name <- paste0("eo_lda_k", k)
  save(eo_lda, file = paste0("/scratch/gpfs/ctokita/ExecutiveOrders/lda_models/", file_name, ".Rdata"))
})

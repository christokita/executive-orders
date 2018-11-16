##################################################################################################
#
# Add ST Topic flag
#
##################################################################################################

rm(list = ls())
source("scripts/000-utilityFunctionsEO.R")
LoadPackages(required.packages = "dplyr")

#################################
# Load topic model spreadsheet and doc.topic matrix
#################################
# Now only using science topic for flagging EOs
# topick80 <- read.csv("Topic Modeling/output/EO_Topic_k80_SummaryWFlag.csv", 
#                      header = TRUE, 
#                      stringsAsFactors = FALSE)

load("Topic Modeling/output/k80_EO_model.RData")

#################################
# Load EO dataset
#################################
load("output/eosCuratedWithS&TQuery.RData")

#################################
# Get EOs that are non-zero in ST topic
#################################
STeos <- which(doc.topic[, 33] > 0)
STeos <- names(STeos)

#################################
# Flag ST eos
#################################
eos$st.tm.topic <- eos$num %in% STeos

#################################
# Save
#################################
save(eos, file = "output/eosCuratedWithS&TQuery.RData")


##################################################################################################
#
# Document network via topics
#
##################################################################################################

rm(list = ls())

library(igraph)
library(dplyr)

load("Topic Modeling/output/k80_EO_model.RData")

##################################################################################################
# Create Matrix 
##################################################################################################

DocToDoc <- doc.topic %*% t(doc.topic)


##################################################################################################
# Create graph object and edgelist
##################################################################################################

graph <- graph.adjacency(adjmatrix = DocToDoc, mode = "undirected", weighted = TRUE, diag = FALSE)
edgelist <- get.edgelist(graph)
edgelist <- cbind(edgelist, E(graph)$weight)
colnames(edgelist) <- c("Source", "Target", "Weight")

write.csv(edgelist, file = "Topic Modeling/output/EO_k80_doc_edgelist.csv", row.names = FALSE)



##################################################################################################
# Create nodelist
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
  filter(st.flag == TRUE) %>% 
  select(num, title, year, word.count, president, party, st.flag, eoMatches.num, matchWords.num, stMatch.count, stMatchRatio) %>% 
  mutate(Id = num) %>% 
  mutate(Label = num)

# Write
write.csv(STeos, file = "Topic Modeling/output/EO_k80_doc_nodelist.csv")




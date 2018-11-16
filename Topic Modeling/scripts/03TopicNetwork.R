##################################################################################################
#
# Topic network creation for gephi
#
##################################################################################################


rm(list = ls())

library(igraph)

load("Topic Modeling/output/k80_EO_model.RData")


##################################################################################################
# Create Matrix 
##################################################################################################

TopicToTopic <- t(doc.topic) %*% doc.topic


##################################################################################################
# Create graph object and edgelist
##################################################################################################

graph <- graph.adjacency(adjmatrix = TopicToTopic, mode = "undirected", weighted = TRUE, diag = FALSE)
edgelist <- get.edgelist(graph)
edgelist <- cbind(edgelist, E(graph)$weight)
colnames(edgelist) <- c("Source", "Target", "Weight")

#Reweight edges
edgelist <- as.data.frame(edgelist, stringsAsFactors = FALSE)
edgelist$Weight <- as.numeric(edgelist$Weight)
edgelist$Weight <- edgelist$Weight / max(edgelist$Weight)

# Write
write.csv(edgelist, file = "Topic Modeling/output/EO_k80_edgelist.csv", row.names = FALSE)




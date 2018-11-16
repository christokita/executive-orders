##################################################################################################
#
# Analyze EO network 
#
##################################################################################################

rm(list = ls())
source("scripts/000-utilityFunctionsEO.R")

# Load packages
required.packages <- c("dplyr",
                       "stringr",
                       "igraph")
LoadPackages(required.packages = required.packages)


#################################
# Create initial graph from edgelist
#################################
# Read in edgelist and nodetable
eoEdgelist <- read.csv("output/EO-edgelist.csv", 
                       header = TRUE, 
                       colClasses = c("character", "character"))
eoEdgelist <- as.matrix(eoEdgelist)

eoNodelist <- read.csv("output/EO-nodetable.csv", 
                       header = TRUE, 
                       as.is = TRUE)

# Edgelist to graph object
eoNetwork <- graph.edgelist(eoEdgelist, directed = TRUE)


#################################
# Add nodes that had no edges
#################################
# Get vertices already in graph
verts <- as.character(V(eoNetwork)$name)

# Get vertices to add
vertsToAdd <- eoNodelist$num[!eoNodelist$num %in% verts]

# Add to graph
eoNetworkFull <- eoNetwork + vertsToAdd


#################################
# Calculate network statistics 
#################################
# Create network stats dataframe
networkStats <- data.frame(EOnumber = eoNodelist$num,
                           ST.flag  = eoNodelist$st.tm.topic,
                           topic    = eoNodelist$topic.broad,
                           stringsAsFactors = FALSE)

# Calculate Total degree
deg <- degree(graph = eoNetworkFull, mode = c("total"))
networkStats$TotalDegree <- deg[match(networkStats$EOnumber, names(deg))]

# Calculate in-degree (referencing by other EOs)
deg <- degree(graph = eoNetworkFull, mode = c("in"))
networkStats$InDegree <- deg[match(networkStats$EOnumber, names(deg))]

# Calculate out-degree (referencing to other EOs)
deg <- degree(graph = eoNetworkFull, mode = c("out"))
networkStats$OutDegree <- deg[match(networkStats$EOnumber, names(deg))]

# Calculate betweeness centrality
cent <- betweenness(graph = eoNetworkFull, directed = TRUE)
networkStats$BetwCentrality <- cent[match(networkStats$EOnumber, names(cent))] / max(cent) #normalize

# Calculate eigenvector centrality
egcent <- evcent(graph = eoNetworkFull, directed = TRUE)
networkStats$EigCentrality <- egcent$vector[match(networkStats$EOnumber, names(egcent$vector))]

#################################
# Calculate category-specific average stats
#################################
topic.means <- networkStats %>%
  group_by(topic) %>%
  summarize(total.deg.mean = mean(TotalDegree),
            in.deg.mean    = mean(InDegree),
            out.deg.mean   = mean(OutDegree),
            bet.cent.mean  = mean(BetwCentrality))


#################################
# Calculate S&T vs non-S&T average stats
#################################
STnetworkStats <- data.frame(EOtype           = c("S&T", "non-S&T", "AllEOs"),
                             TotalDegree      = NA,
                             InDegree         = NA,
                             OutDegree        = NA,
                             BetwCentrality   = NA,
                             EigCentrality    = NA,
                             stringsAsFactors = FALSE)

# Calculate average total degree
STnetworkStats$TotalDegree[1] <- mean(networkStats$TotalDegree[networkStats$ST.flag == TRUE])
STnetworkStats$TotalDegree[2] <- mean(networkStats$TotalDegree[networkStats$ST.flag == FALSE])
STnetworkStats$TotalDegree[3] <- mean(networkStats$TotalDegree)

# Calculate average in-degree
STnetworkStats$InDegree[1] <- mean(networkStats$InDegree[networkStats$ST.flag == TRUE])
STnetworkStats$InDegree[2] <- mean(networkStats$InDegree[networkStats$ST.flag == FALSE])
STnetworkStats$InDegree[3] <- mean(networkStats$InDegree)

# Calculate average out-degree
STnetworkStats$OutDegree[1] <- mean(networkStats$OutDegree[networkStats$ST.flag == TRUE])
STnetworkStats$OutDegree[2] <- mean(networkStats$OutDegree[networkStats$ST.flag == FALSE])
STnetworkStats$OutDegree[3] <- mean(networkStats$OutDegree)

# Calculate average normalized betweeness centrality
STnetworkStats$BetwCentrality[1] <- mean(networkStats$BetwCentrality[networkStats$ST.flag == TRUE])
STnetworkStats$BetwCentrality[2] <- mean(networkStats$BetwCentrality[networkStats$ST.flag == FALSE])
STnetworkStats$BetwCentrality[3] <- mean(networkStats$BetwCentrality)

# Calculate average eigenvector centrality
STnetworkStats$EigCentrality[1] <- mean(networkStats$EigCentrality[networkStats$ST.flag == TRUE])
STnetworkStats$EigCentrality[2] <- mean(networkStats$EigCentrality[networkStats$ST.flag == FALSE])
STnetworkStats$EigCentrality[3] <- mean(networkStats$EigCentrality)


#################################
# Save/write dataframes
#################################
save(networkStats, file = "output/EOnetworkStats.RData")
write.csv(STnetworkStats, 
          file = "output/NetworkStatsTable.csv",
          row.names = FALSE)


# networkStats$ST.flag <- FALSE
# networkStats[networkStats$topic == "Government Operations", ]$ST.flag <- TRUE

# Make ST flag a factor
networkStats$ST.flag <- as.factor(networkStats$ST.flag)

#################################
# Calculate statistical significance with Mann-Whitney U Test
#################################
# Total degree
wilcox.test(TotalDegree ~ ST.flag, data = networkStats) #p-value = 0.111

# In degree
wilcox.test(InDegree ~ ST.flag, data = networkStats) #p-value = 0.149

# Out degree
wilcox.test(OutDegree ~ ST.flag, data = networkStats) #p-value = 0.316

# Betweeness Centrality
wilcox.test(BetwCentrality ~ ST.flag, data = networkStats) #p-value = 0.111


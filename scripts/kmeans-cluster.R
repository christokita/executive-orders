
rm(list = ls())
library(gmodels)
library("NbClust")
library(stringr)
library(dplyr)
library(fpc)

## Load final data used for for matching. CT: Load data frame here
df.merge.final <-
  df.merge.final %>% 
  select(t.1:t.90)

compute_cluster_fit_indices <- function(df){
  
#####  COMPLETE list of fit indices ####
  # indexMethods <- c("kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", , "ratkowsky", "ball", "ptbiserial",  "frey", "mcclain", "dunn", "hubert", "sdindex", "dindex", "sdbw", "gap", "gamma", "gplus", "tau")
#####  
  
  indexMethods <- c("frey", "mcclain", "dunn", "sdindex","sdbw","kl", "ch", "hartigan", "ccc", 
                    "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db"  
                    ,"silhouette", "duda","beale")
  
  fit.indices <- list()
  
  for (idx in indexMethods) {
    print(idx)
    fit.indices[[idx]] <- NbClust(df, diss = NULL, 
                                  distance = "euclidean", min.nc = 2, 
                                  max.nc = 30,  method = "kmeans", index = idx, alphaBeale = 0.1)
    
    print(fit.indices[[idx]]$Best.nc)
  }

  fit.indices  
}
 
fit.indices <- compute_cluster_fit_indices(df.merge.final)


compute.cluster.analysis <- function(df, centers){
  cluster_analysis <- kmeans(df, centers)
}


cluster.analysis <- compute.cluster.analysis(df.cluster, centers)
plotcluster(df.cluster[42:129], cluster.analysis$cluster)






 
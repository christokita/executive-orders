##################################################################################################
#
# General Utility Functions for EOs
#
##################################################################################################


######################
# Load/Install Packages
######################
LoadPackages <- function(required.packages) {
  
  for(pkg in required.packages){
    
    if(!require(pkg, character.only = TRUE)){
      
      if(pkg %in% c("idaTopicModels", "idaMakeDtm")) {
        
        LoadPackages(c("snowfall", "SnowballC", "tm", "RWeka"))
        
        path <- paste0("Topic Modeling/packages/", pkg, "/")
        install(path, dependencies = TRUE)
        library(pkg, character.only = TRUE)
        
      } else {
        
        install.packages(pkg, dependencies = TRUE)
        library(pkg, character.only = TRUE)
                
      }
      
    }
    
  }
  
}
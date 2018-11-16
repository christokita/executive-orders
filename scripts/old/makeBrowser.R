rm(list = ls())

load("output/eoMatchesCurated.RData")

basePath <- "file:///X:/___STPI_Data/ExecutiveOrders/ExecutiveOrderBrowser/" #directory in which to produce the file tree

eoTable <- eos2

#remove characters that might not be permitted in file names
eoTable$Source.Label <- gsub("[^a-zA-Z0-9]", " ", eoTable$Source.Label)
eoTable$Target.Label <- gsub("[^a-zA-Z0-9]", " ", eoTable$Target.Label)

#shorten titles
eoTable$Source.Label <- substr(eoTable$Source.Label, 1, 150)
eoTable$Target.Label <- substr(eoTable$Target.Label, 1, 150)


#insert hyperlinks to other executive orders in the html of each executive order
nt <- subset(eoTable, select=c(Source, Source.Label, Source.Year, OSTP.Mentioned))
nt <- unique(nt)

for(i in 1:nrow(nt)){
  print(i)
  
  if(nt[i, ]$OSTP.Mentioned){
    replacement <- paste0("<a href=\"", basePath, "OSTP/\\1 - ", nt[i, ]$Source.Label, " (", nt[i, ]$Source.Year, ")\\.html\">\\1</a>")
  }else{
    replacement <- paste0("<a href=\"", basePath, "Non-OSTP/\\1 - ", nt[i, ]$Source.Label, " (", nt[i, ]$Source.Year, ")\\.html\">\\1</a>")
  }
  
  eoTable$Source.Html <- gsub(paste0("(", nt[i, ]$Source, ")"), replacement, eoTable$Source.Html, perl=T)
}


eoTable <- eoTable[with(eoTable, order(Source, decreasing=T)), ]
eoTable$file.path <- ""

#generate the html files, binning them into two folders: ones that mention OSTP and ones that do not 
for(i in 1:nrow(eoTable)){
  
  num <- eoTable[i, ]$Source
  
  if(eoTable[i, ]$OSTP.Mentioned){
    title <- paste0(basePath, "OSTP/", num, " - ", eoTable[i, ]$Source.Label, " (", eoTable[i, ]$Source.Year, ")")
  } else{
    title <- paste0(basePath, "Non-OSTP/", num, " - ", eoTable[i, ]$Source.Label, " (", eoTable[i, ]$Source.Year, ")")
  }
  
  eoTable[i, ]$file.path <- title

  sink(file=paste0(title, ".html"))
  cat(eoTable[i, ]$Source.Html, sep="\n")
  sink()
  
}
  

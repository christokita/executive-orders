#' @title Get term frequencies and document frequencies of a corpus represented by a document term matrix.
#' @description This function takes a document term matrix as input and returns a data frame with columns for
#' term frequency, document frequency, and inverse-document frequency
#' @param dtm A document term matrix of class Matrix
#' @export
#' @examples
#' myvec <- c("the quick brown fox eats chickens", "the slow gray fox eats the slow chicken", "look at my horse", "my horses are amazing")
#' names(myvec) <- paste("doc", 1:length(myvec), sep="_")
#' 
#' dtm <- Vec2Dtm(vec = myvec, min.n.gram = 1, max.n.gram = 1)
#' 
#' colnames(dtm)
#' [1] "amazing"  "brown"    "chicken"  "chickens" "eats"     "fox"      "gray"     "horse"    "horses"   "quick"    "slow"   
#' 
#' dtm
#' 4 x 11 sparse Matrix of class "dgCMatrix"
#'    [[ suppressing 11 column names ‘amazing’, ‘brown’, ‘chicken’ ... ]]
#'                            
#' doc_1 . 1 . 1 1 1 . . . 1 .
#' doc_2 . . 1 . 1 1 1 . . . 2
#' doc_3 . . . . . . . 1 . . .
#' doc_4 1 . . . . . . . 1 . .
#' TermDocFreq(dtm)
#'        term term.freq doc.freq       idf
#' 1   amazing         1        1 1.3862944
#' 2     brown         1        1 1.3862944
#' 3   chicken         1        1 1.3862944
#' 4  chickens         1        1 1.3862944
#' 5      eats         2        2 0.6931472
#' 6       fox         2        2 0.6931472
#' 7      gray         1        1 1.3862944
#' 8     horse         1        1 1.3862944
#' 9    horses         1        1 1.3862944
#' 10    quick         1        1 1.3862944
#' 11     slow         2        1 1.3862944
#' 

TermDocFreq <- function(dtm) {
    message("Constructing tdf data.frame...")
	freq.mat <- data.frame(term=colnames(dtm), term.freq=Matrix::colSums(dtm), doc.freq=Matrix::colSums(dtm > 0), stringsAsFactors=FALSE)
	freq.mat$idf <- log(nrow(dtm) / freq.mat$doc.freq)
	return(freq.mat)
}

# This file was generated by Rcpp::compileAttributes
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Calculate the log likelihood of a DTM given a model using sequential C++ code
#' 
#' @param dtm A document term matrix
#' @param phi = a topics X terms dimensional matrix where each entry is p(term|topic)
#' @param theta = a documents X topics dimensional matrix where each entry is p(topic|document)//' @export
#' @description This function calculates the log likelihood for a topic model. Generally, this function (CalcLikelihoodC)
#' is called by the R function CalcLikelihood, which has options for paralellization. If you call CalcLikelihood but set
#' \code{parallel=FALSE}, then CalcLikelihood passes off to CalclikelihoodC completely.
#' @export
CalcLikelihoodC <- function(dtm, phi, theta) {
    .Call('idaTopicModels_CalcLikelihoodC', PACKAGE = 'idaTopicModels', dtm, phi, theta)
}

#' Calculate the sum of square errors for documents
#' 
#' @param dtm A document term matrix
#' @param phi = a topics X terms dimensional matrix where each entry is p(term|topic)
#' @param theta = a documents X topics dimensional matrix where each entry is p(topic|document)//' @export
#' @description This function calculates a vector whose entries are SSE and SST, respectively. 
#' It is desinged to work with the function CalcTopicModelR2. Generally, this function (CalcSumSquares)
#' should is not designed to be called by itself.
#' @export
CalcSumSquares <- function(dtm, phi, theta, ybar) {
    .Call('idaTopicModels_CalcSumSquares', PACKAGE = 'idaTopicModels', dtm, phi, theta, ybar)
}


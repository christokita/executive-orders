#' @title Collapsed Gibbs Sampling at the Institute for Defense Analyses
#' @name IDA.lda.collapsed.gibbs.sampler
#' @description   This function fits a collapsed Gibbs sampler using asymmetric or 
#' symmetric Dirichlet priors for Latent Dirichlet Allocation (LDA)
#' models. This function takes a sparse representation of input documents,
#' performs the inference, and returns point estimates of the latent
#' parameters using the last state of iteration of Gibbs sampling.
#' 
#' This code is mostly built on on Jonathan Changs' "lda" package.
#' 
#'  @param documents Output from the lexicalize() function
#'
#'  @param K
#'    An integer representing the number of topics in the model.
#'
#'  @param vocab
#'    A character vector specifying the vocabulary words associated with
#'  the word indices used in documents. 
#'
#'  @param num.iterations
#'    The number of sweeps of Gibbs sampling over the entire corpus to make.
#'
#'  
#'   @param alpha
#'    Dirichlet hyperparameter for topic proportions.
#'    If a symmetric Dirichlet hyperparamter is desired, a scalar value is all
#'    that is required. If an asymmetric Dirichlet prior is desired instead, a 
#'    list of length D, the number of documents, is required. The asymmetric version
#'    is only approriate if there is strong prior knowledge about the prevalence of topic
#'    proportions.Otherwise the base
#'    lda.collapse.gibbs.sampler is from John Chang's 'lda' package is adequate.
#'  
#'  
#' 
#'  @param eta
#'    Dirichlet hyperparameter for topic multinomials.
#'    If a symmetric Dirichlet hyperparameter is desired, a scalar value is all
#'    that is required. If an asymmetric Dirichlet prior is desired instead, a list
#'    of length V, the number of unique tokens, is required. The asymmetric version
#'    is only appropriate for use in an ensemble of asymmetric models. Otherwise the base
#'    lda.collapse.gibbs.sampler is from John Chang's 'lda' package is adequate.
#'  
#'  @param initial
#'    A list of initial topic assignments for words.  It should be
#'  in the same format as the assignments field of the return
#'  value.  If this field is NULL, then the sampler will be initialized
#'  with random assignments.
#'  
#'  @param burnin
#'    A scalar integer indicating the number of Gibbs sweeps to consider
#'    as burn-in (i.e., throw away) for IDA.lda.collapsed.gibbs.sampler
#'    If this parameter is non-NULL, it
#'    will also have the side-effect of enabling the
#'    document_expects field of the return value (see below for
#'    details).  Note that burnin iterations do NOT count towards num.iterations.
#'  
#'  @param compute.log.likelihood
#'    A scalar logical which when TRUE will cause the sampler to
#'  compute the log likelihood of the words (to within a constant
#'  factor) after each sweep over the variables.  The log likelihood for each
#'  iteration is stored in the log.likelihood field of the result.
#'  This is useful for assessing convergence, but slows things down a tiny
#'  bit.
#'
#'  @param regularise
#'    When TRUE, a Gaussian prior is used for the regression
#'    coefficients. This requires the penalized package.
#'
#'  @param trace
#'    When trace is greater than zero, diagnostic messages will be
#'  output.  Larger values of trace imply more messages.
#'
#'  @param freeze.topics
#'    When TRUE, topic assignments will occur but the counts of
#'  words associated with topics will not change. initial should be
#'  set when this option is used. This is best use for sampling test
#'  documents.
#'
#' @details Returs fitted model as a list with the following components
#'  \item{assignments}{A list of length D.  Each element of the list, say
#'  \code{assignments[[i]]} is an integer vector of the same length as the
#'  number of columns in \code{documents[[i]]} indicating the topic
#'  assignment for each word.}  
#'  \item{topics}{A \eqn{K \times V} matrix where each entry indicates the
#'    number of times a word (column) was assigned to a topic (row).  The column
#'    names should correspond to the vocabulary words given in \var{vocab}.}
#'  \item{topic_sums}{A length K vector where each entry indicates the
#'    total number of times words were assigned to each topic.}
#'  \item{document_sums}{A \eqn{K \times D} matrix where each entry is an
#'    integer indicating the number of times words in each document
#'    (column) were assigned to each topic (column).}
#'  \item{log.likelihoods}{Only for \code{lda.collapsed.gibbs.sampler}.  A
#'    matrix with 2 rows and \code{num.iterations} columns of log likelihoods when the flag
#'    \code{compute.log.likelihood} is set to \code{TRUE}.  The first row
#'    contains the full log likelihood (including the prior), whereas the
#'    second row contains the log likelihood of the observations
#'    conditioned on the assignments.}
#'  \item{document_expects}{This field only exists if \var{burnin} is
#'    non-NULL. This field is like document_sums but instead of only
#'    aggregating counts for the last iteration, this field aggegates
#'    counts over all iterations after burnin.}  
#'
#' @references
#' \cite{Blei, David M. and Ng, Andrew and Jordan, Michael. Latent Dirichlet allocation. Journal of Machine Learning Research, 2003.}
#'
#'  \cite{Airoldi , Edoardo M.  and Blei, David M. and Fienberg, Stephen E. and Xing, Eric P.  Mixed Membership Stochastic Blockmodels. Journal of Machine Learning Research, 2008.}
#'
#'  \cite{Blei, David M. and McAuliffe, John.  Supervised topic models. Advances in Neural Information Processing Systems, 2008.}
#'
#'  \cite{Griffiths, Thomas L. and Steyvers, Mark.  Finding scientific topics.  Proceedings of the National Academy of Sciences, 2004.}
#'
#'
#'@note
#'  WARNING: This function does not compute precisely the correct thing
#'    when the count associated with a word in a document is not 1 (this
#'    is for speed reasons currently).  A workaround when a word appears
#'    multiple times is to replicate the word across several columns of a
#'    document.  This will likely be fixed in a future version.
#'
#'
#'@seealso
#' \code{\link{read.documents}} and \code{\link{lexicalize}} can be used
#'    to generate the input data to these models.
#'
#'    \code{\link{top.topic.words}},
#'    \code{\link{predictive.distribution}}, and \code{\link{slda.predict}} for operations on the fitted models. 
#'
#'@examples
#'
#'lex <- lexicalize(Dtm2Docs(dtm=mydtm), sep=" ", vocab=colnames(dtm))
#'
#'beta <- Matrix::colSums(dtm) / sum(Matrix::colSums(dtm)) * 500
#'
#'mylda <- IDA.lda.collapsed.gibbs.sampler(documents=lex, K=100, vocab=colnames(dtm), num.iterations=2000, alpha=0.1, eta=beta)
#'
#'
#' @export









IDA.lda.collapsed.gibbs.sampler <-function (documents, K, vocab, num.iterations, alpha, eta, initial = NULL, 
            burnin = NULL, compute.log.likelihood = FALSE, trace = 0L, 
            freeze.topics = FALSE, debug = FALSE){ 
    
    
    if (class(vocab) == "list") {
      lengths <- as.integer(sapply(vocab, length))
      V = length(lengths)
      all.vocab <- do.call(c, vocab)
    } else {
      lengths <- as.integer(length(vocab))
      V = lengths
      all.vocab <- vocab
    }
  
    if(length(alpha) == 1){
      alpha = rep(alpha,K)
    }
  
    if(length(eta)==1){
      eta = rep(eta, V)
    }
  
    if (debug == TRUE) {
      print(paste0("DEBUG: alpha: ", alpha))
      print(paste0("DEBUG: eta: ", eta))
      print(paste0("DEBUG: K: ", K))
      
    }

    alpha = sample(x = alpha, size = length(alpha), replace = F)
    #eta = sample(x = eta, size = length(eta), replace = F)
    
    retval <- structure(.Call("MYcollapsedGibbsSampler", documents, 
                              as.integer(K), lengths, as.integer(num.iterations), alpha, 
                              eta, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
                              initial, as.integer(burnin), as.logical(compute.log.likelihood), 
                              trace, as.logical(freeze.topics), PACKAGE="idaTopicModels" ), names = c("assignments", 
                                                                           "topics", "topic_sums", "document_sums", if (is.null(burnin)) NA else "document_expects", 
                                                                           NA, NA, NA, NA, if (compute.log.likelihood) "log.likelihoods" else NA))
    colnames(retval$topics) <- all.vocab
    retval
  }
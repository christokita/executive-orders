#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <Rmath.h>

#define sLDA 1
#define corrLDA 2
#define prodLDA 3

//This returns an object of type double depending
//on the method sLDA corrLDA or prodLDA

double dv_update(SEXP annotations, int dd,
		 double beta_z, double var,
		 int nw, int method, int logistic) {
  if (method == sLDA) {
    return beta_z / nw;
  } else if (method == corrLDA) {
    if (logistic) {
      double x_d = LOGICAL(annotations)[dd] * 2 - 1;
      return 1.0 / (1.0 + exp(-x_d * beta_z)) / nw;
    } else {
      double x_d = REAL(annotations)[dd];
      return exp(-(x_d - beta_z) * (x_d - beta_z) / (2 * var)) / nw;
    }
  } else if (method == prodLDA) {
    error("Not implemented.");
  }
}
//This is a general function to check that the value is 
//of the correct type, value and type are provided
#define CHECK(VAL, TYPE) if (!is##TYPE(VAL)) { \
    error(#VAL " must be a(n) " #TYPE "."); \
  }

 //This checks if the value is of the correct length
#define CHECKLEN(VAL, TYPE, LEN) if (!is##TYPE(VAL) || length(VAL) != LEN) { \
    error(#VAL " must be a length " #LEN " " #TYPE "."); \
  }

  //checks if a matrix has the correct number of rows
  //or if it is the wrong type
#define CHECKMATROW(VAL, TYPE, NROW) if (!isMatrix(VAL) || !is##TYPE(VAL) || NUMROWS(VAL) != NROW) { \
    error(#VAL " must be a matrix with " #NROW " rows of type " #TYPE "."); \
  }

//This is a utility function to grab the number of
//rows the matrix has
#define NUMROWS(MAT) (INTEGER(GET_DIM(MAT))[0])
//This is a utility function to grab the number of
//columns the matrix has
#define NUMCOLS(MAT) (INTEGER(GET_DIM(MAT))[1])

//this is a function to update the sums 
//defining the function this way makes it take global
//parameters instead of local parameters

#define UPDATESUMS(weight) { \
  INTEGER(topics)[z + sumK * word] += weight * count; \
  INTEGER(topic_sums)[z] += weight * count; \
  int src = INTEGER(source)[c];	\
  int lt = INTEGER(local_topics)[src]; \
  INTEGER(VECTOR_ELT(document_sums, src))[z - partialK[lt]] += weight * count; \
  INTEGER(document_sources)[c] += weight * count; \
  document_lengths[src] += weight * count; \
  }

 
//here we go, this is the collapsed Gibbs Sampler function
// Things may not work if annotations and netannotations are both non-null.
SEXP MYcollapsedGibbsSampler(SEXP documents, //user provided documents, a list
//of the length of the number of documents, each element in the list is a
//matrix with the number of words, the index of words, and the frequency of words
//in LDA style presentation
			   SEXP K_,//user provided number of topics
			   SEXP V_, //a vector of length 1 that is the length of 
			   //the number of vocab words
			   SEXP N_,//gibbs iterations
			   SEXP alpha_,//user provided alpha
			   SEXP eta_,//user provided eta
			   SEXP annotations,//creator provided NULL
			   SEXP beta,//NULL
			   SEXP var_,//NULL
			   SEXP method_,//NULL
			   SEXP lambda_,//NULL
			   SEXP nbeta,//NULL
			   SEXP net_annotations,//NULL
			   SEXP initial_,//User provided initial parameters
			   SEXP burnin_,//User provided burnin integer
			   SEXP compute_log_likelihood_,//user provided T/F logical
			   SEXP trace_,//provided trace (default 0 logical)
			   SEXP freeze_topics_) {//creator provided character vector
  GetRNGstate();
 // Rprintf("Entering Function");
  Rprintf("Entering Function");
  int dd;
  //Rprintf("Entering Function");
  int ii;
  //Rprintf("Entering Function");
  int kk;
  //Rprintf("Entering Function");
  double var = 0;
  //Rprintf("Entering Function");
  int logistic = 0;
  //Rprintf("Entering Function");
  double lambda = 0;
  //Rprintf("Entering Function");
  int burnin = -1;
  Rprintf("Checking K");

  CHECKLEN(K_, Integer, 1);
  Rprintf("Setting K");
  int K = INTEGER(K_)[0]; //K gets translated into C

  CHECKLEN(trace_, Integer, 1);
  int trace = INTEGER(trace_)[0]; //trace gets translated into CHAR

  //Vocab gets checked to be an integer
    Rprintf("Getting V");
  CHECK(V_, Integer);
  int V = 0;
  for (ii = 0; ii < length(V_); ++ii) {
    V += INTEGER(V_)[ii];
  }
//V is now the number of words, while V_ is the vocab
Rprintf("Checking Alpha");
  /////////////////////////////////////////////////////////////////////
 // CHECK(alpha_,NewList); //checks that the alpha is of length 1, needs to change to 
							//the correct length  
//double* p = (double *)R_alloc(K, sizeof(double));

	//SEXP alpha = alpha_;
	CHECKLEN(alpha_, Real, K);
	double* alpha = (double *)R_alloc(K, sizeof(double));
	Rprintf("Building Alpha");
	//Rprintf("Building Alpha");
	for(ii = 0; ii < K; ++ii){
		alpha[ii] = REAL(alpha_)[ii];//need to check this declaration line
	}
	Rprintf("Alpha Initialized");
	  CHECKLEN(eta_, Real, V); //Need to change eta to be the correct length 
	double* eta = (double *)R_alloc(V, sizeof(double));
	for(ii=0; ii < V; ++ii){
		eta[ii] = REAL(eta_)[ii];
	}//and need to adjust how it is fed into eta 
/////////////////////////////////////////////////////////////////////////
  //translate the number of iterations
  CHECKLEN(N_, Integer, 1);
  int N = INTEGER(N_)[0];

  int method = -1;

  //get the documents into CHAR
  CHECK(documents, NewList);
  //nd is now the number of documents
  int nd = length(documents);

  double* dv = NULL;
  double* wx2 = NULL;
  double* wx = NULL;
//this all deals with annotated stuff which we aren't worrying about here
  if (!isNull(annotations)) {
    if (length(annotations) != nd) {
      error("annotations must have same length as documents.");
    }
    if (isReal(annotations)) {
      logistic = 0;
    } else if (isLogical(annotations)) {
      logistic = 1;
    } else {
      error("annotations must be real or logical.");
    }

    CHECKLEN(var_, Real, 1);
    var = REAL(var_)[0];

    CHECKLEN(beta, Real, K);

    CHECKLEN(method_, Integer, 1);
    method = INTEGER(method_)[0];
    if (method < 1 || method > 3) {
      error("method must be between 1 and 3.");
    }

    dv = (double *)R_alloc(nd, sizeof(double));
    if (method == prodLDA) {
      wx = (double *)R_alloc(K, sizeof(double));
      wx2 = (double *)R_alloc(K, sizeof(double));
      CHECKLEN(lambda_, Real, 1);
      lambda = REAL(lambda_)[0];
    }
  } else {
    if (!isNull(beta)) {
      error("beta must be null when annotations are empty.");
    }
    if (!isNull(var_)) {
      error("var must be null when annotations are empty.");
    }
  }
//initialize the retval - return value and its partners
  SEXP retval;
  PROTECT(retval = allocVector(VECSXP, 10));

  SEXP nbeta_one, nbeta_zero;
  SEXP nassignments_left, nassignments_right;
//This if loops contains only annotated data considerations
  if (!isNull(net_annotations)) {
    CHECKLEN(net_annotations, Logical, nd * nd);
    CHECKLEN(nbeta, NewList, 2);
    CHECKLEN(VECTOR_ELT(nbeta, 0), Real, K*K);
    CHECKLEN(VECTOR_ELT(nbeta, 1), Real, K*K);

    SET_VECTOR_ELT(retval, 5, nassignments_left = allocMatrix(INTSXP, nd, nd));
    SET_VECTOR_ELT(retval, 6, nassignments_right = allocMatrix(INTSXP, nd, nd));
    SET_VECTOR_ELT(retval, 7, nbeta_zero = allocMatrix(INTSXP, K, K));
    SET_VECTOR_ELT(retval, 8, nbeta_one = allocMatrix(INTSXP, K, K));

    for (ii = 0; ii < K * K; ++ii) {
      INTEGER(nbeta_one)[ii] = 0;
      INTEGER(nbeta_zero)[ii] = 0;
    }
    for (ii = 0; ii < nd * nd; ++ii) {
      INTEGER(nassignments_left)[ii] = -1;
      INTEGER(nassignments_right)[ii] = -1;
    }
  }
//Initializes and sets the retval components
  SEXP assignments;
  SEXP topics = NULL;
  SEXP topic_sums = NULL;
  SEXP document_expects = NULL;
  SEXP document_sums;
  SEXP initial = NULL;
  SEXP initial_topic_sums = NULL;
  SEXP initial_topics = NULL;
  SEXP log_likelihood = NULL;

  SET_VECTOR_ELT(retval, 0, assignments = allocVector(VECSXP, nd));
  SET_VECTOR_ELT(retval, 1, topics = allocMatrix(INTSXP, K, V));
  SET_VECTOR_ELT(retval, 2, topic_sums = allocMatrix(INTSXP, K, length(V_)));
  SET_VECTOR_ELT(retval, 3, document_sums = allocMatrix(INTSXP, K, nd));

  //Decide if compute the log likelihood needs to occur
  CHECKLEN(compute_log_likelihood_, Logical, 1);
  int compute_log_likelihood = LOGICAL(compute_log_likelihood_)[0];
  //Check if the topics need to be frozen
  CHECKLEN(freeze_topics_, Logical, 1);
  int freeze_topics = LOGICAL(freeze_topics_)[0];
  if (compute_log_likelihood) {
    SET_VECTOR_ELT(retval, 9, log_likelihood = allocMatrix(REALSXP, 2, N));
  }
  //If there is burnin required, translate length to c...
  if (length(burnin_) > 0) {
    CHECKLEN(burnin_, Integer, 1);
    burnin = INTEGER(burnin_)[0];
    if (burnin < 0) {
      error("burnin must be positive.");
    }
//and include it in the retval
    SET_VECTOR_ELT(retval, 4, document_expects = allocMatrix(INTSXP, K, nd));
    for (ii = 0; ii < K * nd; ++ii) {
      INTEGER(document_expects)[ii] = 0;
    }
  }

  //initialize parameter values in case the initial values
  //were left as NULL by the users
  if (!isNull(initial_)) {
	//Make sure the intial_ vector provided is of type NewList
    CHECK(initial_, NewList);
	//Find the names that were given in R to the list components
    SEXP names = getAttrib(initial_, R_NamesSymbol);
	
	//for each of the initial parameters provided, translate
    for (ii = 0; ii < length(initial_); ++ii) {
	//strcmp compares two strings - as long as the string elements
	//are not "assignments" 
      if (!strcmp(CHAR(STRING_ELT(names, ii)), "assignments")) {
	initial = VECTOR_ELT(initial_, ii); //make initial that vector
	CHECKLEN(initial, NewList, nd); //make sure initial is the length nd
	//which it must be because it is the inital assignment for each topic
      } else if (!strcmp(CHAR(STRING_ELT(names, ii)), "topic_sums")) {
	initial_topic_sums = VECTOR_ELT(initial_, ii);
	if (!isInteger(initial_topic_sums) ||
	    INTEGER(GET_DIM(initial_topic_sums))[0] != K ||
	    INTEGER(GET_DIM(initial_topic_sums))[1] != length(V_)) {
	  error("Initial topic sums must be a K x length(V) integer matrix.");
	}
      } else if (!strcmp(CHAR(STRING_ELT(names, ii)), "topics")) {
	initial_topics = VECTOR_ELT(initial_, ii);
	if (!isInteger(initial_topics) ||
	    INTEGER(GET_DIM(initial_topics))[0] != K ||
	    INTEGER(GET_DIM(initial_topics))[1] != V) {
	  error("Initial topics (%d x %d) must be a %d x %d integer matrix.",
		INTEGER(GET_DIM(initial_topics))[0],
		INTEGER(GET_DIM(initial_topics))[1],
		K,
		V);
	}
      } else {
	error("Unrecognized initialization field: '%s'",
	      CHAR(STRING_ELT(names, ii)));
      }
    }
  }
//If only one of initial_topics_sums and initial_topics were stated
//then return an error, if neither were stated then continue wihtout
//specifying

  if ((initial_topic_sums == NULL) ^ (initial_topics == NULL)) {
    error("initial topic sums and topics must both be specified.");
  }

//initialize the topics
  if (initial_topics == NULL) {
    for (ii = 0; ii < K * V; ++ii) {
      INTEGER(topics)[ii] = 0; //initialize to 0 if it was empty
    }
  } else {
    for (ii = 0; ii < K * V; ++ii) {
      INTEGER(topics)[ii] = INTEGER(initial_topics)[ii]; //initialize
	  //to user specified topics if it was input
    }
  }
  if (initial_topic_sums == NULL) {
    for (ii = 0; ii < K * length(V_); ++ii) {
      INTEGER(topic_sums)[ii] = 0; //set topic sums to 0 if null
    }
  } else {
    for (ii = 0; ii < K * length(V_); ++ii) {
      INTEGER(topic_sums)[ii] = INTEGER(initial_topic_sums)[ii];
	  //integer topic sums set to user specified initial values
    }
  }

  for (ii = 0; ii < K * nd; ++ii) {
    INTEGER(document_sums)[ii] = 0; //initialize document sums to 0
  }

//Process the documents, check for errors in vocab
  for (dd = 0; dd < nd; ++dd) {
    int ww;
	//select the dd-th document
    SEXP document = VECTOR_ELT(documents, dd);
	
	//Check that the document is of the right type and dimension
    CHECKMATROW(document, Integer, 2);
	
	//Check annotations - we aren't dealing with this
    if (!isNull(annotations)) {
      if (method == corrLDA || logistic) {
	dv[dd] = 0.0;
      } else {
	dv[dd] = REAL(annotations)[dd];
      }
    }

	//nw is the number of words
    int nw = INTEGER(GET_DIM(document))[1];
	//set the dd-th element in assignments to nw for the doc
    SET_VECTOR_ELT(assignments, dd, allocVector(INTSXP, nw));
	//zs is the vector element of assignments dd
    SEXP zs = VECTOR_ELT(assignments, dd);

	//For each word in the document
    for (ww = 0; ww < nw; ++ww) {
	//Take the word of the document - which is organized
	//as a vector apparently - even takes words, odd takes counts
      int word = INTEGER(document)[ww * 2];
      int count = INTEGER(document)[ww * 2 + 1];
      if (count < 0) {
	error("Count must be positive.");
      }
      if (word >= V || word < 0) {
	error("Word (%d) must be positive and less than or "
	      "equal to the number of words (%d).", word, V);
      }
	  //?
      INTEGER(zs)[ww] = -1;
    }
  }
//Not worried about prodLDA
  if (method == prodLDA) {
    for (kk = 0; kk < K; ++kk) {
      wx[kk] = 0.0;
      wx2[kk] = 0.0;
    }
  }

  //allocate memory to store p and p_pair
  double* p = (double *)R_alloc(K, sizeof(double));
  double* p_pair = NULL;
  if (!isNull(net_annotations)) {
  //p_pair is K^2 size, seems like a pairwise probability of topics
    p_pair = (double *)R_alloc(K * K, sizeof(double));
  }

  double sumlgalph = 0;
  double alphsum = 0;
  double sumlgeta = 0;
  double etasum = 0;
  double const_prior = 0;
  double const_ll = 0;

  //need to investigate how the loglikelihood changes with
  //vectorizing alpha and eta
  //////////BEGIN//EDITED////////////////////////////////
  Rprintf("Computing log-likelihood ");
  if (compute_log_likelihood) {
    //                log B(\alpha)
	for(int aa = 0; aa < K; ++aa){
		sumlgalph += lgammafn(alpha[aa]);
		alphsum += alpha[aa];
	}
	for(int ee = 0; ee < V; ++ee){
		sumlgeta += lgammafn(eta[ee]);
		etasum += eta[ee];
	}
    const_prior = (sumlgalph - lgammafn(alphsum)) * nd;
    //                log B(\eta)
    const_ll = (sumlgeta - lgammafn(etasum)) * K;
  }
///////END///EDITED///////////////////////
//start main Gibbs loop
  int iteration;
  for (iteration = 0; iteration < N; ++iteration) {
    if (trace >= 1) {
      Rprintf("Iteration %d\n", iteration);
    }
    for (dd = 0; dd < nd; ++dd) {
      R_CheckUserInterrupt(); //check to see if need to exit loop
      SEXP zs = VECTOR_ELT(assignments, dd); //zs is topic assignment for document
      SEXP document = VECTOR_ELT(documents, dd); //get document
      int ww;
      int nw = INTEGER(GET_DIM(document))[1]; //get nw number of words
      SEXP initial_d = NULL;

      if (initial != NULL) {
	initial_d = VECTOR_ELT(initial, dd); //check that initial values
	//for document are length number of words
	CHECKLEN(initial_d, Integer, nw);
      }
	  //this if condition is onyl for when annotations are on
      if (!isNull(net_annotations)) {
	for (ww = 0; ww < nd; ++ww) {
	  if (ww == dd) {
	    continue;
	  }
	//first pass is always -1, so is z2
	  int* z = &INTEGER(nassignments_left)[nd * dd + ww];
	  int* z2 = &INTEGER(nassignments_right)[nd * ww + dd];
	  //y indicates if annotations are available which we 
	  //aren't worried about
	  int y = LOGICAL(net_annotations)[nd * dd + ww];

	  if (*z != -1) {
	    if (*z2 == -1) {
	      error("Internal error (1).");
	    }
	    INTEGER(document_sums)[K * dd + *z]--;//decrements the document sum
	    INTEGER(document_sums)[K * ww + *z2]--;
	    if (y == 1) {
	    INTEGER(nbeta_one)[K * (*z) + (*z2)]--;
	    } else {
	      INTEGER(nbeta_zero)[K * (*z) + (*z2)]--;
	    }
	  } else if (*z2 != -1) {
	    error("Internal error (2).");
	  }

	  
	  double p_sum = 0.0;
	  int jj;
	  //for each topic
	  for (ii = 0; ii < K; ++ii) {
	  //for each topic
	    for (jj = 0; jj < K; ++jj) {
		//if z ==-1
	      if (*z == -1) {
		p_pair[ii * K + jj] = 1.0;
	      } else {
		p_pair[ii * K + jj] = (INTEGER(document_sums)[K * dd + ii] + alpha[ii])*
		  (INTEGER(document_sums)[K * ww + jj] + alpha[ii]);
		if (y == 1) {
		  p_pair[ii * K + jj] *= INTEGER(nbeta_one)[ii * K + jj] +
		    REAL(VECTOR_ELT(nbeta, 1))[ii * K + jj];
		} else if (y == 0) {
		  p_pair[ii * K + jj] *= INTEGER(nbeta_zero)[ii * K + jj] +
		    REAL(VECTOR_ELT(nbeta, 0))[ii * K + jj];
		} else {
		  error("What the hell happened?");
	      }
		p_pair[ii * K + jj] /= INTEGER(nbeta_one)[ii * K + jj] +
		  INTEGER(nbeta_zero)[ii * K + jj] +
		  REAL(VECTOR_ELT(nbeta, 0))[ii * K + jj] +
		  REAL(VECTOR_ELT(nbeta, 1))[ii * K + jj];
	      }
	      if (p_pair[ii * K + jj] < 0) {
		error("What the WHAT?! (%d, %d)",
		      INTEGER(nbeta_one)[ii * K + jj],
		      INTEGER(nbeta_zero)[ii * K + jj]);
	      }
	      p_sum += p_pair[ii * K + jj];
	    }
	  }

	  *z = -1;
	  *z2 = -1;
	  double r = unif_rand();
	  double r_orig = r;
	  for (ii = 0; ii < K; ++ii) {
	    for (jj = 0; jj < K; ++jj) {
	      if (r < p_pair[ii * K + jj] / p_sum) {
		*z = ii;
		*z2 = jj;
		ii = jj = K;
		break;
	      }
	      r -= p_pair[ii * K + jj] / p_sum;
	    }
	  }
	  if (*z == -1 || *z2 == -1) {
	    error("The laws of science be a harsh mistress "
		  "(%d, %d, %g, %g, %g).",
		  *z, *z2, r_orig, r, p_sum);
	  }

	  INTEGER(document_sums)[K * dd + *z]++;
	  INTEGER(document_sums)[K * ww + *z2]++;

	  if (burnin > -1 && iteration >= burnin) {
	    INTEGER(document_expects)[K * dd + *z]++;
	    INTEGER(document_expects)[K * ww + *z2]++;
	  }

	  if (y == 1) {
	    INTEGER(nbeta_one)[K * (*z) + (*z2)]++;
	  } else {
	    INTEGER(nbeta_zero)[K * (*z) + (*z2)]++;
	  }
	}
      }
//when there are no annotations, then this loop starts
//for each word in the document
      for (ww = 0; ww < nw; ++ww) {
	int* z = &INTEGER(zs)[ww]; //get the current topic for the word
	int word = -1; //initialize word and count
	int count = 1;
	int* topic_wk; //initialize topic_wk, topic_k and document_k
	int* topic_k;
	int* document_k;

	word = INTEGER(document)[ww * 2]; //get the current word
	int partialsum = 0; //initialize partial sum
	int topic_index = -1; //initialize the topic index variable
	//for each word in the vocabulary
	for (ii = 0; ii < length(V_); ++ii) {
	//increase partial sum by the count in each V_ (should just be nw)?
	  partialsum += INTEGER(V_)[ii]; 
	  if (word < partialsum) { //if the word value is less than nw
	    topic_index = ii; //topic index gets set to ii at that point
	  }
	}
	if (topic_index == -1) {
	  error("Oops I did it again");
	}
	//get the frequency of the word in the document
	count = INTEGER(document)[ww * 2 + 1];

	//if the topic has a previous assignment, get counts
	//associated with its assignment
	if (*z != -1) {
	  topic_wk = &INTEGER(topics)[(*z) + K * word];
	  topic_k = &INTEGER(topic_sums)[*z + K * topic_index];
	  if(!freeze_topics)
	  {
	    *topic_wk -= count;
	    *topic_k -= count;
	  }
	  document_k = &INTEGER(document_sums)[K * dd + *z];
	  *document_k -= count;

	  if (!isNull(annotations)) {
	    if (method == prodLDA) {
	      wx2[*z] -= count * REAL(annotations)[dd] * REAL(annotations)[dd];
	      wx[*z] -= count * REAL(annotations)[dd];
	    } else {
	      dv[dd] += count * dv_update(annotations, dd, REAL(beta)[*z],
					  var, nw, method, logistic);
	    }
	  }

	  if (*topic_wk < 0 || *topic_k < 0 || *document_k < 0) {
	    error("Counts became negative for word (%d): (%d, %d, %d)",
		  word, *topic_wk, *topic_k, *document_k);
	  }
	}

	//take r the random draw for assigning
	double r = unif_rand();
	//get the total "probability" we accumulate that each
	//sum ratio is proportional to
	double p_sum = 0.0;
	for (kk = 0; kk < K; ++kk) {
	  if (*z == -1) {
	    if (initial != NULL) {
	      if (INTEGER(initial_d)[ww] == kk) {
		p[kk] = 1;
	      } else {
		p[kk] = 0;
	      }
	    } else {
	      p[kk] = 1;
	    }
		//if it's not a trivial case
	  } else {
	  //This assignment becomes easy, alpha simply because alpha[k]
	  // and eta becomes eta[k]
	  //note that we never actually update eta - it's just the prior
	  ///////////////////BEGIN///EDITED////////////////////////////////////
	    p[kk] = (INTEGER(document_sums)[K * dd + kk] + alpha[kk]);
	    p[kk] *= (INTEGER(topics)[kk + K * word] + eta[word]);
	    p[kk] /= (INTEGER(topic_sums)[kk + K * topic_index] + etasum);
/////////////////////END EDITED////////////////////////////////////////////////
	    if (!isNull(annotations)) {
	      if (method == corrLDA) {
		p[kk] *= dv_update(annotations, dd, REAL(beta)[kk],
				   var, nw, method, logistic) - dv[dd];
	      } else if (method == sLDA) {
		double change = REAL(beta)[kk] / nw;
		if (logistic) {
		  double yv = 2 * LOGICAL(annotations)[dd] - 1.0;
		  p[kk] /= 1.0 + exp(yv * (dv[dd] - change));
		} else {
      // How does this work?
      // dv[dd] = y - sum_{i != n} beta_{z_i} / N
      // change = beta_{z_n} / N
      // What we want to compute i:
      // exp(2 * change * (dv[dd]) - change^2)
		  p[kk] *= exp(change * (dv[dd] - change / 2) / var);
		}
	      } else if (method == prodLDA) {
		double x_d = REAL(annotations)[dd];
		int n_k = INTEGER(topic_sums)[kk + K * topic_index] + 1 + lambda;
		p[kk] *= sqrt(n_k) *
		  exp(-(wx2[kk] - wx2[0] - (wx[kk] + x_d)*(wx[kk] + x_d)/n_k + (wx[0] + x_d) * (wx[0] + x_d) / n_k) / (2 * var));
	      } else {
		error("Not implemented.");
	      }
	    }
	  }
	  p_sum += p[kk];
	}

	if (p_sum <= 0.0) {
	  kk = K - 1;
	  error("Numerical problems (%g, %g).", dv[dd],
		dv_update(annotations, dd, REAL(beta)[kk],
			  var, nw, method, logistic));
	}
//reset the topic assignment
	*z = -1;
	for (kk = 0; kk < K; ++kk) {
	  if (r < p[kk] / p_sum) {
	  //if r < what's left over, then we have hit the correct topic,
	  //so make the assignment
	    *z = kk;
	    break;
	  }
	  //if there is more probability mass, then subtract off what 
	  //we've compared already
	  r -= p[kk] / p_sum;
	}

	if (*z == -1) {
	  for (kk = 0; kk < K; ++kk) {
	    Rprintf("%g\n", p[kk]);
	  }
	  error("This should not have happened (%g).", r);
	}

	if(!freeze_topics)
	{
	  INTEGER(topics)[*z + K * word] += count;
	  INTEGER(topic_sums)[*z + K * topic_index] += count;
	}
	INTEGER(document_sums)[K * dd + *z] += count;
	if (burnin > -1 && iteration >= burnin) {
	  INTEGER(document_expects)[K * dd + *z] += count;
	}

	if (!isNull(annotations)) {
	  if (method == prodLDA) {
	    wx2[*z] += count * REAL(annotations)[dd] * REAL(annotations)[dd];
	    wx[*z] += count * REAL(annotations)[dd];
	  } else {
	    dv[dd] -= count * dv_update(annotations, dd, REAL(beta)[*z],
					var, nw, method, logistic);
	  }
	}
      }
    }

////////////////////////////////////////////////////////////////////////////
    /*Compute the likelihoods:*/
    if (compute_log_likelihood) {
      double doc_ll = 0;
      for (dd = 0; dd < nd; ++dd) {
	double sum = alphsum;//instead of *K this needs to be sum =+alpha[kk]
	for (kk = 0; kk < K; ++kk) {
	//similarly this needs to be + alpha[k] vectorized
	  doc_ll += lgammafn(INTEGER(document_sums)[K * dd + kk] + alpha[kk]);
	  sum += INTEGER(document_sums)[K * dd + kk];
	}
	doc_ll -= lgammafn(sum);
      }
      double topic_ll = 0;
      for (kk = 0; kk < K; ++kk) {
	double sum = etasum; //instead should be =+ eta[V]
	for (ii = 0; ii < V; ++ii) {
	  topic_ll += lgammafn(INTEGER(topics)[kk + K * ii] + eta[ii]);//should be eta[ii]
	  sum += INTEGER(topics)[kk + K * ii];
	}
	topic_ll -= lgammafn(sum);
      }
      if (trace >= 2) {
	Rprintf("ll: %g + %g - %g - %g = %g\n", doc_ll, topic_ll, const_ll, const_prior,
	       doc_ll + topic_ll - const_ll - const_prior);
      }
      REAL(log_likelihood)[2 * iteration] = doc_ll - const_prior + topic_ll - const_ll;
      REAL(log_likelihood)[2 * iteration + 1] = topic_ll - const_ll;
    }
  }
///////////////////////////////////////////////////////////

  PutRNGstate();
  UNPROTECT(1);
  return retval;
}




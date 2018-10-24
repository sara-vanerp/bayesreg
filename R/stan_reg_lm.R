#' Bayesian regularized linear regression with Stan
#'
#' Function to run regularized linear regression models using Stan. Allows the user to specify which 
#' shrinkage prior to use for the regression coefficients.
#' 
#' @export
#' @param X Numeric matrix[N, p] of input values. The input values get automatically standardized in the function.
#' @param y Numeric vector[N] of output values.
#' @param N_train Size of the training set. First part of the data will be used for training. 
#' @param prior Shrinkage prior to use for regularization. Current options are: ridge, student, lasso,
#' elasticNet, hyperlasso, horseshoe, regularizedHorseshoe, and mixture.
#' @param df Degrees of freedom for the hyperlasso. Defaults to 0.5, but larger values can lead to less divergent transitions.
#' @param p0 Prior information on the number of relevant predictors. Used to determine the global scale parameter in the regularized horseshoe.
#' @param global_scale Scale parameter for the t-distribution on the global shrinkage parameter in the regularized horseshoe. Defaults to 1, but better is to provide p0 if prior info is available. 
#' @param global_df Degrees of freedom for the t-distribution on the global shrinkage parameter in the regularized horseshoe. Defaults to 1.
#' @param local_df Degrees of freedom for the t-distribution on the local shrinkage parameter in the regularized horseshoe. Defaults to 1.
#' @param slab_scale Scale parameter for the t-distribution shrinking the large coefficients. Defaults to 1.
#' @param slab_df Degrees of freedom for the t-distribution shrinking the large coefficients. Defaults to 1.
#' @param hyperprior_mix Hyperprior for the mixing probabilities in the normal mixture prior. Options are: uniform or Bernoulli.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`.
#'

stan_reg_lm <- function(X, y, N_train, 
	prior = c("ridge", "student", "lasso", "elasticNet", "hyperlasso", "horseshoe", "regularizedHorseshoe", "mixture"), 
	df = 0.5, p0 = NA, global_scale = 1, global_df = 1, local_df = 1,
	slab_scale = 1, slab_df = 1, hyperprior_mix = c("Bernoulli", "uniform"),
	...){

	if(prior != "mixture"){ mod = paste0("lm_noNA_", prior, "_FB")}
	if(prior == "mixture"){
		if(hyperprior_mix == "uniform"){ mod = "lm_noNA_normalMixUni_FB" }
		if(hyperprior_mix == "Bernoulli"){ mod = "lm_noNA_normalMixBer_FB" }
	}

	# standardize the predictor matrix
	stanX = scale(X)

	# extract data information
	npred = ncol(stanX)
	ytrain = y[1:N_train]
	xtrain = stanX[1:N_train, ]
	ntest = nrow(stanX) - N_train
	xtest = stanX[(N_train + 1):nrow(stanX), ]

	# create stan input & run
	if(prior %in% c("ridge", "student", "lasso", "elasticNet", "horseshoe", "mixture")){
		standata <- list(N_train = N_train, p = npred, y_train = ytrain, X_train = xtrain, N_test = ntest, X_test = xtest)
	}
	if(prior == "hyperlasso"){ 
		if(df <= 0) stop(" 'df' must be > 0")
		standata <- list(N_train = N_train, p = npred, y_train = ytrain, X_train = xtrain, N_test = ntest, X_test = xtest, df = df)
	}
	if(prior == "regularizedHorseshoe"){
		tau0 <- ifelse(is.na(p0), global_scale, p0/((npred - p0) * sqrt(N_train)))
		standata <- list(N_train = N_train, p = npred, y_train = ytrain, X_train = xtrain, N_test = ntest, X_test = xtest,
						 scale_global = tau0, nu_global = global_df, nu_local = local_df, slab_scale = slab_scale, slab_df = slab_df)
	}

  	out <- rstan::sampling(stanmodels[[mod]], data = standata, ...)
  	return(out)
}
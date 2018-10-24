#' Prediction mean squared error for Bayesian regularized regression models
#'
#' Function to compute the prediction mean squared error (PMSE) on models fit using `stan_reg_lm`. The PMSE is computed as:
#' \eqn{ \frac{1}{N} \Sigma^N_{i=1} (y^{gen}_i - y_i)^2 }, with \eqn{ y^{gen}_i } being the posterior mean of the MCMC draws
#' for the predicted value of that observation and \eqn{y_i} being the actual value in the test set.
#' 
#' @export
#' @param object An object of class `stanfit` returned by `stan_reg_lm`.
#' @param ytest Numeric vector of output values for the test set. Provide either `ytest` or `y` and `N_train`.
#' @param y Numeric vector[N] of output values. Provide either `ytest` or `y` and `N_train`.
#' @param N_train Size of the training set. First part of the data will be used for training. Provide either `ytest` or `y` and `N_train`.
#' @return Numeric value for the prediction mean squared error.
#'

pmse_lm <- function(object, ytest = NULL, y = NULL, N_train = NULL){
	if(is.null(ytest)){
		if(is.null(N_train)) stop("Both y and N_train should be provided instead of ytest.")
		if(is.null(y)) stop("Both y and N_train should be provided instead of ytest.")
		ytest = y[(N_train + 1):length(y)]
	}

	draws = rstan::extract(object, pars = "y_test")
	postmeans = apply(draws[[1]], 2, mean)
	if(length(postmeans) != length(ytest)) stop("Length of ytest should equal the length of the predicted test set.")
	pmse = mean((postmeans-ytest)^2) 
	return(pmse)
}
#' Variable selection for Bayesian regularized regression models
#'
#' Function to perform variable selection on models fit using `stan_reg_lm`. Variable selection is based on the
#' credibility interval criterion, excluding predictors for which the marginal credibility interval covers 0.
#' 
#' @export
#' @param object An object of class `stanfit` returned by `stan_reg_lm`.
#' @param X Numeric matrix[N, p] of input values used to fit the model including column names.
#' @param prob Numeric vector with probabilities \eqn{p \in (0,1)}{p (0 < p < 1)} indicating the desired
#'   probability mass to include in the intervals. The default is to report variable selection based on all 
#' 	 intervals ranging from 10\% to 90\%, i.e., prob = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9).
#' @return Logical matrix with variables in the rows and CIs in the columns returning TRUE if the CI covers 0, and FALSE otherwise. 
#'

select_lm <- function(object, X, prob = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)){
	varnms = colnames(X)
	draws = rstan::extract(object, pars = "beta")
	excl = sapply(prob, function(d, nms = varnms){
		ci = posterior_interval(draws[[1]], d) # compute CIs
  		out = apply(ci, 1, function(x){
  			x[1] <= 0 & x[2] >= 0 
  		}) # check if 0 is in CI
  		names(out) = varnms
  		return(out) 
  	})
	colnames(excl) <- paste0(prob*100, "%")
	return(excl)
}
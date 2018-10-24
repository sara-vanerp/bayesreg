#' Summary function for Bayesian regularized regression models
#'
#' Function to return a summary of results for models fit using `stan_reg_lm`. 
#' 
#' @export
#' @param object An object of class `stanfit` returned by `stan_reg_lm`.
#' @param CI Numeric value specifying the probability \eqn{p \in (0,1)}{p (0 < p < 1)} indicating the desired
#' 	 probability mass to include in the reported marginal credibility interval. The default is 0.95.
#' @return Numeric matrix including the posterior mean, median, standard deviation and credibility interval
#' for the intercept (`int`), regression coefficients, and residual standard deviation (`sigma`).
#'

summary_lm <- function(object, CI = 0.95, prob = NULL){
	# results intercept
	intercept = rstan::extract(object, pars = "mu")
	mean.int = mean(intercept[[1]])
	median.int = median(intercept[[1]])
	sd.int = sd(intercept[[1]])
	ci.int = posterior_interval(as.matrix(intercept[[1]]), prob = CI)
	res.int = c(mean.int, median.int, sd.int, ci.int)

	# results regression coefficients
	beta = rstan::extract(object, pars = "beta")
	mean.beta = apply(beta[[1]], 2, mean)
	median.beta = apply(beta[[1]], 2, median)
	sd.beta = apply(beta[[1]], 2, sd)
	ci.beta = posterior_interval(beta[[1]], prob = CI)
	res.beta = cbind(mean.beta, median.beta, sd.beta, ci.beta)
	nms = paste0("beta", 1:ncol(beta[[1]]))

	# results residual standard deviation
	sigma = rstan::extract(object, pars = "sigma")
	mean.sigma = mean(sigma[[1]])
	median.sigma = median(sigma[[1]])
	sd.sigma = sd(sigma[[1]])
	ci.sigma = posterior_interval(as.matrix(sigma[[1]]), prob = CI)
	res.sigma = c(mean.sigma, median.sigma, sd.sigma, ci.sigma)

	# combine results
	res = rbind(res.int, res.beta, res.sigma)
	rownames(res) = c("int", nms, "sigma")
	nms.ci = c(paste0((1-CI)/2*100, "%"), paste0((CI+(1-CI)/2)*100, "%"))
  	colnames(res) = c("mean", "median", "sd", nms.ci)
	return(res)
}
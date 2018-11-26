#' Plot results for Bayesian regularized regression models
#'
#' Function to plot point estimates and credibility intervals for models fit using `stan_reg_lm`.
#' Results from multiple fit objects can be plotted for comparison.
#'
#' @export
#' @importFrom reshape2 melt
#' @import ggplot2
#' @importFrom plyr mapvalues
#' @param fit A named list with one or more objects of class `stanfit` returned by `stan_reg_lm`.
#' @param est A character indicating the posterior estimate to plot. Options are: posterior mean or median.
#' @param CI Numeric value specifying the probability \eqn{p \in (0,1)}{p (0 < p < 1)} indicating the desired
#' 	 credibility interval to plot. The default is 0.95.
#' @param npar Numeric value or sequence specifying the number of parameters to include in the plot. When a numeric
#' 	value is given, all parameters are plotted with a maximum of npar per plot, thus returning multiple plots. When a
#' 	sequence is given, only the parameters given by the sequence are plotted in one plot.
#' @param pred.nms A character vector of parameter names to include in the plot. Make sure that the names are ordered the
#' 	same way as the columns of the input matrix X used in the `stan_reg_lm` function.
#' @param pd Numeric value specifying the width with which to adjust the lines when multiple fitobjects are plotted to avoid overlap.
#' @return A list with ggplot objects.
#'

plot_est <- function(fit, est = c("mean", "median"), CI = 0.95, npar = 50, pred.nms = NULL, pd = 0.5){
  lb = (1-CI)/2
  lb.char = paste0(lb*100, "%")
  ub = CI + lb
  ub.char = paste0(ub*100, "%")
  if(est == "mean"){ post.est = "mean" }
  if(est == "median"){ post.est = "50%" }
  df = list()
  for(i in 1:length(fit)){
    posterior = as.matrix(fit[[i]])
    summ = rstan::summary(fit[[i]], pars = "beta", prob = c(lb, ub))$summary
    df[[i]] = cbind.data.frame(rownames(summ), summ[, post.est], summ[, lb.char], summ[, ub.char])
    colnames(df[[i]]) = c("par", "est", "lb", "ub")
    df[[i]]$name = factor(rep(names(fit)[i], nrow(df[[i]])))
    if(is.null(pred.nms) == FALSE){
      df[[i]]$par = plyr::mapvalues(df[[i]]$par, from = levels(df[[i]]$par), to = pred.nms)
    }
  }
  df.comb = do.call(rbind.data.frame, df)

  dfm1 = reshape2::melt(df.comb[, c("par", "est", "name")], id.vars = c("par", "name"), value.name = "est")
  dfm2 = reshape2::melt(df.comb[, c("par", "lb", "name")], id.vars = c("par", "name"), value.name = "lb")
  dfm3 = reshape2::melt(df.comb[, c("par", "ub", "name")], id.vars = c("par", "name"), value.name = "ub")
  dfm = Reduce(function(x, y) merge(x, y, all=TRUE), list(subset(dfm1, select = -variable), subset(dfm2, select = -variable),
                                                          subset(dfm3, select = -variable)))


  if(length(npar) == 1){
    sq = c(seq(1, nrow(dfm), npar), nrow(dfm) + 1)
    plotlist = list()
    for(i in 1:(length(sq)-1)){
      dfsel = dfm[sq[i]:(sq[i+1]-1), ]
      plotlist[[i]] = ggplot2::ggplot(data = dfsel, ggplot2::aes(x = par, y = est, group = name)) +
        ggplot2::geom_point(ggplot2::aes(colour = name), position = ggplot2::position_dodge(width = pd)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lb, ymax = ub, colour = name), position = ggplot2::position_dodge(width = pd)) +
        ggplot2::theme(legend.title = element_blank(), axis.title = element_blank()) +
        ggplot2::geom_hline(yintercept = 0, colour = "darkgrey") +
        ggplot2::coord_flip()
    }
  }

  if(length(npar) != 1){
    plotlist = list()
    dfsel = dfm[dfm$par %in% levels(dfm$par)[1:10], ]
    plotlist[[1]] = ggplot2::ggplot(data = dfsel, ggplot2::aes(x = par, y = est, group = name)) +
      ggplot2::geom_point(ggplot2::aes(colour = name), position = ggplot2::position_dodge(width = pd)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lb, ymax = ub, colour = name), position = ggplot2::position_dodge(width = pd)) +
      ggplot2::theme(legend.title = element_blank(), axis.title = element_blank()) +
      ggplot2::geom_hline(yintercept = 0, colour = "darkgrey") +
      ggplot2::coord_flip()
  }

  return(plotlist)
}

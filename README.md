# bayesreg

[![DOI](https://zenodo.org/badge/154002826.svg)](https://zenodo.org/badge/latestdoi/154002826)

An R package to perform Bayesian regularization using Stan. Models are pre-compiled and run using the probabilistic programming language [Stan](http://mc-stan.org). Currently only supports Bayesian regularization for linear regression models. Multiple shrinkage priors are implemented that will shrink small regression coefficients towards zero. The available options are based on the overview in the preprint [Shrinkage priors for Bayesian penalized regression](https://osf.io/4gr6z/). Note that only the full Bayesian approach is available to estimate the penalty parameter &lambda;.

## Getting Started

These instructions will install the package to your computer.

### Prerequisites

In order to install the package directly from Github, you need to have the **devtools** package:

```
install.packages("devtools")
```

Moreover, it might be needed to install a C++ toolchain, such as Rtools, by following these [instructions](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).

### Installing

To install the package from Github, first load **devtools**:

```
library(devtools)
```

Next, install **bayesreg** and make sure that `local = TRUE` to avoid recompilation of the Stan models. On Mac, the version of the package in the master branch can be installed as follows:

```
devtools::install_github("sara-vanerp/bayesreg", local = TRUE)
```
For Windows, the version of the package should be installed that is available in the Windows branch (thanks to Duco Veen for making this happen), as follows:

```
devtools::install_github("sara-vanerp/bayesreg@windows", local = TRUE)
```

The package can now be loaded into R and used:

```
library(bayesreg)
```

## Example

To provide an example of the **bayesreg** package, we will first simulate some data from a linear regression model:

```
library(MASS) # call install.packages("MASS") first if not yet installed
set.seed(31102018)
sim.lm <- function(nobs, cfs, sd, cov.mat){
  nvar <- length(cfs)  
  
  beta <- as.matrix(cfs)
  X <- mvrnorm(nobs, mu=rep(0, nvar), Sigma=cov.mat)
  y <- X %*% beta + rnorm(nobs,0,sd)
  
  return (list(x=X, y=y, coefficients=cfs, var=sd^2, covX=cov.mat))
}

dat <- sim.lm(nobs = 240, cfs = c(3, 1.5, 0, 0, 2, 0, 0, 0), sd = 3, cov.mat = diag(8))
```

The function `stan_reg_lm` is used to fit the model:

```
fit <- stan_reg_lm(X = dat$x, y = dat$y, N_train = 40, prior = "lasso")
```

Note that this function uses the **rstan** MCMC defaults. These can be adapted through the regular **rstan** arguments. For example, the number of iterations can be increased as follows:

```
fit <- stan_reg_lm(X = dat$x, y = dat$y, N_train = 40, prior = "lasso", iter = 4000)
```

See the **stan** function in the [**rstan**](https://cran.r-project.org/web/packages/rstan/rstan.pdf) documentation for options and details.

Currently, missing data is not supported. However, it is possible to impute the data before fitting the model, for example using the **mice** package. The model can then be fitted to each of the imputed data sets sepately and the resulting posterior draws can be combined.

The `stan_reg_lm` function returns an object of class `stanfit`, so the results can be extracted and plotted as usual `stanfit` objects. The **bayesreg** package has several post-estimation functions specific to Bayesian penalized regression.
First, we can request a summary of the results, including the 95% credibility intervals:

```
summary_lm(fit, CI = 0.95)
```

The prediction mean squared error (PMSE) on the test set is obtained as follows:

```
pmse_lm(fit, y = dat$y, N_train = 40)
```

We can perform variable selection based on the credibility interval criterion, excluding predictors for which the marginal credibility interval covers 0:

```
select_lm(fit, X = dat$x, prob = 0.90)
```

Finally, a plot function is included to plot point estimates and credibility intervals for various fitobjects obtained with the `stan_reg_lm` function. To use this function, combine different `stanfit` objects, for example using different priors or hyperparameter settings, in a named list:

```
fitlist <- list(fit1, fit2)
names(fitlist) <- c("fit1", "fit2")
plots <- plot_est(fitlist, est = "mean", CI = 0.95)
plots[[1]]
``` 

See also the documentation for the different functions, using e.g., `?select_lm`.


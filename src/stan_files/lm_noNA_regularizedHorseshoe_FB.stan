// This code has been adapted from Piironen & Vehtari (2017)
data{
	int N_train; //number of observations training and validation set
	int p; //number of predictors
	real y_train[N_train]; //response vector
	matrix[N_train, p] X_train; //model matrix
	//test set
	int N_test; //number of observations test set
	matrix[N_test, p] X_test; //model matrix test set
	real<lower=0> scale_global; // scale for the half-t prior for tau
	real<lower=1> nu_global; // df for the half-t prior for tau
	real<lower=1> nu_local; // df for the half-t priors for lambdas
	real<lower=0> slab_scale; // slab scale for the regularized horseshoe
	real<lower=0> slab_df; // slab df for the regularized horseshoe 
}
parameters{
	real mu; //intercept
	real<lower=0> sigma2; //error variance
	// auxiliary parameters/hyperparameters prior
	vector[p] z;
	real<lower=0> aux1_global;
	real<lower=0> aux2_global;
	vector<lower=0>[p] aux1_local;
	vector<lower=0>[p] aux2_local;
	real<lower=0> caux;
}
transformed parameters{
	real<lower=0> sigma; //error sd
	vector[N_train] linpred; //mean normal model
	real<lower=0> tau; // global shrinkage parameter
	vector<lower=0>[p] lambda; // local shrinkage parameter
	vector<lower=0>[p] lambda_tilde; // 'truncated' local shrinkage parameter
	real<lower=0> c; // slab scale
	vector[p] beta; // regression coefficients
	sigma = sqrt(sigma2);
	lambda = aux1_local .* sqrt(aux2_local);
	tau = aux1_global * sqrt(aux2_global) * scale_global * sigma;
	c = slab_scale * sqrt(caux);
	lambda_tilde = sqrt(c^2 * square(lambda) ./ (c^2 + tau^2 * square(lambda)) );
	beta = z .* lambda_tilde*tau;
	linpred = mu + X_train*beta;
}
model{
 //prior regression coefficients: regularized horseshoe
 // half-t priors for lambdas and tau; inverse-gamma for c^2
 	z ~ normal(0, 1);
 	aux1_local ~ normal(0, 1);
	aux2_local ~ inv_gamma(0.5*nu_local, 0.5*nu_local);
	aux1_global ~ normal(0, 1);
	aux2_global ~ inv_gamma(0.5*nu_global, 0.5*nu_global);
	caux ~ inv_gamma(0.5*slab_df, 0.5*slab_df);
	
 //priors nuisance parameters: uniform on log(sigma^2) & mu
	target += -2 * log(sigma); 
	
 //likelihood
	y_train ~ normal(linpred, sigma);
}
generated quantities{ //predict responses test set
	real y_test[N_test]; //predicted responses
	for(i in 1:N_test){
		y_test[i] = normal_rng(mu + X_test[i,] * beta, sigma);
	}
}	

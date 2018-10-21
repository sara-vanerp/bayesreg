data{
	int N_train; //number of observations training and validation set
	int p; //number of predictors
	real y_train[N_train]; //response vector
	matrix[N_train, p] X_train; //model matrix
	//test set
	int N_test; //number of observations test set
	matrix[N_test, p] X_test; //model matrix test set
}
parameters{
	real mu; //intercept
	real<lower=0> sigma2; //error variance
	vector[p] beta; // regression parameters
	//hyperparameters prior
	vector<lower=0>[p] tau2; //variance slab
}
transformed parameters{
	real<lower=0> sigma; //error sd
	vector[N_train] linpred; //mean normal model
	sigma = sqrt(sigma2);
	linpred = mu + X_train*beta;
}
model{
 //prior regression coefficients: normal mixture
 tau2 ~ inv_gamma(0.5, 0.5);
 
 for(j in 1:p){
 	target += log_sum_exp(bernoulli_lpmf(0 | 0.5) + normal_lpdf(beta[j] | 0, sqrt(0.001)), 
 						  bernoulli_lpmf(1 | 0.5) + normal_lpdf(beta[j] | 0, sqrt(tau2[j])));
 }		
	
	
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

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
	real<lower = 0> sigma2; //error variance
	vector[p] beta_raw; // regression parameters
	//hyperparameters prior
	vector<lower = 1>[p] tau;
	real<lower = 0> lambda1;
	real<lower = 0> lambda2;
}
transformed parameters{
	vector[p] beta;
	real<lower = 0> sigma; //error sd
	vector[N_train] linpred; //mean normal model
	for(j in 1:p){
		beta[j] = sqrt(((sigma2*(tau[j]-1))/(lambda2*tau[j]))) * beta_raw[j];
	}
	sigma = sqrt(sigma2);
	linpred = mu + X_train*beta;
}
model{
 //prior regression coefficients: elastic net
	beta_raw ~ normal(0, 1); //implies beta ~ normal(0, sqrt(1/(lambda2/sigma2 * (tau[j]/(tau[j]-1)))))
	tau ~ gamma(0.5, (8*lambda2*sigma2)/(lambda1^2));
	lambda1 ~ cauchy(0, 1);
	lambda2 ~ cauchy(0, 1);
	
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

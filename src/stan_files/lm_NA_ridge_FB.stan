data{
	int N_train; //number of observations training and validation set
	int p; //number of predictors
	vector[N_train] y_train; //response vector
	matrix[N_train, p] X_train; //model matrix
	//test set
	int N_test; //number of observations test set
	matrix[N_test, p] X_test; //model matrix test set
	//matrix or vector indices which rows and columns are missing 
	int<lower = 0> N_mis_trainY;
	int ind_mis_trainY[N_mis_trainY]; 
	int<lower = 0> N_mis_trainX;
	int ind_mis_trainX[N_mis_trainX, 2]; 
	int<lower = 0> N_mis_testX;
	int ind_mis_testX[N_mis_testX, 2];
	//scale normal distribution imputations
	real<lower=0> sd_imp;
}
parameters{
	real mu; //intercept
	real<lower = 0> sigma2; //error variance
	vector[p] beta; // regression parameters
	//hyperparameters prior
	real<lower = 0> lambda; //penalty parameter
	//missing values to impute
	vector[N_mis_trainY] impute_trainY;
	vector[N_mis_trainX] impute_trainX;
	vector[N_mis_testX] impute_testX;
}
transformed parameters{
	vector[N_train] y_train_compl; //response vector with imputed values
	matrix[N_train, p] X_train_compl; //model matrix with imputed values
	matrix[N_test, p] X_test_compl; //model matrix test set with imputed values
	real<lower = 0> tau2; //prior variance
	real<lower = 0> sigma; //error sd
	vector[N_train] linpred; //mean normal model
	matrix[N_train + N_test, p] X_compl; //combine test and train set 
	//combine imputed and observed values
	y_train_compl = y_train;
	X_train_compl = X_train;
	X_test_compl = X_test;
	for(i in 1:N_mis_trainY){ y_train_compl[ind_mis_trainY[i]] = impute_trainY[i]; }
	for(i in 1:N_mis_trainX){ X_train_compl[ind_mis_trainX[i, 1], ind_mis_trainX[i, 2]] = impute_trainX[i]; }
	for(i in 1:N_mis_testX){ X_test_compl[ind_mis_testX[i, 1], ind_mis_testX[i, 2]] = impute_testX[i]; }
	X_compl[1:N_train, ] = X_train_compl;
	X_compl[(N_train+1):(N_train+N_test), ] = X_test_compl;
	tau2 = sigma2/lambda;
	sigma = sqrt(sigma2);
	linpred = mu + X_train_compl*beta;
}
model{
	//assume normal distributions for the predictor values to impute
	impute_trainX ~ normal(0, sd_imp);
	impute_testX ~ normal(0, sd_imp); 

	//prior regression coefficients: ridge
	beta ~ normal(0, sqrt(tau2));
	lambda ~ cauchy(0, 1);
	
 //priors nuisance parameters: uniform on log(sigma^2) & mu
	target += -2 * log(sigma); 
	
 //likelihood
	y_train_compl ~ normal(linpred, sigma);
}
generated quantities{ //predict responses test set
	real y_test[N_test]; //predicted responses
	for(i in 1:N_test){
		y_test[i] = normal_rng(mu + X_test_compl[i,] * beta, sigma);
	}
}

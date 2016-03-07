data {
  int<lower=1> I;                  // number of observations
  int<lower=1> J;                  // number of parameters
  vector<lower=0,upper=1>[I] Y; // proportions
  matrix[I,J] X;                   // matrix of independent variables
  matrix[2,J] X_new;               // new matrix to get predicted values comparing lib and con
}
parameters {
  vector[J] gamma;
  vector<lower=0>[I] lambda;
}
model {
  vector[I] lnalpha;
  vector[I] beta;
  vector[I] phi;

  for(i in 1:I){
    phi[i] <- inv_logit(X[i] * gamma);
    lnalpha[i] <- log(lambda[i] * phi[i]);
    beta[i] <- lambda[i] * (1 - phi[i]);
  }
    
  Y ~ beta(exp(lnalpha), beta);
}
generated quantities {
  //matrix[2,K] Y_new;
  //Y_new <- dirichlte_rng(X * beta);
}

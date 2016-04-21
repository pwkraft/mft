data {
  int<lower=1> I;                 // number of observations
  int<lower=1> J;                 // number of parameters
  int<lower=1> K;                 // number of dimensions
  simplex[K] Y[I]; // proportions
  vector[J] X[I];                  // matrix of independent variables
  matrix[2,J] X_new;              // new matrix to get predicted values comparing lib and con
}
parameters {
  matrix[K,J] beta;
}
model {
  vector[K] alpha[I];
  for(i in 1:I){
    alpha[i] <- exp(beta * X[i]);
    Y[i] ~ dirichlet(alpha[i]);
  }
}
generated quantities {
  //matrix[2,K] Y_new;
  //Y_new <- dirichlte_rng(X * beta);
}

// The following code implements the proposed model in Stan, defining the statistical structure of 
// the model and its respective parameters.

data {

  // Sample size
  int<lower=0> N;

  // Number of elements in the design matrix, variables without commune, vaccination scheme +
  // growth and development + social security + intercept
  int<lower=0> P; // Fixed parameters
  
  // Number of levels commune
  int<lower=1> C;
  
  // Number of levels of vaccination scheme
  int<lower=1> E;
  
  // Cantidad de niveles crecimiento y desarrollo 
  int<lower=1> D;
  
  // Number of social security levels
  int<lower=1> S;

  // Number of gender levels
  int<lower=1> G;
  
  // Number of year levels
  int<lower=1> Y;
  
  // Number of CIAF index levels
  int<lower=1> I;

  int<lower=1, upper=C> Commune[N]; // Commune variable
  
  int<lower=1, upper=E> Scheme[N]; // Vaccination schedule variable 

  int<lower=1, upper=D> Development[N]; // Growth and development variable

  int<lower=1, upper=S> Security[N]; // Social security variable
  
  int<lower=1, upper=G> Gender[N]; // Gender variable
  
  int<lower=1, upper=Y> Period[N]; // Period variable
  
  int<lower=1, upper=I> Index[N]; // CIAF index variable
  
  int<lower=0,upper=1> y[N];
  
  // Predictor matrix
  matrix[N, P] X; 
}

// Model parameters
parameters {
   // coefficients vector
  vector[P] beta;
  
  vector[C] commune;
  
  vector[E] scheme; 
  
  vector[D] development; 
  
  vector[S] security; 
  
  vector[G] gender;
  
  vector[Y] period;
  
  vector[I] index;
  
  real<lower=0> sigma;
  
}

transformed parameters{
  // Transformed parameters to improve numerical stability
  
  vector[C] commune_cen;
  
  vector[E] scheme_cen;
  
  vector[D] development_cen;
  
  vector[S] security_cen;
  
  vector[G] gender_cen;
  
  vector[Y] period_cen;
  
  vector[I] index_cen;
  
  commune_cen =  commune - mean(commune);
  
  scheme_cen =  scheme - mean(scheme);
  
  development_cen =  development - mean(development);
  
  security_cen =  security - mean(security);
  
  gender_cen =  gender - mean(gender);
  
  period_cen =  period - mean(period);
  
  index_cen =  index - mean(index);
  
}

model {
  // A  priori distributions for the variables 
  commune ~ normal(0, 1);
  
  scheme ~ normal(0, 1);
  
  development ~ normal(0, 1);
  
  security ~ normal(0, 1);
  
  gender ~ normal(0,1);
  
  period ~ normal(0,1);
  
  index ~ normal(0,1);
  
  beta ~ normal(0, 1);
  
  // Likelihood
  for(i in 1:N){
y[i] ~ bernoulli_logit(
      commune_cen[Commune[i]] + scheme_cen[Scheme[i]] + development_cen[Development[i]] +
      security_cen[Security[i]] + gender_cen[Gender[i]] +
      period_cen[Period[i]] + index_cen[Index[i]] + X[i] * beta 
    );
    }
}

// Variance associated to each qualitative predictor:
generated quantities {
  
real<lower=0>  S_commune;
real<lower=0>  S_scheme;
real<lower=0>  S_development;
real<lower=0>  S_security;
real<lower=0> S_gender;
real<lower=0> S_period;
real<lower=0> S_index;

 matrix[C, C] C1; 
 row_vector[C] b1 = rep_vector(1.0, C)';
  matrix[E, E] E1;
 row_vector[E] b2 = rep_vector(1.0, E)';
  matrix[D, D] D1;
 row_vector[D] b3 = rep_vector(1.0, D)';
  matrix[S, S] S1;
 row_vector[S] b4 = rep_vector(1.0, S)';
  matrix[I, I] I1;
 row_vector[I] b5 = rep_vector(1.0, I)';
  matrix[G, G] G1;
 row_vector[G] b6 = rep_vector(1.0, G)';
  matrix[Y, Y] Y1;
 row_vector[Y] b7 = rep_vector(1.0, Y)';

 for(j in 1:C) C1[j] = b1;
 for(j in 1:E) E1[j] = b2;
 for(j in 1:D) D1[j] = b3;
 for(j in 1:S) S1[j] = b4;
 for(j in 1:I) I1[j] = b5;
 for(j in 1:G) G1[j] = b6;
 for(j in 1:Y) Y1[j] = b7;

 S_commune = pow(pow(C-1, -1)*commune_cen'*(diag_matrix(rep_vector(1.0, C)) - pow(C, -1)*C1)*commune_cen, 0.5);
 S_scheme = pow(pow(E-1, -1)*scheme_cen'*(diag_matrix(rep_vector(1.0, E)) - pow(E, -1)*E1)*scheme_cen, 0.5);
 S_development = pow(pow(D-1, -1)*development_cen'*(diag_matrix(rep_vector(1.0, D)) - pow(D, -1)*D1)*development_cen, 0.5);
 S_security = pow(pow(S-1, -1)*security_cen'*(diag_matrix(rep_vector(1.0, S)) - pow(S, -1)*S1)*security_cen, 0.5);
 S_gender = pow(pow(G-1, -1)*gender_cen'*(diag_matrix(rep_vector(1.0, G)) - pow(G, -1)*G1)*gender_cen, 0.5);
 S_period = pow(pow(Y-1, -1)*period_cen'*(diag_matrix(rep_vector(1.0, Y)) - pow(Y, -1)*Y1)*period_cen, 0.5);
 S_index = pow(pow(I-1, -1)*index_cen'*(diag_matrix(rep_vector(1.0, I)) - pow(I, -1)*I1)*index_cen, 0.5);
}


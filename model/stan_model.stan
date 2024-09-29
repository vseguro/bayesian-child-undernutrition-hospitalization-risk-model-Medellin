data {

  // Tamaño de la muestra
  int<lower=0> N;

  // Número de elementos en la matriz de diseño, variables sin comuna, esq + crec + tipo_ss + intercepto
  int<lower=0> P; // Parámetros fijos
  
  // Cantidad de niveles comuna
  int<lower=1> C;
  
  // Cantidad de niveles esquema
  int<lower=1> E;
  
  // Cantidad de niveles crecimiento y desarrollo 
  int<lower=1> D;
  
  // Cantidad de niveles seguridad social
  int<lower=1> S;

  // Número de niveles del género
  int<lower=1> G;
  
  // Número de niveles del año
  int<lower=1> Y;

  int<lower=1, upper=C> Commune[N]; // Variable comuna 
  
  int<lower=1, upper=E> Scheme[N]; // Variable esquema de vacunación 

  int<lower=1, upper=D> Development[N]; // Variable crecimiento y desarrollo

  int<lower=1, upper=S> Security[N]; // Variable comuna 
  
  int<lower=1, upper=G> Gender[N]; 
  
  int<lower=1, upper=Y> Period[N];
  
  int<lower=0,upper=1> y[N];
  
  // variables de entrada x, matriz de predictores
  matrix[N, P] X; 
}

// Parámetros del modelo
parameters {
   // vector de coeficientes
  vector[P] beta;
  
  vector[C] commune; // comunas
  
  vector[E] scheme; 
  
  vector[D] development; 
  
  vector[S] security; 
  
  vector[G] gender;
  
  vector[Y] period;
  
  real<lower=0> sigma;
  
}

transformed parameters{
  vector[C] commune_cen;
  
  vector[E] scheme_cen;
  
  vector[D] development_cen;
  
  vector[S] security_cen;
  
  vector[G] gender_cen;
  
  vector[Y] period_cen;
  
  commune_cen =  commune - mean(commune); // sum = 0, estabilidad numérica
  
  scheme_cen =  scheme - mean(scheme);
  
  development_cen =  development - mean(development);
  
  security_cen =  security - mean(security);
  
  gender_cen =  gender - mean(gender);
  
  period_cen =  period - mean(period);
  
}

model {
  // A prioris
  commune ~ normal(0, 1);
  
  scheme ~ normal(0, 1);
  
  development ~ normal(0, 1);
  
  security ~ normal(0, 1);
  
  gender ~ normal(0,1);
  
  period ~ normal(0,1);
  
  beta ~ normal(0, 1);
  
  // Verosimilitud
  for(i in 1:N){
y[i] ~ bernoulli_logit(
      commune_cen[Commune[i]] + scheme_cen[Scheme[i]] + development_cen[Development[i]] +
      security_cen[Security[i]] + gender_cen[Gender[i]] +
      period_cen[Period[i]] + X[i] * beta 
    );
    }
}

generated quantities{
real<lower=0>  S_commune;
real<lower=0>  S_scheme;
real<lower=0>  S_development;
real<lower=0>  S_security;
real<lower=0> S_gender;
real<lower=0> S_period;
// 
 matrix[C, C] C1; // comunas
 row_vector[C] b1 = rep_vector(1.0, C)';
  matrix[E, E] E1;
 row_vector[E] b2 = rep_vector(1.0, E)';
  matrix[D, D] D1;
 row_vector[D] b3 = rep_vector(1.0, D)';
  matrix[S, S] S1;
 row_vector[S] b4 = rep_vector(1.0, S)';
  matrix[G, G] G1;
 row_vector[G] b6 = rep_vector(1.0, G)';
  matrix[Y, Y] Y1;
 row_vector[Y] b7 = rep_vector(1.0, Y)';

 for(j in 1:C) C1[j] = b1;
 for(j in 1:E) E1[j] = b2;
 for(j in 1:D) D1[j] = b3;
 for(j in 1:S) S1[j] = b4;
 for(j in 1:G) G1[j] = b6;
 for(j in 1:Y) Y1[j] = b7;

 S_commune = pow(pow(C-1, -1)*commune_cen'*(diag_matrix(rep_vector(1.0, C)) - pow(C, -1)*C1)*commune_cen, 0.5);
 S_scheme = pow(pow(E-1, -1)*scheme_cen'*(diag_matrix(rep_vector(1.0, E)) - pow(E, -1)*E1)*scheme_cen, 0.5);
 S_development = pow(pow(D-1, -1)*development_cen'*(diag_matrix(rep_vector(1.0, D)) - pow(D, -1)*D1)*development_cen, 0.5);
 S_security = pow(pow(S-1, -1)*security_cen'*(diag_matrix(rep_vector(1.0, S)) - pow(S, -1)*S1)*security_cen, 0.5);
 S_gender = pow(pow(G-1, -1)*gender_cen'*(diag_matrix(rep_vector(1.0, G)) - pow(G, -1)*G1)*gender_cen, 0.5);
 S_period = pow(pow(Y-1, -1)*period_cen'*(diag_matrix(rep_vector(1.0, Y)) - pow(Y, -1)*Y1)*period_cen, 0.5);
}


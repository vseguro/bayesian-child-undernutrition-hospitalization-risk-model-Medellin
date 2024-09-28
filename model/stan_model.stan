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

  int<lower=1, upper=C> Comuna[N]; // Variable comuna 
  
  int<lower=1, upper=E> Esquema[N]; // Variable esquema de vacunación 

  int<lower=1, upper=D> Desarrollo[N]; // Variable crecimiento y desarrollo

  int<lower=1, upper=S> Seguridad[N]; // Variable comuna 
  
  int<lower=1, upper=G> Genero[N]; 
  
  int<lower=1, upper=Y> Periodo[N];
  
  int<lower=0,upper=1> y[N];
  
  // variables de entrada x, matriz de predictores
  matrix[N, P] X; 
}

// Parámetros del modelo
parameters {
   // vector de coeficientes
  vector[P] beta;
  
  vector[C] comuna; // comunas
  
  vector[E] esquema; 
  
  vector[D] desarrollo; 
  
  vector[S] seguridad; 
  
  vector[G] genero;
  
  vector[Y] periodo;
  
  real<lower=0> sigma;
  
}

transformed parameters{
  vector[C] comuna_cen;
  
  vector[E] esquema_cen;
  
  vector[D] desarrollo_cen;
  
  vector[S] seguridad_cen;
  
  vector[G] genero_cen;
  
  vector[Y] periodo_cen;
  
  comuna_cen =  comuna - mean(comuna); // sum = 0, estabilidad numérica
  
  esquema_cen =  esquema - mean(esquema);
  
  desarrollo_cen =  desarrollo - mean(desarrollo);
  
  seguridad_cen =  seguridad - mean(seguridad);
  
  genero_cen =  genero - mean(genero);
  
  periodo_cen =  periodo - mean(periodo);
  
}

model {
  // A prioris
  comuna ~ normal(0, 1);
  
  esquema ~ normal(0, 1);
  
  desarrollo ~ normal(0, 1);
  
  seguridad ~ normal(0, 1);
  
  genero ~ normal(0,1);
  
  periodo ~ normal(0,1);
  
  beta ~ normal(0, 1);
  
  // Verosimilitud
  for(i in 1:N){
y[i] ~ bernoulli_logit(
      comuna_cen[Comuna[i]] + esquema_cen[Esquema[i]] + desarrollo_cen[Desarrollo[i]] +
      seguridad_cen[Seguridad[i]] + genero_cen[Genero[i]] +
      periodo_cen[Periodo[i]] + X[i] * beta 
    );
    }
}

generated quantities{
real<lower=0>  S_comuna;
real<lower=0>  S_esquema;
real<lower=0>  S_crecimiento;
real<lower=0>  S_seguridad;
real<lower=0> S_genero;
real<lower=0> S_periodo;
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

 S_comuna = pow(pow(C-1, -1)*comuna_cen'*(diag_matrix(rep_vector(1.0, C)) - pow(C, -1)*C1)*comuna_cen, 0.5);
 S_esquema = pow(pow(E-1, -1)*esquema_cen'*(diag_matrix(rep_vector(1.0, E)) - pow(E, -1)*E1)*esquema_cen, 0.5);
 S_crecimiento = pow(pow(D-1, -1)*desarrollo_cen'*(diag_matrix(rep_vector(1.0, D)) - pow(D, -1)*D1)*desarrollo_cen, 0.5);
 S_seguridad = pow(pow(S-1, -1)*seguridad_cen'*(diag_matrix(rep_vector(1.0, S)) - pow(S, -1)*S1)*seguridad_cen, 0.5);
 S_genero = pow(pow(G-1, -1)*genero_cen'*(diag_matrix(rep_vector(1.0, G)) - pow(G, -1)*G1)*genero_cen, 0.5);
 S_periodo = pow(pow(Y-1, -1)*periodo_cen'*(diag_matrix(rep_vector(1.0, Y)) - pow(Y, -1)*Y1)*periodo_cen, 0.5);
}


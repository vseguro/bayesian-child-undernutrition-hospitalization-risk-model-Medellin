library(rstan)
library(HDInterval)
library(dplyr)
library(ROCR)

# NOTA: cambiar dirección del directorio
datos_final <-   read.csv("acute_malnutrition_final_data.csv", sep = ",", header = TRUE, encoding = "UTF-8")
datos_final <- datos_final[ , -1]  

y <- datos_final$pac_hos_ 
x1 <-(datos_final$edad_ - mean(datos_final$edad_)) #Mejorar la convergencia
x3 <- (datos_final$peso_act - mean(datos_final$peso_act))
x4 <- (datos_final$talla_act - mean(datos_final$talla_act))
x5 <- (datos_final$edad_ges - mean(datos_final$edad_ges))

X_models12 <- model.matrix(~ x1 + x3 + x4 + x5) #matriz de diseño
head (X_models12)

stan_data_models12 <- list(
  
  "X" = X_models12,
  
  "y" = y,
  
  "N" = nrow(datos_final), # número de observaciones
  
  "P" = ncol(X_models12),  # número de parámetros fijos
  
  "C" = 22, # número de comunas
  
  "E" = 3, # niveles del esquema de vacunación, (sí, no, desconocido) 
  
  "D" = 2, # niveles de crecimiento y desarrollo, (sí, no) 
  
  "S" = 5, # niveles de seguridad social, (subsidiado, contributivo, pendiente, especial, no asegurado)
  
  "G" = 2, # Niveles de género
  
  "Y" = 8, # Niveles año
  
  "Commune" = datos_final$comuna,
  
  "Scheme" = datos_final$esq_vac,
  
  "Development" = datos_final$crec_dllo,
  
  "Security" = datos_final$tipo_ss_, 
  
  "Gender" = datos_final$sexo_,
  
  "Period" = datos_final$year_
  
)

# NOTA: cambiar a dirección del directorio
fit_models12 <- stan(file = file.choose(), data = stan_data_models12, chains = 3, iter = 200000, warmup = 80000, cores = 3, thin = 4 )

#summary de los betas (variables continuas)
print(fit_models12, pars = "beta")

#summary de las comunas
print(fit_models12, pars = "commune_cen")

#summary del esquema de vacunación
print(fit_models12, pars = "scheme_cen")

#summary de  crecimiento y desarrollo
print(fit_models12, pars = "development_cen")

#summary de seguridad social
print(fit_models12, pars = "security_cen")

#summary de genero
print(fit_models12, pars = "gender_cen")

#summary de año
print(fit_models12, pars = "period_cen")


#Se extraen los valores simulados en txt
beta_poste <- extract(fit_models12, pars = "beta")
commune_poste <- extract(fit_models12, pars = "commune_cen")
scheme_poste <- extract(fit_models12, pars = "scheme_cen")
development_poste <- extract(fit_models12, pars = "development_cen")
security_poste <- extract(fit_models12, pars = "security_cen")
gender_poste <- extract(fit_models12, pars = "gender_cen")
period_poste <- extract(fit_models12, pars = "period_cen")
{
# IMPORTANTE: Cambiar dirección de archivo para guardar los resultados
beta_data <- as.data.frame(beta_poste) 
write(x=t(beta_data), file='/home/ruser/Definitive_model/Resultados/posterior_betas.txt', ncolumns=5, append=TRUE)
commune_data <- as.data.frame(commune_poste) 
write(x=t(commune_data), file='/home/ruser/Definitive_model/Resultados/posterior_communes.txt', ncolumns=22, append=TRUE)
scheme_data <- as.data.frame(scheme_poste) 
write(x=t(scheme_data), file='/home/ruser/Definitive_model/Resultados/posterior_schemes.txt', ncolumns=3, append=TRUE)
development_data <- as.data.frame(development_poste) 
write(x=t(development_data), file='/home/ruser/Definitive_model/Resultados/posterior_growth.txt', ncolumns=2, append=TRUE)
security_data <- as.data.frame(security_poste) 
write(x=t(security_data), file='/home/ruser/Definitive_model/Resultados/posterior_security.txt', ncolumns=5, append=TRUE)
gender_data <- as.data.frame(gender_poste) 
write(x=t(gender_data), file='/home/ruser/Definitive_model/Resultados/posterior_gender.txt', ncolumns=2, append=TRUE)
period_data <- as.data.frame(period_poste) 
write(x=t(period_data), file='/home/ruser/Definitive_model/Resultados/posterior_period.txt', ncolumns=8, append=TRUE)


# traceplots 
traceplot(fit_models12, pars = "beta")
traceplot(fit_models12, pars = "commune_cen")
traceplot(fit_models12, pars = "scheme_cen")
traceplot(fit_models12, pars = "development_cen")
traceplot(fit_models12, pars = "security_cen")
traceplot(fit_models12, pars = "gender_cen")
traceplot(fit_models12, pars = "period_cen")

# IMPORTANTE: Cambiar dirección de archivo para guardar los resultados
# Se guarda los s_alpha 
commune_sim <- c(fit_models12@sim[[1]][[1]]$S_commune,fit_models12@sim[[1]][[2]]$S_commune ,fit_models12@sim[[1]][[3]]$S_commune)
write(x=t(commune_sim), file='/home/ruser/Definitive_model/Resultados/S_commune.txt', ncolumns=1, append=TRUE)

scheme_sim <- c(fit_models12@sim[[1]][[1]]$S_scheme,fit_models12@sim[[1]][[2]]$S_scheme,fit_models12@sim[[1]][[3]]$S_scheme)
write(x=t(scheme_sim), file='/home/ruser/Definitive_model/Resultados/S_scheme.txt', ncolumns=1, append=TRUE)

development_sim <- c(fit_models12@sim[[1]][[1]]$S_development,fit_models12@sim[[1]][[2]]$S_development,fit_models12@sim[[1]][[3]]$S_development)
write(x=t(development_sim), file='/home/ruser/Definitive_model/Resultados/S_development.txt', ncolumns=1, append=TRUE)

security_sim <- c(fit_models12@sim[[1]][[1]]$S_security,fit_models12@sim[[1]][[2]]$S_security,fit_models12@sim[[1]][[3]]$S_security)
write(x=t(security_sim), file='/home/ruser/Definitive_model/Resultados/S_security.txt', ncolumns=1, append=TRUE)

gender_sim <- c(fit_models12@sim[[1]][[1]]$S_gender,fit_models12@sim[[1]][[2]]$S_gender,fit_models12@sim[[1]][[3]]$S_gender)
write(x=t(gender_sim), file='/home/ruser/Definitive_model/Resultados/S_gender.txt', ncolumns=1, append=TRUE)

period_sim <- c(fit_models12@sim[[1]][[1]]$S_period,fit_models12@sim[[1]][[2]]$S_period,fit_models12@sim[[1]][[3]]$S_period)
write(x=t(period_sim), file='/home/ruser/Definitive_model/Resultados/S_period.txt', ncolumns=1, append=TRUE)
}

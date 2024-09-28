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
  
  "Comuna" = datos_final$comuna,
  
  "Esquema" = datos_final$esq_vac,
  
  "Desarrollo" = datos_final$crec_dllo,
  
  "Seguridad" = datos_final$tipo_ss_, 
  
  "Genero" = datos_final$sexo_,
  
  "Periodo" = datos_final$year_
  
)

# NOTA: cambiar a dirección del directorio
fit_models12 <- stan(file = file.choose(), data = stan_data_models12, chains = 3, iter = 200000, warmup = 80000, cores = 3, thin = 4 )

#summary de los betas (variables continuas)
print(fit_models12, pars = "beta")

#summary de las comunas
print(fit_models12, pars = "comuna_cen")

#summary del esquema de vacunación
print(fit_models12, pars = "esquema_cen")

#summary de  crecimiento y desarrollo
print(fit_models12, pars = "desarrollo_cen")

#summary de seguridad social
print(fit_models12, pars = "seguridad_cen")

#summary de genero
print(fit_models12, pars = "genero_cen")

#summary de año
print(fit_models12, pars = "periodo_cen")


#Se extraen los valores simulados en txt
Beta.poste <- extract(fit_models12, pars = "beta")
comuna.poste <- extract(fit_models12, pars = "comuna_cen")
esquema.poste <- extract(fit_models12, pars = "esquema_cen")
desarrollo.poste <- extract(fit_models12, pars = "desarrollo_cen")
seguridad.poste <- extract(fit_models12, pars = "seguridad_cen")
genero.poste <- extract(fit_models12, pars = "genero_cen")
periodo.poste <- extract(fit_models12, pars = "periodo_cen")
{
# IMPORTANTE: Cambiar dirección de archivo para guardar los resultados
beta.data <- as.data.frame(Beta.poste) 
write(x=t(beta.data), file='/home/ruser/Definitive_model/Resultados/Betas posterior.txt', ncolumns=5, append=TRUE)
comuna.data <- as.data.frame(comuna.poste) 
write(x=t(comuna.data), file='/home/ruser/Definitive_model/Resultados/Comunas posterior.txt', ncolumns=22, append=TRUE)
esquema.data <- as.data.frame(esquema.poste) 
write(x=t(esquema.data), file='/home/ruser/Definitive_model/Resultados/Esquemas posterior.txt', ncolumns=3, append=TRUE)
desarrollo.data <- as.data.frame(desarrollo.poste) 
write(x=t(desarrollo.data), file='/home/ruser/Definitive_model/Resultados/Crecimiento posterior.txt', ncolumns=2, append=TRUE)
seguridad.data <- as.data.frame(seguridad.poste) 
write(x=t(seguridad.data), file='/home/ruser/Definitive_model/Resultados/Seguridad posterior.txt', ncolumns=5, append=TRUE)
genero.data <- as.data.frame(genero.poste) 
write(x=t(genero.data), file='/home/ruser/Definitive_model/Resultados/Genero posterior.txt', ncolumns=2, append=TRUE)
periodo.data <- as.data.frame(periodo.poste) 
write(x=t(periodo.data), file='/home/ruser/Definitive_model/Resultados/Periodo posterior.txt', ncolumns=8, append=TRUE)


# traceplots 
traceplot(fit_models12, pars = "beta")
traceplot(fit_models12, pars = "comuna_cen")
traceplot(fit_models12, pars = "esquema_cen")
traceplot(fit_models12, pars = "desarrollo_cen")
traceplot(fit_models12, pars = "seguridad_cen")
traceplot(fit_models12, pars = "genero_cen")
traceplot(fit_models12, pars = "periodo_cen")

# IMPORTANTE: Cambiar dirección de archivo para guardar los resultados
# Se guarda los s_alpha 
comuna_sim <- c(fit_models12@sim[[1]][[1]]$S_comuna,fit_models12@sim[[1]][[2]]$S_comuna ,fit_models12@sim[[1]][[3]]$S_comuna)
write(x=t(comuna_sim), file='/home/ruser/Definitive_model/Resultados/S_comunas.txt', ncolumns=1, append=TRUE)

esq_sim <- c(fit_models12@sim[[1]][[1]]$S_esquema,fit_models12@sim[[1]][[2]]$S_esquema,fit_models12@sim[[1]][[3]]$S_esquema)
write(x=t(esq_sim), file='/home/ruser/Definitive_model/Resultados/S_esq.txt', ncolumns=1, append=TRUE)

crec_sim <- c(fit_models12@sim[[1]][[1]]$S_crecimiento,fit_models12@sim[[1]][[2]]$S_crecimiento,fit_models12@sim[[1]][[3]]$S_crecimiento)
write(x=t(crec_sim), file='/home/ruser/Definitive_model/Resultados/S_crec.txt', ncolumns=1, append=TRUE)

seg_sim <- c(fit_models12@sim[[1]][[1]]$S_seguridad,fit_models12@sim[[1]][[2]]$S_seguridad,fit_models12@sim[[1]][[3]]$S_seguridad)
write(x=t(seg_sim), file='/home/ruser/Definitive_model/Resultados/S_seg.txt', ncolumns=1, append=TRUE)

gen_sim <- c(fit_models12@sim[[1]][[1]]$S_genero,fit_models12@sim[[1]][[2]]$S_genero,fit_models12@sim[[1]][[3]]$S_genero)
write(x=t(gen_sim), file='/home/ruser/Definitive_model/Resultados/S_genero.txt', ncolumns=1, append=TRUE)

per_sim <- c(fit_models12@sim[[1]][[1]]$S_periodo,fit_models12@sim[[1]][[2]]$S_periodo,fit_models12@sim[[1]][[3]]$S_periodo)
write(x=t(per_sim), file='/home/ruser/Definitive_model/Resultados/S_periodo.txt', ncolumns=1, append=TRUE)
}

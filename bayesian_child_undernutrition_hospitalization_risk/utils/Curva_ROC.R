library(ROCR)
source("utils/load_data.R")
##### ---------------------------- MATRIZ DE DISEÑO -------------------------------------------- ###
datos_final <-   read.csv("acute_malnutrition_final_data.csv", sep = ",", header = TRUE, encoding = "UTF-8")
datos_final <- datos_final[ , -1]  

y <- datos_final$pac_hos_ 
x1 <-(datos_final$edad_ - mean(datos_final$edad_))
x3 <- (datos_final$peso_act - mean(datos_final$peso_act))
x4 <- (datos_final$talla_act - mean(datos_final$talla_act))
x5 <- (datos_final$edad_ges - mean(datos_final$edad_ges))

X_models12 <- model.matrix(~ x1 + x3 + x4 + x5) 
head (X_models12)

# IMPORTANTE: Cambiar dirección de archivo para guardar los resultados
# Lecturas de los .txt con los betas:
betas.pos <-   read.table("~/Definitive_model/Resultados/Betas posterior.txt", quote="\"", comment.char="")
esquema.pos <- read.table("~/Definitive_model/Resultados/Esquemas posterior.txt", quote="\"", comment.char="")
desarrollo.pos <- read.table("~/Definitive_model/Resultados/Crecimiento posterior.txt", quote="\"", comment.char="")
tiposs.pos <- read.table("~/Definitive_model/Resultados/Seguridad posterior.txt", quote="\"", comment.char="")
comunas.pos <- read.table("~/Definitive_model/Resultados/Comunas posterior.txt", quote="\"", comment.char="")
genero.pos <- read.table("~/Definitive_model/Resultados/Genero posterior.txt", quote="\"", comment.char="")
periodo.pos <- read.table("~/Definitive_model/Resultados/Periodo posterior.txt", quote="\"", comment.char="")


## ------------------------------
# Sample:
betas.pos <- sapply(betas.pos, sample, size = 20000,replace = TRUE)
esquema.pos <- sapply(esquema.pos, sample, size = 20000,replace = TRUE)
desarrollo.pos <- sapply(desarrollo.pos, sample, size = 20000,replace = TRUE)
tiposs.pos <- sapply(tiposs.pos, sample, size = 20000,replace = TRUE)
comunas.pos <- sapply(comunas.pos, sample, size = 20000,replace = TRUE)
genero.pos <- sapply(genero.pos, sample, size = 20000,replace = TRUE)
periodo.pos <- sapply(periodo.pos, sample, size = 20000,replace = TRUE)

#############
#Curva:
Py2 = sapply(1:dim(X_models12)[1], 
             function(j){median(sapply(1:dim(betas.pos)[1], 
                                       function(i){exp(X_models12[j,]%*%as.numeric(betas.pos[i,]) +
                                                         as.numeric(comunas.pos[i,datos_final$comuna[j]]) + 
                                                         as.numeric(esquema.pos[i,datos_final$esq_vac[j]]) + 
                                                         as.numeric(desarrollo.pos[i,datos_final$crec_dllo[j]]) + 
                                                         as.numeric(tiposs.pos[i, datos_final$tipo_ss_[j]])+
                                                         as.numeric(genero.pos[i, datos_final$sexo_[j]]) +
                                                         as.numeric(periodo.pos[i, datos_final$year_[j]]) 
                                                         )/
                                           (1 + exp(X_models12[j,]%*%as.numeric(betas.pos[i,]) + 
                                                      as.numeric(comunas.pos[i,datos_final$comuna[j]]) +  
                                                      as.numeric(esquema.pos[i,datos_final$esq_vac[j]]) + 
                                                      as.numeric(desarrollo.pos[i,datos_final$crec_dllo[j]]) + 
                                                      as.numeric(tiposs.pos[i,datos_final$tipo_ss_[j]])+
                                                      as.numeric(genero.pos[i, datos_final$sexo_[j]]) +
                                                      as.numeric(periodo.pos[i, datos_final$year_[j]]) 
                                                      ))}))})
plot(density(Py2)) # Más eficiencia con funciones vectorizadas.
ROCR.simple2c = list(prediccionesc = Py2, labels = y)
dfc <- data.frame(ROCR.simple2c)
predc <- prediction(dfc$prediccionesc, dfc$labels)
perfc<- performance(predc,"tpr","fpr")
perf_auc <- performance(predc,"auc")
perf_auc@y.values # AUC
plot(perfc,colorize=TRUE,type="l", main = "Curva ROC")
abline(a=0,b=1,col="red")

############

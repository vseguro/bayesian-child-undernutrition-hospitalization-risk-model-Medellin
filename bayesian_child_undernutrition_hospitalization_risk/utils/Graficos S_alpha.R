library(ggplot2)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(HDInterval)
source("utils/load_data.R")

######### función cuantiles al 90%
f <- function(x) {
  r <- quantile(x, probs = c(0, 0.05, 0.5, 0.95, 1)) 
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

######### S_alpha:
S_comuna <- LoadData("S_comunas.txt", data_type = "processed/Definitive_model")
S_comuna <- remove_outliers(S_comuna$V1)
S_comuna <- as.data.frame(S_comuna)
colnames(S_comuna) <- "V1"

S_crec <- LoadData("S_crec.txt", data_type = "processed/Definitive_model")
S_crec <- remove_outliers(S_crec$V1)
S_crec <- as.data.frame(S_crec)
colnames(S_crec) <- "V1"

S_seg <- LoadData("S_seg.txt", data_type = "processed/Definitive_model")
S_seg <- remove_outliers(S_seg$V1)
S_seg <- as.data.frame(S_seg)
colnames(S_seg) <- "V1"

S_esq <- LoadData("S_esq.txt", data_type = "processed/Definitive_model")
S_esq <- remove_outliers(S_esq$V1)
S_esq <- as.data.frame(S_esq)
colnames(S_esq) <- "V1"

S_gen <- LoadData("S_genero.txt", data_type = "processed/Definitive_model")
S_gen <- remove_outliers(S_gen$V1)
S_gen <- as.data.frame(S_gen)
colnames(S_gen) <- "V1"

S_per <- LoadData("S_periodo.txt", data_type = "processed/Definitive_model")
S_per <- remove_outliers(S_per$V1)
S_per <- as.data.frame(S_per)
colnames(S_per) <- "V1"

S_alphas <- rbind(S_comuna,S_crec,S_seg,S_esq,S_gen,S_per) # se unen en una matriz
grupos <- data.frame(rep(c("Commune(22)","Growth and \n development program(2)","Type of Social \n security(5)","Vaccination \n schedule(3)","Gender(2)", "Year(8)"), each = 150000)) # se identifica al grupo que pertencen (comuna, esquema, ...)
S_alphas_grup <- cbind(S_alphas,grupos) 
colnames(S_alphas_grup) <- c("S_alpha","Grupo") # se asignan los nombres

######### Anova
Anova <- ggplot(S_alphas_grup, aes(x=Grupo, y=S_alpha)) + 
  #geom_boxplot(fill='steelblue',width = 0.03,position = position_dodge(width=0.5)) +
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.03,position = position_dodge(width=0.8))+
  stat_summary(fun=median, geom="point", shape=21, size=3, col = "black",bg="cadetblue2")+
  theme(aspect.ratio = .6)+
  labs(y = expression(S[alpha]), x = "", title = "Bayesian ANOVA")+
  coord_flip()

Anova

######### Betas

betas.pos <- exp(LoadData("Betas posterior.txt", data_type = "processed/Definitive_model"))
esquema.pos <- exp(LoadData("Esquemas posterior.txt", data_type = "processed/Definitive_model"))
desarrollo.pos <- exp(LoadData("Crecimiento posterior.txt", data_type = "processed/Definitive_model"))
tiposs.pos <- exp(LoadData("Seguridad posterior.txt", data_type = "processed/Definitive_model"))
comunas.pos <- exp(LoadData("Comunas posterior.txt", data_type = "processed/Definitive_model"))
genero.pos <- exp(LoadData("Genero posterior.txt", data_type = "processed/Definitive_model"))
periodo.pos <- exp(LoadData("Periodo posterior.txt", data_type = "processed/Definitive_model"))

######### Nombres de las variables
colnames(betas.pos) <- c("Intercept", "Age", "Weight", "length/height", "Gestational age")
colnames(esquema.pos) <- c("Unknown", "No", "Yes")
colnames(desarrollo.pos) <- c("Yes","No")
colnames(tiposs.pos) <- c("Contributory", "Special", "Uninsured", "Pending", "Subsidized")
colnames(comunas.pos) <- c("Altavista", "Aranjuez",                     
                           "Belen","Buenos Aires",                  
                           "Castilla","Corregimiento de San Cristobal",
                           "Corregimiento De Santa Elena","Doce de Octubre",               
                           "El Poblado","Guayabal",                      
                           "La America","La Candelaria",                 
                           "Laureles","Manrique",                      
                           "Popular","Robledo",                       
                           "San Antonio de Prado","San Javier",                   
                           "San Sebastian de Palmitas", "Santa Cruz", 
                           "Villa Hermosa","No information")
colnames(genero.pos) <- c("F", "M")
colnames(periodo.pos) <- c("2016", "2017","2018","2019","2020","2021","2022","2023")

######### Organizar las bases con gather
esquema.pos2 <- gather(esquema.pos,"Unknown", "No", "Yes", key = "Tipo",value = "Beta")
desarrollo.pos2 <- gather(desarrollo.pos, "No", "Yes", key = "Tipo", value = "Beta")
tiposs.pos2 <- gather(tiposs.pos,"Contributory", "Special", "Uninsured", "Pending", "Subsidized", key = "Tipo", value = "Beta")
comunas.pos2 <- gather(comunas.pos,"Altavista", "Aranjuez",                     
                       "Belen","Buenos Aires",                  
                       "Castilla","Corregimiento de San Cristobal",
                       "Corregimiento De Santa Elena","Doce de Octubre",               
                       "El Poblado","Guayabal",                      
                       "La America","La Candelaria",                 
                       "Laureles","Manrique",                      
                       "Popular","Robledo",                       
                       "San Antonio de Prado","San Javier",                   
                       "San Sebastian de Palmitas", "Santa Cruz",                    
                       "Villa Hermosa","No information", key = "Comuna", value = "Beta")
genero.pos2 <- gather(genero.pos,"F", "M", key = "Sexo", value = "Beta")
periodo.pos2 <- gather(periodo.pos,"2016", "2017","2018","2019","2020","2021","2022","2023", key = "Periodo", value = "Beta")

#### -------------------------------------- Con cuantiles: --------------------------------##
######### Comunas:
comuna <- ggplot(comunas.pos2, aes(x=Comuna, y=Beta)) + 
  #geom_boxplot(fill='steelblue',width = 0.03,position = position_dodge(width=0.5)) +
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.1,position = position_dodge(width=0.5))+
  stat_summary(fun=median, geom="point", shape=21, size=4, col = "black",bg="cadetblue2")+
  geom_hline(aes(yintercept=1), linetype="dashed", color = "blue",size = 0.5)+
  labs(title = "Communes", y= expression(gamma), x = "")+
  theme(aspect.ratio = .8)+
  coord_flip()

######### Esquema:
Esquema <- ggplot(esquema.pos2, aes(x=Tipo, y=Beta)) + 
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.03,position = position_dodge(width=0.5))+
  #geom_boxplot(fill='steelblue',width = 0.03,position = position_dodge(width=0.5)) +
  stat_summary(fun=median, geom="point", shape=21, size=5, col = "black",bg="cadetblue2")+
  geom_hline(aes(yintercept=1), linetype="dashed", color = "blue")+
  labs(title = "Vaccination schedule", y= expression(gamma), x = "")+
  theme(aspect.ratio = .5)+
  coord_flip()

######### Desarrollo:
Desarrollo <- ggplot(desarrollo.pos2, aes(x=Tipo, y=Beta)) + 
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.03,position = position_dodge(width=0.5))+
  #geom_boxplot(fill='steelblue',width = 0.03,position = position_dodge(width=0.5)) +
  stat_summary(fun=median, geom="point", shape=21, size=5, col = "black",bg="cadetblue2")+
  geom_hline(aes(yintercept=1), linetype="dashed", color = "blue")+
  labs(title = "Growth and development program", y= expression(gamma), x = "")+
  theme(aspect.ratio = .5)+
  coord_flip()

######### Seguridad social:
Seguridad <- ggplot(tiposs.pos2, aes(x=Tipo, y=Beta)) + 
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.04,position = position_dodge(width=0.5))+
  #geom_boxplot(fill='steelblue',width = 0.03,position = position_dodge(width=0.5)) +
  stat_summary(fun=median, geom="point", shape=21, size=5, col = "black",bg="cadetblue2")+
  geom_hline(aes(yintercept=1), linetype="dashed", color = "blue")+
  labs(title = "Type of social security", y= expression(gamma), x = "")+
  theme(aspect.ratio = .5)+
  coord_flip()

######### Genero
Genero <- ggplot(genero.pos2, aes(x=Sexo, y=Beta)) + 
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.04,position = position_dodge(width=0.5))+
  #geom_boxplot(fill='steelblue',width = 0.03,position = position_dodge(width=0.5)) +
  stat_summary(fun=median, geom="point", shape=21, size=5, col = "black",bg="cadetblue2")+
  geom_hline(aes(yintercept=1), linetype="dashed", color = "blue")+
  labs(title = "Gender", y= expression(gamma), x = "")+
  theme(aspect.ratio = .5)+
  coord_flip()

######### Periodo
Periodo <- ggplot(periodo.pos2, aes(x=Periodo, y=Beta)) + 
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.04,position = position_dodge(width=0.5))+
  #geom_boxplot(fill='steelblue',width = 0.03,position = position_dodge(width=0.5)) +
  stat_summary(fun=median, geom="point", shape=21, size=5, col = "black",bg="cadetblue2")+
  geom_hline(aes(yintercept=1), linetype="dashed", color = "blue")+
  labs(title = "Year", y= expression(gamma), x = "")+
  theme(aspect.ratio = .5)+
  coord_flip()
#### ------------------------------------ HDI ------------------------------------- ####

## Intervalos HDI beta
par(mfrow=c(2,3))
for(i in 1:5){
  #Inicio
  HDI.interval.beta <- hdi(betas.pos[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(betas.pos[,i])
  # "Gender", "IMC", "Gestational age", "Lactation time","Complementary feeding time","Growth and development", "Immunization schedule","Type of social security"
  var <- c("Intercept", "Age", "Current weight", "Current height","Gestational age")
  plot(DENSITITY.BETA, main = var[i], xlab = parse(text=(paste0("beta[",i,"]"))))
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "slateblue1")
  #Fin
}

#Intervalos HDI esquema
par(mfrow=c(2,2))
for(i in 1:3){
  #Inicio
  HDI.interval.beta <- hdi(esquema.pos[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(esquema.pos[,i])
  var <- c("Desconocido","No", "Sí")
  plot(DENSITITY.BETA, main = "Immunization schedule",xlab = parse(text=(paste0(var[i]))))
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "slateblue1")
  #Fin
}

#Intervalos HDI crec_desarrollo
par(mfrow=c(1,2))
for(i in 1:2){
  #Inicio
  HDI.interval.beta <- hdi(desarrollo.pos[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(desarrollo.pos[,i])
  var <- c("No", "Sí")
  plot(DENSITITY.BETA, main = "Growth and development", xlab =parse(text=(paste0(var[i]))) )
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "slateblue1")
  #Fin
}

#Intervalos HDI tipo ss
par(mfrow=c(2,3))
for(i in 1:5){
  #Inicio
  HDI.interval.beta <- hdi(tiposs.pos[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(tiposs.pos[,i])
  var <- c("Contributory", "Special", "Uninsured", "Pending", "Subsidized")
  plot(DENSITITY.BETA, main = "Type of social security", xlab =parse(text=(paste0(var[i]))))
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "slateblue1")
}

# Año:
par(mfrow=c(2,3))
for(i in 1:6){
  #Inicio
  HDI.interval.beta <- hdi(periodo.pos[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(periodo.pos[,i])
  var <- c("2016", "2017", "2018", "2019", "2020", "2021")
  plot(DENSITITY.BETA, main = "Año",xlab = parse(text=(paste0(var[i]))))
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "slateblue1")
  #Fin
}

# ---------------------------------------------- Intervalos HDI - comunas ---------------------------------------------------------

nomcomunas <- c("Altavista", "Aranjuez",                     
                "Belen","Buenos Aires",                  
                "Castilla","Corregimiento de San Cristobal",
                "Corregimiento De Santa Elena","Doce de Octubre",               
                "El Poblado","Guayabal",                      
                "La America","La Candelaria",                 
                "Laureles","Manrique",                      
                "Popular","Robledo",                       
                "San Antonio de Prado","San Javier",                   
                "San Sebastian de Palmitas", "Santa Cruz",                    
                "Villa Hermosa","Sin informacion")
par(mfrow= c(2,3))
for(i in 1:6){
  #Inicio
  HDI.interval.beta <- hdi(comunas.pos[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(comunas.pos[,i])
  var <- nomcomunas
  plot(DENSITITY.BETA, main = "Commune", xlab =var[[i]]) #organizar
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "slateblue1")
}

par(mfrow= c(2,3))
for(i in 7:12){
  #Inicio
  HDI.interval.beta <- hdi(comunas.pos[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(comunas.pos[,i])
  var <- nomcomunas
  plot(DENSITITY.BETA, main = "Commune", xlab =var[[i]]) #organizar
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "slateblue1")
}

par(mfrow= c(2,3))
for(i in 13:18){
  #Inicio
  HDI.interval.beta <- hdi(comunas.pos[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(comunas.pos[,i])
  var <- nomcomunas
  plot(DENSITITY.BETA, main = "Commune", xlab =var[[i]]) #organizar
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "slateblue1")
}

par(mfrow= c(2,2))
for(i in 19:22){
  #Inicio
  HDI.interval.beta <- hdi(comunas.pos[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(comunas.pos[,i])
  var <- nomcomunas
  plot(DENSITITY.BETA, main = "Commune", xlab =var[[i]]) #organizar
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "slateblue1")
}


par(mfrow= c(2,2))
for(i in 19:22){
  #Inicio
  HDI.interval.beta <- hdi(comunas.pos[,i])
  value1 <- HDI.interval.beta[1]
  value2 <- HDI.interval.beta[2]
  DENSITITY.BETA <- density(comunas.pos[,i])
  var <- nomcomunas
  plot(DENSITITY.BETA, main = "Commune", xlab =var[[i]]) #organizar
  DENSITITY.BETAy <- DENSITITY.BETA$y
  DENSITITY.BETAx <- DENSITITY.BETA$x
  # Lower and higher indices on the X-axis
  l <- min(which(DENSITITY.BETAx >= value1))
  h <- max(which(DENSITITY.BETAx < value2))
  
  polygon(c(DENSITITY.BETAx[c(l, l:h, h)]),
          c(0, DENSITITY.BETAy[l:h], 0),
          col = "slateblue1")
}

# ---------------------------------------------- Intervalos de credibilidad ----------------------------

######## Intervalos credibilidad al 95%
round(quantile(betas.pos$Age, probs = c(0.025, 0.975)),3)#edad
round(quantile(betas.pos$Weight, probs = c(0.025, 0.975)),3)#peso
round(quantile(betas.pos$`length/height`, probs = c(0.025, 0.975)),3)#talla
round(quantile(betas.pos$`Gestational age`, probs = c(0.025, 0.975)),3)#edad gest

######## Intervalos de credibilidad al 90%
round(quantile(betas.pos$Age, probs = c(0.05,0.5,0.95)),3)#edad
round(quantile(betas.pos$Weight, probs = c(0.05,0.5, 0.95)),3)#peso
round(quantile(betas.pos$`length/height`, probs = c(0.05,0.5, 0.95)),3)#talla
round(quantile(betas.pos$`Gestational age`, probs = c(0.05,0.5, 0.95)),3)#edad gest

######## Intervalos de credibilidad de los odds al 90%
round(exp(quantile(betas.pos$Age, probs = c(0.05,0.5,0.95))),3) #edad
round(exp(quantile(betas.pos$Weight, probs = c(0.05,0.5,0.95))),3) #peso
round(exp(quantile(betas.pos$`length/height`, probs = c(0.05,0.5,0.95))),3) #talla
round(exp(quantile(betas.pos$`Gestational age`, probs = c(0.05,0.5,0.95))),3) #edad gest

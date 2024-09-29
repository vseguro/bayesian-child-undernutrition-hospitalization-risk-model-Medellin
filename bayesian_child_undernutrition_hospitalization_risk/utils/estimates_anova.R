# This script performs a Bayesian analysis of variance (Bayesian ANOVA) for 
# qualitative predictors and calculates point estimates and 90% credible intervals 
# for the fixed effects of these predictors. It generates a plot describing the 
# Bayesian ANOVA and a plot for each qualitative predictor, comparing the estimates 
# for the effects of its respective levels. Furthermore, this script includes the 
# necessary code to calculate point Point estimate and 90% credibility intervals for the
# odds ratios of quantitative predictor variables

## Loading libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(HDInterval)
source("bayesian_child_undernutrition_hospitalization_risk/utils/load_data.R")

## Bayesian ANOVA

# Load sample from the posterior distribution of the finite variance of each qualitative predictor. 
commune_sim <- LoadData("S_commune.txt", data_type = "processed/Definitive_model")
development_sim <- LoadData("S_development.txt", data_type = "processed/Definitive_model")
security_sim <- LoadData("S_security.txt", data_type = "processed/Definitive_model")
scheme_sim <- LoadData("S_scheme.txt", data_type = "processed/Definitive_model")
gender_sim <- LoadData("S_gender.txt", data_type = "processed/Definitive_model")
period_sim <- LoadData("S_period.txt", data_type = "processed/Definitive_model")

# Remove sample values that are significantly below or above the 5% and 95% quantiles,
# respectively, based on the interquartile range (IQR)
commune_sim <- remove_outliers(commune_sim$V1)
development_sim <- remove_outliers(development_sim$V1)
security_sim <- remove_outliers(security_sim$V1)
scheme_sim <- remove_outliers(scheme_sim$V1)
gender_sim <- remove_outliers(gender_sim$V1)
period_sim <- remove_outliers(period_sim$V1)

# Save results from the previous step as data frames
commune_sim <- as.data.frame(commune_sim)
colnames(commune_sim) <- "V1"

development_sim <- as.data.frame(development_sim)
colnames(development_sim) <- "V1"

security_sim <- as.data.frame(security_sim)
colnames(security_sim) <- "V1"

scheme_sim <- as.data.frame(scheme_sim)
colnames(scheme_sim) <- "V1"

gender_sim <- as.data.frame(gender_sim)
colnames(gender_sim) <- "V1"

period_sim <- as.data.frame(period_sim)
colnames(period_sim) <- "V1"

# Merge data frame with final samples
S_alphas <- rbind(commune_sim,development_sim,security_sim,scheme_sim,gender_sim,period_sim) 
# Create data frame with the names of the qualitative predictors
groups <- data.frame(rep(c("Commune(22)","Growth and \n development program(2)","Type of Social \n security(5)","Vaccination \n schedule(3)","Gender(2)", "Year(8)"), each = 150000)) # se identifica al grupo que pertencen (comuna, esquema, ...)
# Combine S_alphas and groups by columns, obtaining a new data frame
S_alphas_grup <- cbind(S_alphas,groups) 
# Assign column names to the new data frame "S_alphas_grup"
colnames(S_alphas_grup) <- c("S_alpha","Grupo") 

# Plot Bayesian ANOVA
Anova <- ggplot(S_alphas_grup, aes(x=Grupo, y=S_alpha)) + 
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.03,position = position_dodge(width=0.8))+
  stat_summary(fun=median, geom="point", shape=21, size=3, col = "black",bg="cadetblue2")+
  theme(aspect.ratio = .6)+
  labs(y = expression(S[alpha]), x = "", title = "Bayesian ANOVA")+
  coord_flip()


## Point estimates and 90% credible intervals for the fixed effects of qualitative predictors

# Load and exponentiate the samples obtained from the posterior distributions of the effects
beta_poste <-   exp(LoadData(file_name = "posterior_betas.txt", data_type = "processed/effects_samples"))
commune_poste <- exp(LoadData(file_name = "posterior_communes.txt", data_type = "processed/effects_samples"))
sheme_poste <- exp(LoadData(file_name = "posterior_schemes.txt", data_type = "processed/effects_samples"))
development_poste <- exp(LoadData(file_name = "posterior_growth.txt", data_type = "processed/effects_samples"))
security_poste <- exp(LoadData(file_name = "posterior_security.txt", data_type = "processed/effects_samples"))
gender_poste <- exp(LoadData(file_name = "posterior_gender.txt", data_type = "processed/effects_samples"))
period_poste <- exp(LoadData(file_name = "posterior_period.txt", data_type = "processed/effects_samples"))

# Name columns with the levels of the respective predictor
colnames(beta_poste) <- c("Intercept", "Age", "Weight", "length/height", "Gestational age")
colnames(sheme_poste) <- c("Unknown", "No", "Yes")
colnames(development_poste) <- c("Yes","No")
colnames(security_poste) <- c("Contributory", "Special", "Uninsured", "Pending", "Subsidized")
colnames(commune_poste) <- c("Altavista", "Aranjuez",                     
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
colnames(gender_poste) <- c("F", "M")
colnames(period_poste) <- c("2016", "2017","2018","2019","2020","2021","2022","2023")

# Associate each value (sample) with its corresponding predictor level
sheme_poste2 <- gather(sheme_poste,"Unknown", "No", "Yes", key = "Tipo",value = "Beta")
development_poste2 <- gather(development_poste, "No", "Yes", key = "Tipo", value = "Beta")
security_poste2 <- gather(security_poste,"Contributory", "Special", "Uninsured", "Pending", "Subsidized", key = "Tipo", value = "Beta")
commune_poste2 <- gather(commune_poste,"Altavista", "Aranjuez",                     
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
gender_poste2 <- gather(gender_poste,"F", "M", key = "Sexo", value = "Beta")
period_poste2 <- gather(period_poste,"2016", "2017","2018","2019","2020","2021","2022","2023", key = "Periodo", value = "Beta")

# Plot point estimates and 90% credible intervals for the fixed effects of qualitative predictors
commune <- ggplot(commune_poste2, aes(x=Comuna, y=Beta)) + 
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.1,position = position_dodge(width=0.5))+
  stat_summary(fun=median, geom="point", shape=21, size=4, col = "black",bg="cadetblue2")+
  geom_hline(aes(yintercept=1), linetype="dashed", color = "blue",size = 0.5)+
  labs(title = "Communes", y= expression(gamma), x = "")+
  theme(aspect.ratio = .8)+
  coord_flip()

sheme <- ggplot(sheme_poste2, aes(x=Tipo, y=Beta)) + 
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.03,position = position_dodge(width=0.5))+
  stat_summary(fun=median, geom="point", shape=21, size=5, col = "black",bg="cadetblue2")+
  geom_hline(aes(yintercept=1), linetype="dashed", color = "blue")+
  labs(title = "Vaccination schedule", y= expression(gamma), x = "")+
  theme(aspect.ratio = .5)+
  coord_flip()

development <- ggplot(development_poste2, aes(x=Tipo, y=Beta)) + 
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.03,position = position_dodge(width=0.5))+
  stat_summary(fun=median, geom="point", shape=21, size=5, col = "black",bg="cadetblue2")+
  geom_hline(aes(yintercept=1), linetype="dashed", color = "blue")+
  labs(title = "Growth and development program", y= expression(gamma), x = "")+
  theme(aspect.ratio = .5)+
  coord_flip()

security <- ggplot(security_poste2, aes(x=Tipo, y=Beta)) + 
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.04,position = position_dodge(width=0.5))+
  stat_summary(fun=median, geom="point", shape=21, size=5, col = "black",bg="cadetblue2")+
  geom_hline(aes(yintercept=1), linetype="dashed", color = "blue")+
  labs(title = "Type of social security", y= expression(gamma), x = "")+
  theme(aspect.ratio = .5)+
  coord_flip()

gender <- ggplot(gender_poste2, aes(x=Sexo, y=Beta)) + 
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.04,position = position_dodge(width=0.5))+
  stat_summary(fun=median, geom="point", shape=21, size=5, col = "black",bg="cadetblue2")+
  geom_hline(aes(yintercept=1), linetype="dashed", color = "blue")+
  labs(title = "Gender", y= expression(gamma), x = "")+
  theme(aspect.ratio = .5)+
  coord_flip()

period <- ggplot(period_poste2, aes(x=Periodo, y=Beta)) + 
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.04,position = position_dodge(width=0.5))+
  stat_summary(fun=median, geom="point", shape=21, size=5, col = "black",bg="cadetblue2")+
  geom_hline(aes(yintercept=1), linetype="dashed", color = "blue")+
  labs(title = "Year", y= expression(gamma), x = "")+
  theme(aspect.ratio = .5)+
  coord_flip()

## Point estimate and 90% credibility intervals for the  odds ratios of quantitative 
## predictor variables

# age
round(exp(quantile(beta_poste$Age, probs = c(0.05,0.5,0.95))),3)

# weight
round(exp(quantile(beta_poste$Weight, probs = c(0.05,0.5,0.95))),3) 

# length/height
round(exp(quantile(beta_poste$`length/height`, probs = c(0.05,0.5,0.95))),3)

# Gestational age
round(exp(quantile(beta_poste$`Gestational age`, probs = c(0.05,0.5,0.95))),3) 

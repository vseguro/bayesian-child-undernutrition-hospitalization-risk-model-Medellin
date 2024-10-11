# This script performs a Bayesian analysis of variance (Bayesian ANOVA) for the 
# qualitative predictors of the model with CIAF and calculates point estimates 
# and 90% credible intervals for the fixed effects of these predictors. It generates
# a plot describing the Bayesian ANOVA and a plot for each qualitative predictor, 
# comparing the estimates for the effects of its respective levels. Furthermore, 
# this script includes the necessary code to calculate point Point estimate and 
# 90% credibility intervals for the odds ratios of quantitative predictor variables

## Loading libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
source("bayesian_child_undernutrition_hospitalization_risk/utils/load_data.R")
source("bayesian_child_undernutrition_hospitalization_risk/utils/utilities.R")

## Bayesian ANOVA

# Load sample from the posterior distribution of the finite variance of each qualitative predictor. 
commune_sim <- LoadData("S_commune.txt", data_type = "processed/variance_samples_CIAF")
development_sim <- LoadData("S_development.txt", data_type = "processed/variance_samples_CIAF")
security_sim <- LoadData("S_security.txt", data_type = "processed/variance_samples_CIAF")
scheme_sim <- LoadData("S_scheme.txt", data_type = "processed/variance_samples_CIAF")
gender_sim <- LoadData("S_gender.txt", data_type = "processed/variance_samples_CIAF")
period_sim <- LoadData("S_period.txt", data_type = "processed/variance_samples_CIAF")
index_sim <- LoadData("S_index.txt", data_type = "processed/variance_samples_CIAF")

# Remove sample values that are significantly below or above the 5% and 95% quantiles,
# respectively, based on the interquartile range (IQR) and save results as data frames
sample_data <- lapply(c(commune_sim,development_sim,security_sim,scheme_sim,gender_sim,period_sim,index_sim), 
                      function(f) {
  data <- remove_outliers(f)
  data <- as.data.frame(data)
  colnames(data) <- "V1"
  data
})

# Merge data frame with final samples
S_alphas <- do.call(rbind, sample_data)
# Create data frame with the names of the qualitative predictors
groups <- data.frame(rep(c("Commune(22)","Growth and \n development program(2)","Type of Social \n security(5)","Vaccination \n schedule(3)", "Gender(2)", "Year(8)","CIAF(7)"), each = 150000))
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

# Save plot
ggsave(filename = "reports/figures/model_with_CIAF/plot_ANOVA.pdf",
       plot = Anova, width = 8, height = 6)

## Point estimates and 90% credible intervals for the fixed effects of qualitative predictors

# Load the samples obtained from the posterior distributions of the effects
beta_poste <-   LoadData(file_name = "posterior_betas.txt", data_type = "processed/fixed_effects_samples_CIAF")
commune_poste <- LoadData(file_name = "posterior_communes.txt", data_type = "processed/fixed_effects_samples_CIAF")
scheme_poste <- LoadData(file_name = "posterior_schemes.txt", data_type = "processed/fixed_effects_samples_CIAF")
development_poste <- LoadData(file_name = "posterior_growth.txt", data_type = "processed/fixed_effects_samples_CIAF")
security_poste <- LoadData(file_name = "posterior_security.txt", data_type = "processed/fixed_effects_samples_CIAF")
gender_poste <- LoadData(file_name = "posterior_gender.txt", data_type = "processed/fixed_effects_samples_CIAF")
period_poste <- LoadData(file_name = "posterior_period.txt", data_type = "processed/fixed_effects_samples_CIAF")
index_poste <- LoadData(file_name = "posterior_index.txt", data_type = "processed/fixed_effects_samples_CIAF")

# Name columns with the levels of the respective predictor
colnames(beta_poste) <- c("Intercept", "Age", "Weight", "length/height", "Gestational age")
colnames(scheme_poste) <- c("Unknown", "No", "Yes")
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
colnames(index_poste) <- c("No anthropometric failure", "Wasting only", "Wasting and underweight", "Wasting, underweight and
stunting", "Stunting and underweight", "Stunting only", "Underweight only")

# Associate each value (sample) with its corresponding predictor level
scheme_poste2 <- gather(scheme_poste,"Unknown", "No", "Yes", key = "type",value = "Beta")
development_poste2 <- gather(development_poste, "No", "Yes", key = "type", value = "Beta")
security_poste2 <- gather(security_poste,"Contributory", "Special", "Uninsured", "Pending", "Subsidized", key = "type", value = "Beta")
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
                       "Villa Hermosa","No information", key = "commune", value = "Beta")
gender_poste2 <- gather(gender_poste,"F", "M", key = "gender", value = "Beta")
period_poste2 <- gather(period_poste,"2016", "2017","2018","2019","2020","2021","2022","2023", key = "year", value = "Beta")
index_poste2 <- gather(index_poste,"No anthropometric failure", "Wasting only", "Wasting and underweight", "Wasting, underweight and
stunting", "Stunting and underweight", "Stunting only", "Underweight only", key = "index", value = "Beta")

# Plot point estimates and 90% credible intervals for the fixed effects of qualitative predictors
generate_plot(data = commune_poste2,title = "Communes",name_file = "commune",path = "reports/figures/model_with_CIAF")
generate_plot(data = scheme_poste2,title = "Vaccination schedule",name_file = "scheme",path = "reports/figures/model_with_CIAF")
generate_plot(data = development_poste2,title = "Growth and development program",name_file = "development",path = "reports/figures/model_with_CIAF")
generate_plot(data = security_poste2,title = "Type of social security",name_file = "security",path = "reports/figures/model_with_CIAF")
generate_plot(data = gender_poste2,title = "Gender",name_file = "gender",path = "reports/figures/model_with_CIAF")
generate_plot(data = period_poste2,title = "Year",name_file = "year",path = "reports/figures/model_with_CIAF")
generate_plot(data = index_poste2,title = "CIAF",name_file = "CIAF",path = "reports/figures/model_with_CIAF")

## Point estimate and 90% credibility intervals for the  odds ratios of quantitative 
## predictor variables

# age variable
round(exp(quantile(beta_poste$Age, probs = c(0.05,0.5,0.95))),3)

# weight variable
round(exp(quantile(beta_poste$Weight, probs = c(0.05,0.5,0.95))),3) 

# length/height variable
round(exp(quantile(beta_poste$`length/height`, probs = c(0.05,0.5,0.95))),3)

# gestational age variable
round(exp(quantile(beta_poste$`Gestational age`, probs = c(0.05,0.5,0.95))),3) 

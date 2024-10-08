# This script calculates the ROC (Receiver Operating Characteristic) curve and 
# the AUC (Area Under the Curve) of the model with CIAF using data from each child 
# (from the final dataset) and samples obtained (simulations) from the posterior 
# distributions of the effects of predictors. 


## Loading libraries
library(ROCR)
source("bayesian_child_undernutrition_hospitalization_risk/utils/load_data.R")
source("bayesian_child_undernutrition_hospitalization_risk/features/build_features.R")

## Loading and preparation of the dataset used in model fitting (final dataset)
data <- BuildFeatures()

## create object for response variable
y <- data$pac_hos_ 

## Center quantitative predictors
x1 <-(data$edad_ - mean(data$edad_))
x3 <- (data$peso_act - mean(data$peso_act))
x4 <- (data$talla_act - mean(data$talla_act))
x5 <- (data$edad_ges - mean(data$edad_ges))

## Create the design matrix
design_matrix <- model.matrix(~ x1 + x3 + x4 + x5) 

## Load the samples obtained from the posterior distributions 
beta_poste <-   LoadData(file_name = "posterior_betas.txt", data_type = "processed/fixed_effects_samples_CIAF")
commune_poste <- LoadData(file_name = "posterior_communes.txt", data_type = "processed/fixed_effects_samples_CIAF")
sheme_poste <- LoadData(file_name = "posterior_schemes.txt", data_type = "processed/fixed_effects_samples_CIAF")
development_poste <- LoadData(file_name = "posterior_growth.txt", data_type = "processed/fixed_effects_samples_CIAF")
security_poste <- LoadData(file_name = "posterior_security.txt", data_type = "processed/fixed_effects_samples_CIAF")
gender_poste <- LoadData(file_name = "posterior_gender.txt", data_type = "processed/fixed_effects_samples_CIAF")
period_poste <- LoadData(file_name = "posterior_period.txt", data_type = "processed/fixed_effects_samples_CIAF")
index_poste <- LoadData(file_name = "posterior_index.txt", data_type = "processed/fixed_effects_samples_CIAF")

## Obtain a subsample of size 20.000 for each effect
beta_poste <- sapply(beta_poste, sample, size = 20000,replace = TRUE)
commune_poste <- sapply(commune_poste, sample, size = 20000,replace = TRUE) 
sheme_poste <- sapply(sheme_poste, sample, size = 20000,replace = TRUE)
development_poste <- sapply(development_poste, sample, size = 20000,replace = TRUE)
security_poste <- sapply(security_poste, sample, size = 20000,replace = TRUE)
gender_poste <- sapply(gender_poste, sample, size = 20000,replace = TRUE)
period_poste <- sapply(period_poste, sample, size = 20000,replace = TRUE)
index_poste <- sapply(index_poste, sample, size = 20000,replace = TRUE)

## Calculate predicted probabilities
Py2 = sapply(1:dim(design_matrix)[1], 
             function(j){median(sapply(1:dim(beta_poste)[1], 
                                       function(i){exp(design_matrix[j,]%*%as.numeric(beta_poste[i,]) +
                                                         as.numeric(commune_poste[i,data$comuna[j]]) + 
                                                         as.numeric(sheme_poste[i,data$esq_vac[j]]) + 
                                                         as.numeric(development_poste[i,data$crec_dllo[j]]) + 
                                                         as.numeric(security_poste[i, data$tipo_ss_[j]])+
                                                         as.numeric(gender_poste[i, data$sexo_[j]]) +
                                                         as.numeric(period_poste[i, data$year_[j]]) +
                                                         as.numeric(index_poste[i, data$CIAF[j]])
                                                         )/
                                           (1 + exp(design_matrix[j,]%*%as.numeric(beta_poste[i,]) + 
                                                      as.numeric(commune_poste[i,data$comuna[j]]) +  
                                                      as.numeric(sheme_poste[i,data$esq_vac[j]]) + 
                                                      as.numeric(development_poste[i,data$crec_dllo[j]]) + 
                                                      as.numeric(security_poste[i,data$tipo_ss_[j]])+
                                                      as.numeric(gender_poste[i, data$sexo_[j]]) +
                                                      as.numeric(period_poste[i, data$year_[j]]) +
                                                      as.numeric(index_poste[i, data$CIAF[j]])
                                                      ))}))})

## Compute the ROC curve
ROCR.simple2c = list(prediccionesc = Py2, labels = y)
dfc <- data.frame(ROCR.simple2c)
predc <- prediction(dfc$prediccionesc, dfc$labels)
perfc<- performance(predc,"tpr","fpr")

## Plot the ROC curve
pdf("reports/figures/model_with_CIAF/plot_ROC_curve.pdf",
    width = 8, height = 6) 
plot(perfc,colorize=TRUE,type="l", main = "Curva ROC")
abline(a=0,b=1,col="red")
dev.off()


## Calculate the AUC (Area Under the Curve)
perf_auc <- performance(predc,"auc")
# Display the value in the console
perf_auc@y.values # AUC
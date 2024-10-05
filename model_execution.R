# The following code implements the proposed model, allowing to obtain the estimates of the 
# parameters taken into account: fixed effects and variances associated to each qualitative variable.

library(rstan)
library(HDInterval)
library(dplyr)
library(ROCR)
source("bayesian_child_undernutrition_hospitalization_risk/utils/load_data.R")
source("bayesian_child_undernutrition_hospitalization_risk/utils/utilities.R")
source("bayesian_child_undernutrition_hospitalization_risk/features/build_features.R")

# The data is called
final_data <- BuildFeatures()

# The variables of the model are defined:
y <- final_data$pac_hos_ 
x1 <-(final_data$edad_ - mean(final_data$edad_))
x3 <- (final_data$peso_act - mean(final_data$peso_act))
x4 <- (final_data$talla_act - mean(final_data$talla_act))
x5 <- (final_data$edad_ges - mean(final_data$edad_ges))

# The design matrix is defined:
X_models12 <- model.matrix(~ x1 + x3 + x4 + x5) 
head (X_models12)

# A list of data is created for the model in Stan, including the predictor matrices (X),
# the dependent variable (y), the number of observations (N), parameters (P), and the other 
# categorical factors:
stan_data_models12 <- list(
  
  "X" = X_models12,
  
  "y" = y,
  
  "N" = nrow(final_data), # Number of observations
  
  "P" = ncol(X_models12),  # Number of fixed parameters
  
  "C" = 22, # Number of communes
  
  "E" = 3, # Vaccination schedule levels, (yes, no, unknown) 
  
  "D" = 2, # Levels of growth and development, (yes, no) 
  
  "S" = 5, # Levels of social security, (subsidised, contributory, pending, special, uninsured)
  
  "G" = 2, # Gender levels
  
  "Y" = 8, # Year levels
  
  # Data vectors are defined for the categorical variables: commune, scheme, development, security, 
  # gender and period:
  
  "Commune" = final_data$comuna,
  
  "Scheme" = final_data$esq_vac,
  
  "Development" = final_data$crec_dllo,
  
  "Security" = final_data$tipo_ss_, 
  
  "Gender" = final_data$sexo_,
  
  "Period" = final_data$year_
  
)

# Fitting of the model with the configuration of its execution.
fit_models12 <- stan(
  file = "model/stan_model.stan", 
  data = stan_data_models12, 
  chains = 3, 
  iter =  200000, 
  warmup = 80000, 
  cores = 3, 
  thin = 4
)

# Summary of betas (for continuous variables)
print(fit_models12, pars = "beta")

# Summary of fixed effects for communes
print(fit_models12, pars = "commune_cen")

# Summary of fixed effects for Vaccination schedule
print(fit_models12, pars = "scheme_cen")

# Summary of fixed effects for Growth and development program
print(fit_models12, pars = "development_cen")

# Summary of fixed effects for Social security
print(fit_models12, pars = "security_cen")

# Summary of fixed effects for Gender
print(fit_models12, pars = "gender_cen")

# Summary of fixed effects for period
print(fit_models12, pars = "period_cen")


# The values for parameters and fixed effects are extracted in txt:
beta_poste <- extract(fit_models12, pars = "beta")
commune_poste <- extract(fit_models12, pars = "commune_cen")
scheme_poste <- extract(fit_models12, pars = "scheme_cen")
development_poste <- extract(fit_models12, pars = "development_cen")
security_poste <- extract(fit_models12, pars = "security_cen")
gender_poste <- extract(fit_models12, pars = "gender_cen")
period_poste <- extract(fit_models12, pars = "period_cen")

# The S_alpha is extracted and saved in txt.
commune_sim <- c(fit_models12@sim[[1]][[1]]$S_commune,fit_models12@sim[[1]][[2]]$S_commune ,fit_models12@sim[[1]][[3]]$S_commune)
scheme_sim <- c(fit_models12@sim[[1]][[1]]$S_scheme,fit_models12@sim[[1]][[2]]$S_scheme,fit_models12@sim[[1]][[3]]$S_scheme)
development_sim <- c(fit_models12@sim[[1]][[1]]$S_development,fit_models12@sim[[1]][[2]]$S_development,fit_models12@sim[[1]][[3]]$S_development)
security_sim <- c(fit_models12@sim[[1]][[1]]$S_security,fit_models12@sim[[1]][[2]]$S_security,fit_models12@sim[[1]][[3]]$S_security)
gender_sim <- c(fit_models12@sim[[1]][[1]]$S_gender,fit_models12@sim[[1]][[2]]$S_gender,fit_models12@sim[[1]][[3]]$S_gender)
period_sim <- c(fit_models12@sim[[1]][[1]]$S_period,fit_models12@sim[[1]][[2]]$S_period,fit_models12@sim[[1]][[3]]$S_period)

# Save fixed effects samples (betas):
write_posterior_data(beta_poste,'fixed_effects_samples', 'posterior_betas.txt', 5)
write_posterior_data(commune_poste, 'fixed_effects_samples', 'posterior_communes.txt', 22)
write_posterior_data(scheme_poste, 'fixed_effects_samples', 'posterior_schemes.txt', 3)
write_posterior_data(development_poste, 'fixed_effects_samples', 'posterior_growth.txt', 2)
write_posterior_data(security_poste, 'fixed_effects_samples', 'posterior_security.txt', 5)
write_posterior_data(gender_poste, 'fixed_effects_samples','posterior_gender.txt', 2)
write_posterior_data(period_poste, 'fixed_effects_samples', 'posterior_period.txt', 8)

# Save variance samples (S_alpha):
write_posterior_data(commune_sim, 'variance_samples', 'S_commune.txt', 1)
write_posterior_data(scheme_sim, 'variance_samples', 'S_scheme.txt', 1)
write_posterior_data(development_sim, 'variance_samples', 'S_development.txt', 1)
write_posterior_data(security_sim, 'variance_samples', 'S_security.txt', 1)
write_posterior_data(gender_sim, 'variance_samples', 'S_gender.txt', 1)
write_posterior_data(period_sim, 'variance_samples', 'S_period.txt', 1)

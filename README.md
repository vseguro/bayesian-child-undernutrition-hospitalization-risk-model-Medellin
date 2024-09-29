# Bayesian child undernutrition hospitalization risk model

This project is based on research: Bayesian analysis of risk hospitalization assessment for children under five years of age with acute undernutrition in Medellin, Colombia.

The objective of which is to build a Bayesian model that quantifies the risk of hospitalization for acute malnutrition in children under 5 years of age.

## Installation
For the execution of the model it is important to configure the tool chain for use in the Stan programming language in R:

 - https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
 - https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#configuring-c-toolchain
 
### The resulting directory structure


```         
├── README.md          <- The top-level README for developers using this project.
├── data
│   ├── raw             <- The original, immutable data dump.
│   ├── interim         <- Intermediate data that has been transformed.
│   └── processed       <- The final, canonical data sets for modeling.
│       ├── fixed_effects_samples  <- The final, samples of the posteriori distributions of the predictor variables. 
│       └── variance_samples       <- The final, samples from the posterior distributions of the finite variance of each qualitative predictor.
│
├── model              <- Stan language code for model implementation. 
│
├── reports            <- Generated analysis as HTML, PDF, LaTeX, etc.
│   └── figures        <- Generated graphics and figures to be used in reporting
│
├── model_execution.R   <- Code to implement the model.
│
└── bayesian_child_undernutrition_hospitalization_risk  <- Source code for use in this project.
    │
    ├── data                   <- Store useful variables and configuration
    │   └──make_dataset.R      <- Scripts to download or generate data
    ├── features 
    │    └── build_features.R  <- Code to create features for modeling
    └── utils                
       ├── load_data.R        <- Functions to read and write data.
       ├── utilities.R        <- Auxiliary functions for the project.        
       ├── estimates_anova    <- Code to build and calculates point estimates and 90% credible intervals plots.
       └── roc_curve.R        <- Code to roc curve plot.
  
```

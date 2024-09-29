# The following script is in charge of making the final adjustments to obtain the final data.
# Read the base obtained in the WHO application with the data created with the MakeDataset() function
# and thus obtain the WHZ, HAZ, WAZ scores. Then eliminate outliers according to these indices and finally
# adapt the levels of categorical variables.

library(dplyr)
source("bayesian_child_undernutrition_hospitalization_risk/utils/load_data.R")
source("bayesian_child_undernutrition_hospitalization_risk/utils/utilities.R")

BuildFeatures <- function(){
  ## Read the data obtained from the WHO application to obtain the zscores, this file must be previously saved in the data/raw folder.
  full_data_zscore <- LoadData("full_data_zscore.csv", data_type = "raw", sep = ",")
  
  ## Select relevant columns for further analysis
  full_data_zscore <- full_data_zscore[,c("edad_", "sexo_", "comuna", "tipo_ss_", "pac_hos_", "edad_ges", 
                                          "crec_dllo", "esq_vac", "peso_act", "talla_act", "year_", "zlen", "zwei", "zwfl")]
  
  ## Rename columns for better clarity (HAZ = height-for-age, WAZ = weight-for-age, WHZ = weight-for-height)
  full_data_zscore <- rename(full_data_zscore, HAZ = zlen, WAZ = zwei, WHZ = zwfl)
  
  ## Check for rows with missing values in the WHZ variable
  full_data_zscore_Na <- full_data_zscore %>% filter(is.na(WHZ)) # Dataset with missing WHZ values
  
  ## Remove rows with missing values in WHZ to clean the data
  full_data_zscore <- full_data_zscore %>% filter(!is.na(WHZ))
  
  ## Handle missing observations (add median and variation manually for some observations)
  full_data_zscore_Na$median <- c(5.9907, 6.3738, 3.1560, 2.4410, 5.5632, 6.3738, 2.948)
  full_data_zscore_Na$variation <- c(0.08342, 0.09135, 0.0906, 0.09182, 0.08406, 0.09135, 0.09007)
  
  ## Calculate missing WHZ values using a custom function
  full_data_zscore_Na$WHZ <- mapply(WHZIndexCalculation, split(full_data_zscore_Na, seq(nrow(full_data_zscore_Na))))
  
  ## Remove the median and variation columns (as they were only used for calculating WHZ)
  full_data_zscore_Na <- full_data_zscore_Na[,-c(15,16)]
  
  ## Combine the cleaned data with the filled missing data
  full_data_zscore <- rbind(full_data_zscore, full_data_zscore_Na)
  
  ## Remove outliers based on the WHZ, HAZ, and WAZ variables (outliers outside specified range)
  final_data <- subset(full_data_zscore, 
                       !(full_data_zscore$WHZ < -5 | full_data_zscore$WHZ > 5 | 
                           full_data_zscore$HAZ < -6 | full_data_zscore$HAZ > 6 | 
                           full_data_zscore$WAZ < -5 | full_data_zscore$WAZ > 5))
  
  ## Factor encoding for the variable 'pac_hos_' 
  ## (hospitalized status: 1 for yes, 0 for no)
  final_data <- final_data %>%
    mutate(
      pac_hos_ = case_when(
        pac_hos_ == "1" ~ 1,
        pac_hos_ == "2" ~ 0
      ))
  
  ## Factor encoding for the vaccination schedule ('esq_vac') 
  ## (1 for Unknown, 2 for No, 3 for Yes)
  final_data <- final_data %>%
    mutate(
      esq_vac = case_when(
        esq_vac == "Unknown" ~ 1,
        esq_vac == "No" ~ 2, 
        esq_vac == "Yes" ~ 3
      ))
  
  ## Factor encoding for the type of social security ('tipo_ss_')
  ## (Assign numeric values for each type)
  final_data <- final_data %>%
    mutate(
      tipo_ss_ = case_when(
        tipo_ss_ == "Contributive" ~ 1,
        tipo_ss_ == "Special" ~ 2,
        tipo_ss_ == "Uninsured" ~ 3, 
        tipo_ss_ == "Pending" ~ 4, 
        tipo_ss_ == "Subsidized" ~ 5
      ))
  
  ## Factor encoding for the commune ('comuna') by assigning numeric values for each name
  final_data <- final_data %>%
    mutate(
      comuna = case_when(
        comuna == "Altavista" ~ 1,
        comuna == "Aranjuez" ~ 2,
        comuna == "Belen" ~ 3,
        comuna == "Buenos Aires" ~ 4,
        comuna == "Castilla" ~ 5,
        comuna == "Corregimiento de San Cristobal" ~ 6,
        comuna == "Corregimiento De Santa Elena" ~ 7,
        comuna == "Doce de Octubre" ~ 8,
        comuna == "El Poblado" ~ 9,
        comuna == "Guayabal" ~ 10,
        comuna == "La America" ~ 11,
        comuna == "La Candelaria" ~ 12,
        comuna == "Laureles" ~ 13,
        comuna == "Manrique" ~ 14,
        comuna == "Popular" ~ 15,
        comuna == "Robledo" ~ 16,
        comuna == "San Antonio de Prado" ~ 17,
        comuna == "San Javier" ~ 18,
        comuna == "San Sebastian de Palmitas" ~ 19,
        comuna == "Santa Cruz" ~ 20,
        comuna == "Villa Hermosa" ~ 21,
        comuna == "Sin informacion" ~ 22
      ))
  
  ## Factor encoding for gender ('sexo_') 
  ## (1 for Female, 2 for Male)
  final_data <- final_data %>%
    mutate(
      sexo_ = case_when(
        sexo_ == "F" ~ 1,
        sexo_ == "M" ~ 2
      ))
  
  ## Factor encoding for the year ('year_')
  ## (Assign numeric values for each year from 2016 to 2023)
  final_data <- final_data %>%
    mutate(
      year_ = case_when(
        year_ == "2016" ~ 1, 
        year_ == "2017" ~ 2, 
        year_ == "2018" ~ 3,
        year_ == "2019" ~ 4, 
        year_ == "2020" ~ 5, 
        year_ == "2021" ~ 6, 
        year_ == "2022" ~ 7,
        year_ == "2023" ~ 8
      )
    )
  
  ## Remove unnecessary columns (HAZ, WAZ, WHZ) to clean up the dataset
  final_data <- final_data[, -c(12:14)]
  
  ## Save the final processed dataset to a CSV file
  SaveData(final_data, data_type = "processed", file_name = "acute_malnutrition_final_data.csv")
}

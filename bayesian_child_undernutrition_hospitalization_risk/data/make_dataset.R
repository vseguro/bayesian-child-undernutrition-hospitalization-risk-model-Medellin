# The following script is in charge of merging the data since some of them come
# from different sources of information (MEDATA and direct communication with Health Secretary of Medellin,
# then the information is depured and cleaned and the resulting file is the one 
# used in the WHO application to obtain the corresponding  WHZ, HAZ, WAZ scores.

## Loading libraries:
library(dplyr)
source("bayesian_child_undernutrition_hospitalization_risk/utils/load_data.R")
source("bayesian_child_undernutrition_hospitalization_risk/utils/utilities.R")

MakeDataset <- function(){
  # Reading data from multiple sources (CSV and Excel files)
  data_2021 <- LoadData("sivigila_malnutrition_medata_until2021.csv", data_type = "raw", sep = ";")
  data_2022 <- LoadData("sivigila_malnutrition_2022.xlsx", data_type = "raw")
  data_2023 <- LoadData("sivigila_malnutrition_2023.xlsx", data_type = "raw")
  
  ## Rename columns in the 2022 and 2023 datasets to ensure consistency
  colnames(data_2022) <- c(colnames(data_2022)[1:7], "tipo_ss_", colnames(data_2022)[9:23],
                           "per_braqu", "evento", colnames(data_2022)[26])
  colnames(data_2023) <- c(colnames(data_2023)[1:7], "tipo_ss_", colnames(data_2023)[9:23],
                           "per_braqu", colnames(data_2023)[25:26])
  
  ## Select variables of interest (columns) from each dataset
  data_2021 <- data_2021[,c(1,3:5,6,7,8,13,16:20,22:24, 26)]
  data_2022 <- data_2022[,c(1,3:5,6,7,8,13,16:20,22:24, 26)]
  data_2023 <- data_2023[,c(1,3:5,6,7,8,13,16:20,22:24, 26)]
  
  # Combine all the datasets into one by stacking rows
  full_data <- rbind(data_2021, data_2022, data_2023)
  
  # Convert relevant columns to the correct data types (numeric, factor)
  full_data <- within(full_data,{
    edad_ <- as.numeric(edad_)                   # Age in numeric format
    uni_med_ <- as.factor(uni_med_)              # Unit of measurement as a factor
    sexo_ <- as.factor(sexo_)                    # Gender as a factor
    nombre_barrio <- as.factor(nombre_barrio)    # Neighborhood name as a factor
    comuna <- as.factor(comuna)                  # Commune as a factor
    pac_hos_ <- as.factor(pac_hos_)              # Hospitalized patient status as a factor
    edad_ges <- as.numeric(edad_ges)             # Gestational age in numeric format
    t_lechem <- as.numeric(t_lechem)             # Time on breast milk in numeric format
    e_complem <- as.numeric(e_complem)           # Time on complementary feeding in numeric format
    crec_dllo <- as.factor(crec_dllo)            # Growth and development status as a factor
    esq_vac <- as.factor(esq_vac)                # Vaccination schedule as a factor
    tipo_ss_ <- as.factor(tipo_ss_)              # Social security type as a factor
    year_ <- as.factor(year_)                    # Year as a factor
  })
  
  # Replace commas with dots in weight, height, and arm circumference, then convert to numeric
  full_data$peso_act <- sub(",",".", full_data$peso_act) |> as.numeric()
  full_data$talla_act <- sub(",",".", full_data$talla_act) |> as.numeric()
  full_data$per_braqu <- sub(",",".", full_data$per_braqu) |> as.numeric()
  
  ## Age conversion: Apply a custom function to convert age to months
  for(i in 1:dim(full_data)[1]){
    full_data$edad_[i] <- AgeConversion(full_data[i,2],full_data[i,3])
  }
  
  ## Eliminate the unit of measurement column as it's no longer needed
  full_data <- full_data[,-3] 
  
  ## Standardize levels in the "comuna" (commune) column
  levels(full_data$comuna) <- sub("^\\d*\\s*", "", levels(full_data$comuna)) 
  levels(full_data$comuna)[levels(full_data$comuna) %in% c("SIN INFORMACION", "Sin información", "Sin Información")] <- "Sin informacion" 
  levels(full_data$comuna)[levels(full_data$comuna)=="Corregimiento de Altavista"] <- "Altavista" 
  levels(full_data$comuna)[levels(full_data$comuna)=="Corregimiento de Palmitas"] <- "San Sebastian de Palmitas"
  levels(full_data$comuna)[levels(full_data$comuna)=="Corregimiento de Santa Elena"] <- "Corregimiento De Santa Elena" 
  levels(full_data$comuna)[levels(full_data$comuna)=="Corregimiento de San Antonio de Prado"] <- "San Antonio de Prado" 
  levels(full_data$comuna)[levels(full_data$comuna)=="Doce De Octubre"] <- "Doce de Octubre" 
  levels(full_data$comuna)[levels(full_data$comuna)=="Corregimiento de San Cristóbal"] <- "Corregimiento de San Cristobal"
  levels(full_data$comuna)[levels(full_data$comuna)=="Laureles Estadio"] <- "Laureles" 
  levels(full_data$comuna)[levels(full_data$comuna)=="La América"] <- "La America" 
  levels(full_data$comuna)[levels(full_data$comuna)=="Belén"] <- "Belen" 
  
  ## Remove rows where gestational age (edad_ges) is missing or equal to zero
  full_data_sinNa <- full_data[!is.na(full_data$edad_ges), ]
  full_data <- full_data_sinNa[full_data_sinNa$edad_ges != 0, ]
  
  ## Rename the levels in vaccination schedule and social security type
  full_data <- full_data %>%
    mutate(
      esq_vac = case_when(
        esq_vac == "1" ~ "Yes",
        esq_vac == "2" ~ "No",
        esq_vac == "3" ~ "Unknown"
      ),
      tipo_ss_ = case_when(
        tipo_ss_ == "C" ~ "Contributive",
        tipo_ss_ == "E" ~ "Special",
        tipo_ss_ == "I" ~ "Pending",
        tipo_ss_ == "N" ~ "Uninsured",
        tipo_ss_ == "P" ~ "Special", 
        tipo_ss_ == "S" ~ "Subsidized"
      )
    ) 
  
  # Convert these variables back to factors
  full_data$tipo_ss_ <- as.factor(full_data$tipo_ss_) 
  full_data$esq_vac <- as.factor(full_data$esq_vac)
  
  # Add a new column for the country
  full_data$country <- "Colombia" 
  
  ## Export the cleaned and processed dataset as a CSV file
  SaveData(full_data, data_folder = "interim", file_name = "full_data.csv")
}

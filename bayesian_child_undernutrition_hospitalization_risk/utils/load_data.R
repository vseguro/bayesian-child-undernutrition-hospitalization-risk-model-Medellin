# The following script contains functions to perform basic and necessary actions
# for running the code, such as loading and saving data.

## Loading libraries
library(readxl)

## Function to upload necessary files in .xlsx, .txt and .csc formats
LoadData <- function(file_name, data_type = "raw", ...){
  
  path <- dirname(getwd())
  
  if(sub(".*/", "", path) != "bayesian-child-undernutrition-hospitalization-risk-model-Medellin"){
    path <- paste0(dirname(getwd()),"/bayesian-child-undernutrition-hospitalization-risk-model-Medellin")
  }
  
  path <- paste0(path, "/data/", data_type,"/", file_name)
  
  file_type = sub(".*\\.", "", file_name)

  if(file_type == "xlsx"){
    return(read_xlsx(path, col_names = TRUE, ...))
  }
  else if(file_type == "csv"){
    return(read.csv(path, header = TRUE, encoding = "UTF-8", ...))
  }
  else if(file_type == "txt"){
    return(read.table(path, ...))
  }
  else{
    return("Specify the file type... ")
  }
}

## Function to save an object containing a dataset as a .csv file
SaveData <- function(dataframe, data_type = "raw", file_name, ...){
  
  # path <- paste0(dirname(getwd()), "/data/", data_folder,"/", file_name)
  path <- dirname(getwd())
  
  if(sub(".*/", "", path) != "bayesian-child-undernutrition-hospitalization-risk-model-Medellin"){
    path <- paste0(dirname(getwd()),"/bayesian-child-undernutrition-hospitalization-risk-model-Medellin")
  }
  
  path <- paste0(path, "/data/", data_type,"/", file_name)
  
  write.csv(dataframe, path)
  print(paste("The", file_name, "file has been saved correctly!"))
}

## Function to save the model outputs: fixed effects sample and variance samples as txt.
write_posterior_data <- function(data, file_path, ncolumns, as_df = TRUE) {
  
  # Converts the object to dataframe
  if(as_df == TRUE){
    data <- as.data.frame(data)
    # Transposes and writes to the file
    write(x = t(data), file = file_path, ncolumns = ncolumns, append = TRUE)
  }else{
    # Transposes and writes to the file
    write(x = t(data), file = file_path, ncolumns = ncolumns, append = TRUE)
  }
}


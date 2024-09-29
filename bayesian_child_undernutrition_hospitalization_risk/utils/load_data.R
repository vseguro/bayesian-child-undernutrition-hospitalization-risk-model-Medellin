library(readxl)

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


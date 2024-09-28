LoadData <- function(file_name, data_type = "raw", ...){
  
  path <- dirname(getwd())
  
  if(sub(".*/", "", path) != "Proyecto-articulo"){
    path <- paste0(dirname(getwd()),"/Proyecto-articulo")
  }
  
  path <- paste0(path, "/data/", data_type,"/", file_name)
  
  # print(path)
  
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
    return("Especifique el tipo de archivo")
  }
}

SaveData <- function(dataframe, data_folder = "raw", file_name, ...){
  
  path <- paste0(dirname(getwd()), "/data/", data_folder,"/", file_name)
  
  write.csv(dataframe, path)
  print("The data has been saved correctly!")
}

WHZIndexCalculation <- function(datazscoreNA){
  weight = datazscoreNA$peso_act
  height = datazscoreNA$talla_act
  sex = datazscoreNA$sexo_
  median = datazscoreNA$median
  variation = datazscoreNA$variation
  
  if(sex == "M"){
    power = -0.3521
  }else if(sex == "F"){
    power = -0.3833
  }
  
  WHZ <- (((weight/median)^power)-1)/(power*variation)
  return(WHZ)
  
}
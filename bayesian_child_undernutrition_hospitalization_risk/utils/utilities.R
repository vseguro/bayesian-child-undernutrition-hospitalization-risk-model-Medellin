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
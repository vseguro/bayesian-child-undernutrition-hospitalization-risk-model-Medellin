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

AgeConversion <- function(year, unidad){
  if(unidad == "1"){ # Year
    age <- year*12
  }
  else if(unidad == "3"){ # Days
    age <- year/30
  }else if(unidad == "4"){
    age <- year/720
  }else {
    age <- year
  }
  return(age)
}

# Function implemented in the "estimates_anova.R" file to calculate quantiles at
# the 0% (minimum), 5% (5th percentile), 50% (median), 95% (95th percentile), and 
# 100% (maximum) positions of the samples
f <- function(x) {
  r <- quantile(x, probs = c(0, 0.05, 0.5, 0.95, 1)) 
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# Function implemented in the "estimates_anova.R" file to remove outliers from 
# the samples
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
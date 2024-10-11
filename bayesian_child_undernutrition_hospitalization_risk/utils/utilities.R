## This script has helper functions needed to execute main code functions 

# Function implemented in the "build_features.R" to calculate the z-score of the WHZ index
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

# Function implemented in the "make_dataset.R" to convert the variable age to months
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

# Function implemented in the "estimates_anova.R" file to create and save in a pdf
# file the graph of the point estimate and credibility interval of each qualitative 
# predictor
generate_plot <- function(data, title, path, name_file, width = 8, height = 6) {
  # Validate the database dimensions
  if (ncol(data) == 2 && nrow(data) > 0) {
    # Create plot
    plot <- ggplot(data, aes(x = data[,1], y = data[,2])) + 
      stat_summary(fun.data = f, geom = "boxplot", fill = 'steelblue', width = 0.04, 
                   position = position_dodge(width = 0.5)) +
      stat_summary(fun = median, geom = "point", shape = 21, size = 5, 
                   col = "black", bg = "cadetblue2") +
      geom_hline(aes(yintercept = 0), linetype = "dashed", color = "blue", size = 0.5) +
      labs(title = title, y = expression(gamma), x = "") +
      theme(aspect.ratio = .6) + 
      coord_flip()
    
    # Show plot
    print(plot)
    
    # Define the save path
    path <- file.path(path, paste0("plot_", name_file, ".pdf"))
    
    # Save plot as a PDF file
    tryCatch({
      ggsave(filename = path, plot = plot, width = width, height = height)
      print(paste0("The plot was successfully saved in ", path))
    }, error = function(e) {
      print("The plot was not saved correctly: ", e$message)
    })
    
  } else {
    print("The plot was not generated. Check the entered parameters.")
  }
}

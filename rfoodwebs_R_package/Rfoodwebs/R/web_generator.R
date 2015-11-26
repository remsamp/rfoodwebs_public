#' calculates the mean of a vector while excluding all NAs from it (roxygen comment)
#' @param spe_list is a string vector containing a list of species
#' @param f_web is a data frame containing predator/prey relationships

web_generator <- function(spe_list, f_web){

# Define lists of predators and prey that are used to provide the existence of predator-prey interactions
  pred <- paste(f_web$Species.predator)
  prey <- paste(f_web$Prey.species.all)

# Initialise 'a', 'ind', 'predation.list' so that they can be used in the for loops below.  
  a <- 1
  ind <- 1:length(pred)
  predation_list <- matrix(0,1,2)
for(i in 1:length(spe_list)){
      for(j in 1:length(spe_list)){
          if(length(ind[pred == spe_list[i] & prey == spe_list[j]]) == 1){
              predation_list[a,1] <- spe_list[i]
              predation_list[a,2] <- spe_list[j]
              pred_list <- matrix(0,a+1,2)
              pred_list[1:a,] <- predation_list
              predation_list <- pred_list
              a <- a+1
              } # end of if
                if(i == length(spe_list) & j == length(spe_list)){
                    predation_list <- predation_list[1:a-1,]
                } # end of if
             
      } # end of for j
          
} # end of for i
return(predation_list)
} 

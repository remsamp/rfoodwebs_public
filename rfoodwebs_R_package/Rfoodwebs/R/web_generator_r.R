#' Remi's version of web_generator
#'
#' Like web_generator it creates a table of predator and corresponding prey. 
#' It it faster but it gives a different result and may not make any sense! To discuss with the group
#' @param spe_list is a string vector containing a list of species
#' @param f_web is a data frame containing predator/prey relationships

web_generator_r <- function(spe_list, f_web){

  # convert predator and prey columns in f_web from factor to character
  f_web$Species.predator <- as.character(f_web$Species.predator)
  f_web$Prey.species.all <- as.character(f_web$Prey.species.all)
  
  # subset the f_web database to only contain the species we are interested in
  idx <- match(f_web$Species.predator, spe_list)
  idx <- !is.na(idx)
  idx1 <- match(f_web$Prey.species.all, spe_list)
  idx1 <- !is.na(idx1)
  subf_web <- f_web[idx & idx1 , ]
  
  # find what prey each of the predator eats
  myres <- table(subf_web$Species.predator, subf_web$Prey.species.all)
  myres[myres > 0] <- 1
  
  # format the output
  mypred <- rownames(myres)
  myprey <- colnames(myres)
  mypred <- unlist(sapply(c(1:nrow(myres)), function(i)rep(mypred[i], each = sum(myres[i, ]))))
  myprey <- unlist(sapply(c(1:nrow(myres)), function(i) myprey[myres[i, ]>0] ))

 return(data.frame(mypred, myprey))
} 

#' Creates a predator/prey table
#'
#' Creates a table in which each row contains a predator and one
#' of its prey. Each predator has as many entries as it has different prey.
#' @param spe_list is a string vector containing a list of species
#' @param f_web is a data frame containing predator/prey relationships

web_generator <- function(spe_list, f_web){
  # convert predator and prey columns in f_web from factor to character
  f_web$Species.predator <- as.character(f_web$Species.predator)
  f_web$Prey.species.all <- as.character(f_web$Prey.species.all)
  
  # subset the f_web database to only contain the species we are interested in
  idx <- match(f_web$Species.predator, spe_list[, 1])
  idx <- !is.na(idx)
  idx1 <- match(f_web$Prey.species.all, spe_list[, 1])
  idx1 <- !is.na(idx1)
  subf_web <- f_web[idx & idx1 , ]
  
  # find what prey each of the predator eats
  myres <- table(subf_web$Species.predator, subf_web$Prey.species.all)
  myres[myres > 0] <- 1
  
  # format the output: this is where the problem is
  mypred <- rownames(myres)
  myprey <- colnames(myres)
  mypred <- unlist(sapply(c(1:nrow(myres)), function(i)rep(mypred[i], each = sum(myres[i, ]))))
  myprey <- unlist(sapply(c(1:nrow(myres)), function(i) myprey[myres[i, ]>0] ))
  
  return(data.frame(Predator = mypred, Prey = myprey))
} 

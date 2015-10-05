#' calculates the mean of a vector while excluding all NAs from it (roxygen comment)
#' @param x a numeric vector

# this is where we will write the R code generating food webs and food webs metrics
mean.r <- function(x){
  mean(x, na.rm = T)
}
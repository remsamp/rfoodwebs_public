createcheddarobj <- function(Scale){
  S <- Scale
  Subset.N <- subset(cbess_N, cbess_N$Scale == S,11:ncol(cbess_N))
  N.sums <- colSums(Subset.N)
  sp.names <- names(Subset.N)
  # sp.present <- sp.names[N.sums > 0]
  
  #----------------------------------------------------
  # Calculating abundances and body masses of all species present
  # at each scale and express these on a per metre squared basis.
  
  Subset.M <- subset(cbess_M, cbess_M$Scale == S,11:ncol(cbess_M))
  M.sums <- colSums(Subset.M)    
  Body.Mass <- M.sums
  Body.Mass <- Body.Mass[N.sums > 0]

  # Abundance expressed on a per metre squared basis using cores here, change when using CBESS
  # updated data which is on square meter basis already to
  N.sums <- ((100*100)/(pi*(5^2)*3))*N.sums
  Abundance <- N.sums[N.sums > 0]
  
  # Format data to feed into output object for 'Cheddar'
  
  Sp.id <- paste(fctgroup$species.id)# need species list here
  # Need to create a subseted version of ee that can then be used to get Category and Functional.group
      
   sp <- sp.names[N.sums > 0]
   subind <- matrix(0, 1, length(sp))
   subind[1, ] <- match(sp, Sp.id)
      
   subset_fctgroup <- fctgroup[subind,]
      cheddar_obj <- data.frame(
                                node = sp,
                                N = paste("Abundance.", S, sep = ""),
                                category = subset_fctgroup$category,
                                functional_group = subset_fctgroup$functional.group,
                                M = paste("Body.Mass.", S, sep = ""))
      # Now need to save the current cheddar object for the appropriate spatial scale
        # eval(parse(text = paste("cheddar_obj.", S, "<- cheddar_obj", sep = "")))
        cheddar_obj
}
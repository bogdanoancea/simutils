#' @title Transform an object and its elements in a list of lists keeping 
#' the attributes' structure. 
#' 
#' @description \code{list_deepest} receives an object and transforms it 
#' and all its elements in a list of lists keeping the original attributes' 
#' structure.
#' 
#' @param object A \code{list} or another object with an attributes' structure.
#' 
#' @details Return a list of lists using the attributes' structure from the
#' original object. 
#' 
#' @examples 
#' 
#' @export

list_deepest <- function(object){
  
  if (!is.list(object)) return(list(object))
  original_attr <- attributes(object)
  output <- lapply(object, function(comp){
    
    localOutput <- list_deepest(comp)
    attributes(localOutput) <- attributes(localOutput) 
    return(localOutput)
    
  })
  attributes(output) <- original_attr
  return(output)

}
